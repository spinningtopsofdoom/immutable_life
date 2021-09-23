(ns life.multiverse.core
  (:require [datahike.api :as dh]
            [clojure.set :as cset]))

(defn board->next-board [board]
  (let [removed-cells (into #{} (take 3 board))
        added-cells (into #{} (map (fn cell [_] (mapv long ((juxt rand-int rand-int) 10))) (range 5)))]
    (cset/union (cset/difference board removed-cells) added-cells)))

(defn board->storage [board]
  (mapv (fn [[x y]] {:board/x x :board/y y :piece/hash (long (hash [x y]))}) board))

(defn storage->db
  "Puts the life board from storage into the database.

  board-storage: Map - Map of the board pieces in database format: x, y, and the hash
  name: String - Name of the game to update the board
  db: datahike.db.DB - Datahike database for the game of life"
  [board-storage name db]
  (let [hashes (set (map :piece/hash board-storage))
        removed-pieces (dh/q
                         '[:find [?pieces ...]
                           :in $ ?game [?hash ...]
                           :where
                           [?e :game/name ?game]
                           [?e :game/pieces ?pieces]
                           (not [?pieces :piece/hash ?hash])]
                         @db name hashes)
        piece-retractions (mapv
                            (fn id-to-retract [piece-id]
                              [:db/retract [:game/name name] :game/pieces piece-id])
                            removed-pieces)
        new-pieces {:game/name   name
                    :game/pieces board-storage}]
    (dh/transact
      db
      (conj (vec piece-retractions) new-pieces))))

(defn db-setup [config]
  (dh/delete-database config)
  (dh/create-database config)
  (dh/connect config))

(def cfg {:store {:backend :mem :id "game of life"}})

(def schema
  [{:db/ident       :game/name
    :db/valueType   :db.type/string
    :db/unique      :db.unique/identity
    :db/cardinality :db.cardinality/one}
   {:db/ident       :game/pieces
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/many}
   {:db/ident       :piece/hash
    :db/valueType   :db.type/long
    :db/unique      :db.unique/identity
    :db/cardinality :db.cardinality/one}
   {:db/ident       :board/x
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident       :board/y
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one}])

(defn starting-board
  "Creates a new game randomly populated game board with the number of cells specified

  cell-num: Long - The number of cells to add to a board. Default of 20

  (starting-board 5)
  ; => #{[8 6] [9 9] [1 3] [5 1] [2 6]} or
  ; => #{[5 3] [4 1] [5 2] [7 0] [6 0]}"
  ([] (starting-board 20))
  ([cell-num]
   (into #{} (map (fn cell [_] (mapv long ((juxt rand-int rand-int) 10))) (range cell-num)))))

(defn modify-board
  "creates a new game of life based off additions and removals of cells

  board: Set - Set of cells for the game of life
  add: Set - Set of cells to be added to the board.
  remove: Set - Set of cells to be removed from the board.

  Addition of cells overwrites any cell removal (i.e. cells get removed from the board then added)

  (modify-board #{[2 2] [0 0] [5 4] [1 7] [0 2]} #{[11 11] [5 8] [7 6]} #{[11 11] [2 2] [5 4]})
  ; => #{[7 6] [0 0] [11 11] [1 7] [5 8] [0 2]}
  Invariants
  running modify board twice with the same add and remove gives the same board
  the only cells in the modified board are in the original or add sets
  Add overrides remove"
  [board add remove]
  (cset/union (cset/difference board remove) add))

(defn branch-board
  "creates a new game of life based off of a current board at a specific time

  Invariants
  The branched board will be equal to the old board"
  [board-time game-name new-game-name]
  #{})

(defn branch-modified-board
  "creates a new game of life based off of a current board at a specific time

  Invariants
  The branched board will equal to the old board with modify-board ran on it"
  [board-time game-name add remove new-game-name]
  #{})

(defn merge-game-boards
  "Merges two game of life boards into a new board

  Invariants
  Cells in both boards will be in the merged board"
  [board-a board-b]
  board-a)

(defn board-diff
  "Gives the cells that were added and removed by a step in the game

  initial-board: Set - The starting board the step will be applied to
  changed-board: Set - The new cells for the board after the step was applied to the initial-board

  The added cells are new cells in the changed-board that are not present in the initial-board. The removed
  cells are cells that are not present in the changed-board but are in the initial-board. The unchanged cells
  are the cells present in both boards

  (board-diff #{[4 3] [8 4] [4 6] [5 7] [1 5]} #{[8 4] [8 6] [6 6] [5 7] [1 5]})
  ; => {:added #{[8 6] [6 6]}, :removed #{[4 3] [4 6]}, :unchanged #{[8 4] [5 7] [1 5]}}
  Invariants
  Diffing an empty board puts everything in added
  Diffing the same board puts everything in removed"
  [initial-board changed-board]
  {:added (cset/difference changed-board initial-board)
   :removed (cset/difference initial-board changed-board)
   :unchanged (cset/intersection initial-board changed-board)})

(defn normalize-board
  "Normalizes a game of life board

  board: Set - Set of points on a game of life board

  (normalize-board #{[8 8] [9 2] [4 5] [6 9] [3 2]})
  ; => #{[0 0] [1 3] [5 6] [3 7] [6 0]}"
  [board]
  (let [min-x (apply min (map first board))
        min-y (apply min (map second board))]
    (into #{} (map (fn normalize [[x y]] [(- x min-x) (- y min-y)]) board))))

(defn boards-equal
  "checks if two normalized boards are equal

  (boards-equal #{[8 8] [7 6] [7 1] [8 5] [6 9]} #{[2 9] [4 8] [3 6] [4 5] [3 1]})
  ; => true
  (boards-equal #{[8 7] [6 3]} #{[2 3] [4 7]})
  ; => true
  (boards-equal #{[8 7] [6 3]} #{[2 3] [4 5]})
  ; => false"
  [board-a board-b]
  (= (normalize-board board-a) (normalize-board board-b)))

(defn db->board
  "Gets a game of life board given a game name

  game-name: String - Unique name of the game of life
  db: datahike.db.DB - Datahike database for the game of life

  (storage->db (board->storage #{[7 1] [5 3] [9 9] [1 8]}) \"ns/game-name\" db)
  (db->board \"ns/game-name\" db)
  ; => #{[7 1] [5 3] [9 9] [1 8]}"
  [game-name db]
  (dh/q
    '[:find ?x ?y
      :in $ ?game
      :where
      [?e :game/name ?game]
      [?e :game/pieces ?pieces]
      [?pieces :board/x ?x]
      [?pieces :board/y ?y]]
    @db game-name))

(defn db-at-time->board
  "Gets a game of life board given a game name at a given time

  game-name: String - Unique name of the game of life
  game-time: Integer - Integer timestamp for each step in the game of life (0, 1, 2, 3, 4...)
  db: datahike.db.DB - Datahike database for the game of life

  (storage->db (board->storage #{[7 1] [5 3]}) \"ns/game-name\" db)
  (storage->db (board->storage #{[7 1] [5 3] [9 9] [1 8]}) \"ns/game-name\" db)
  (storage->db (board->storage #{[7 1] [9 9]}) \"ns/game-name\" db)

  (db->board \"ns/game-name\" 0 db)
  ; => #{[7 1] [5 3]}
  (db->board \"ns/game-name\" 2 db)
  ; => #{[7 1] [9 9]}
  (db->board \"ns/game-name\" 3 db)
  ; => nil

  If the game time is beyond the number of steps then nil is returned"
  [game-name game-time db]
  (let [game-datoms (dh/datoms @db {:index :avet :components [:game/name game-name]})
        tx-time-id (-> game-datoms (nth game-time nil) :tx)]
    (when tx-time-id
      (dh/q
        '[:find ?x ?y
          :in $ ?game ?t
          :where
          [?e :game/name ?game]
          [?e :game/pieces ?pieces ?t]
          [?pieces :board/x ?x]
          [?pieces :board/y ?y]]
        (dh/as-of @db tx-time-id) game-name tx-time-id))))

(defn game-history
  "Gives a history sequence of a particular game of life played out

  game-name: String - Unique name of the game of life
  db: datahike.db.DB - Datahike database for the game of life

  (storage->db (board->storage #{[7 1] [5 3]}) \"ns/game-name\" db)
  (storage->db (board->storage #{[7 1] [5 3] [9 9] [1 8]}) \"ns/game-name\" db)
  (storage->db (board->storage #{[7 1] [9 9]}) \"ns/game-name\" db)

  (game-history \"ns/game-name\" db)
  ; => (#{[7 1] [5 3]} #{[7 1] [5 3] [9 9] [1 8]} #{[7 1] [9 9]})"
  [game-name db]
  (let [game-datoms (dh/datoms @db {:index :avet :components [:game/name game-name]})
        tx-id-seq (map :tx game-datoms)]
    (map
      (fn life-at-time [life-time]
        (dh/q
          '[:find ?x ?y
            :in $ ?t
            :where
            [_ :game/pieces ?pieces ?t]
            [?pieces :board/x ?x]
            [?pieces :board/y ?y]]
          (dh/as-of @db life-time) life-time))
      tx-id-seq)))

(comment
  (def init-board #{})

  (let [db-conn (db-setup cfg)
        game-name "user/foo"
        boards (take 3 (drop 1 (iterate board->next-board init-board)))]
    (dh/transact db-conn schema)
    (doseq [board boards]
      (storage->db (board->storage board) game-name db-conn))
    (dh/q
      '[:find ?x ?y
        :in $ ?game
        :where
        [?e :game/name ?game]
        [?e :game/pieces ?pieces]
        [?pieces :board/x ?x]
        [?pieces :board/y ?y]]
      @db-conn "user/foo")))
