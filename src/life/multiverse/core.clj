(ns life.multiverse.core
  (:require [datahike.api :as dh]
            [datahike.core :as d]))

(defn board->next-board [board]
  (let [removed-cells (into #{} (take 3 board))
        added-cells (into #{} (map (fn cell [_] (map long ((juxt rand-int rand-int) 10))) (range 5)))]
    (clojure.set/union (clojure.set/difference board removed-cells) added-cells)))

(defn board->storage [board]
  (mapv (fn [[x y]] {:board/x x :board/y y}) board))

(defn storage->db [board-storage name db]
  (dh/transact
    db
    [{:game/name   name
      :game/pieces board-storage}]))

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
   {:db/ident       :board/x
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident       :board/y
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one}])

(defn starting-board
  "Creates a new game populated game board"
  []
  #{})

(defn modify-board
  "creates a new game of life based off additions and removals of cells

  Invariants
  running modify board twice with the same add and remove gives the same board
  the only cells in the modified board are in the original or add sets
  Add overrides remove"
  [board add remove]
  board)

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
  "Gives the cells that were added and removed by two boards

  Invariants
  Diffing an empty board puts everything in added
  Diffing the same board puts everything in removed"
  [initial-board changed-board]
  {:added #{} :removed #{}})

(defn boards-equal
  "checks if two boards are equal"
  [board-a board-b]
  false)

(defn db->board
  "Gets a game of life board given a game name"
  [game-name db]
  #{})

(defn db-at-time->board
  "Gets a game of life board given a game name at a given time"
  [game-name game-time db]
  #{})

(defn game-history
  "Gives the history of a particular game of life played out"
  [game-name db]
  [])

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
