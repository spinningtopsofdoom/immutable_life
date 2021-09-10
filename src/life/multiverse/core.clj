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
