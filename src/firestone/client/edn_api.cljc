(ns firestone.client.edn-api
  (:require [firestone.construct :refer [create-game]]
            [firestone.core-api :refer [end-turn]]
            [firestone.client.mapper :refer [get-client-state]]))

(defonce state-atom (atom nil))

(defn create-game!
  []
  (get-client-state (reset! state-atom (create-game))))

(defn end-turn!
  [player-id]
  (get-client-state (swap! state-atom end-turn player-id)))