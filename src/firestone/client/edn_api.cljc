(ns firestone.client.edn-api
  (:require [firestone.construct :refer [create-game]]
            [firestone.core-api :refer [end-turn
                                        play-minion-card
                                        play-spell-card]]
            [firestone.client.mapper :refer [get-client-state]]))

(defonce state-atom (atom nil))

(defn create-game!
  []
  (get-client-state (reset! state-atom (create-game [{:deck    ["Insect Swarm", "Mio", "Jonatan"]
                                                      :hand    ["Emil", "Kato", "Alfred"]
                                                      :mana    10
                                                      :hero "Carl"}
                                                     {:deck    ["Emil", "Alfred", "Jonatan"]
                                                      :hand    ["Ronja", "Kato", "Mio"]
                                                      :mana    10
                                                      :hero "Gustaf"}]))))

(defn end-turn!
  [player-id]
  (get-client-state (swap! state-atom end-turn player-id)))

(defn play-minion-card!
  [player-id card-id position target-id]
  (if-not target-id
    (get-client-state (swap! state-atom play-minion-card player-id card-id position))
    (get-client-state (swap! state-atom play-minion-card player-id card-id position target-id))))

(defn play-spell-card!
  [player-id card-id target-id]
  (if-not target-id
    (get-client-state (swap! state-atom play-spell-card player-id card-id))
    (get-client-state (swap! state-atom play-spell-card player-id card-id target-id))))