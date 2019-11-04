(ns firestone.client.edn-api
  (:require [firestone.construct :refer [create-game]]
            [firestone.core-api :refer [end-turn
                                        play-minion-card
                                        do-hero-power]]
            [firestone.client.mapper :refer [get-client-state]]))

(defonce state-atom (atom nil))

(defn create-game!
  []
  (get-client-state (reset! state-atom (create-game [{:deck    ["Radar Raid", "Mio", "Jonatan"]
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

(defn use-hero-power!
  ([player-id target-id]
  (get-client-state (swap! state-atom do-hero-power player-id :target-id target-id)))
  ([player-id]
    (get-client-state (swap! state-atom do-hero-power player-id ))))
