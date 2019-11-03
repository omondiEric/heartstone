(ns firestone.client.edn-api
  (:require [firestone.construct :refer [create-game]]
            [firestone.core-api :refer [end-turn
                                        play-minion-card]]
            [firestone.client.mapper :refer [get-client-state]]))

(defonce state-atom (atom nil))

(defn create-game!
  []
  (get-client-state (reset! state-atom (create-game [{:deck    ["Ronja", "Mio", "Jonathan"]
                                                      :hand    ["Emil", "Kato", "Alfred"]
                                                      :mana    3}
                                                     {:hero "Carl"}]
                                                    :player-id-in-turn "p1"))))

(defn end-turn!
  [player-id]
  (get-client-state (swap! state-atom end-turn player-id)))

(defn play-minion-card!
  [player-id card-id target-id position]
  (get-client-state (swap! state-atom (play-minion-card state-atom player-id card-id position target-id))))