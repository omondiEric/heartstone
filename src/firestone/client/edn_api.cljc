(ns firestone.client.edn-api
  (:require [firestone.construct :refer [create-game
                                         create-secret]]
            [firestone.core-api :refer [attack-hero-or-minion
                                        do-hero-power
                                        end-turn
                                        play-minion-card
                                        play-spell-card]]
            [firestone.client.mapper :refer [get-client-state]]))

(defonce state-atom (atom nil))

(defn create-game!
  []
  (get-client-state (reset! state-atom (create-game [{:deck    ["Insect Swarm", "Madicken", "Jonatan"]
                                                      :hand    ["Emil", "Al'Akir the Windlord", "Annika", "Explosive Trap", "Stormwind Knight"]
                                                      :mana    10
                                                      :hero "Carl"}
                                                     {:deck    ["Pippi", "Uncle Nilsson", "Alfred", "Vaporize"]
                                                      :hand    ["Vaporize", "Kato", "Secretkeeper", "Kezan Mystic", "Leeroy Jenkins"]
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

(defn use-hero-power!
  ([player-id target-id]
   (get-client-state (swap! state-atom do-hero-power player-id :target-id target-id)))
  ([player-id]
   (get-client-state (swap! state-atom do-hero-power player-id ))))

(defn attack-with-minion!
  [player-id minion-id target-id]
  (get-client-state (swap! state-atom attack-hero-or-minion player-id minion-id target-id)))