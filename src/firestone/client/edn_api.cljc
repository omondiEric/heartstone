(ns firestone.client.edn-api
  (:require [firestone.construct :refer [create-game
                                         create-secret]]
            [firestone.core-api :refer [attack-hero-or-minion
                                        do-hero-power
                                        end-turn
                                        play-minion-card
                                        play-spell-card]]
            [firestone.client.mapper :refer [get-client-state]]))

(defonce state-atom (atom {:state-history (list)
                           :undo-states   (list)}))

(defn get-current-state
  []
  (first (:state-history (deref state-atom))))


(defn create-game!
  []
  (get-client-state (first (:state-history (swap! state-atom update :state-history conj
                                                  (create-game [{:deck []
                                                                 :hand []
                                                                 :minions ["Jonatan"
                                                                           "Jonatan"
                                                                           "Hadronox"]
                                                                 :mana 10
                                                                 :hero "Carl"}
                                                                {:deck ["Mio"
                                                                        "Mio"
                                                                        "Mio"]
                                                                 :hand []
                                                                 :mana 10
                                                                 :hero "Gustaf"
                                                                 :minions ["The Mistcaller"
                                                                           "The Mistcaller"
                                                                           "The Mistcaller"
                                                                           "The Mistcaller"]}]))))))

(defn end-turn!
  [player-id]
  (let [previous-state (get-current-state)]
    (swap! state-atom update :state-history conj (end-turn previous-state player-id))
    (get-client-state (first (:state-history (deref state-atom))))))

(defn play-minion-card!
  [player-id card-id position target-id]
  (let [previous-state (get-current-state)]
    (if-not target-id
      (swap! state-atom update :state-history conj
             (play-minion-card previous-state player-id card-id position))
      (swap! state-atom update :state-history conj
             (play-minion-card previous-state player-id card-id position target-id)))
    (get-client-state (get-current-state))))

(defn play-spell-card!
  [player-id card-id target-id]
  (let [previous-state (get-current-state)]
    (if-not target-id
      (swap! state-atom update :state-history conj
             (play-spell-card previous-state player-id card-id))
      (swap! state-atom update :state-history conj
             (play-spell-card previous-state player-id card-id target-id)))
    (get-client-state (first (:state-history (deref state-atom))))))

(defn use-hero-power!
  [player-id target-id]
  (let [previous-state (get-current-state)]
    (if-not target-id
      (swap! state-atom update :state-history conj
             (do-hero-power previous-state player-id))
      (swap! state-atom update :state-history conj
             (do-hero-power previous-state player-id :target-id target-id)))
    (get-client-state (first (:state-history (deref state-atom))))))

(defn attack-with-minion!
  [player-id minion-id target-id]
  (let [previous-state (get-current-state)]
    (swap! state-atom update :state-history conj
           (attack-hero-or-minion previous-state player-id minion-id target-id))
    (get-client-state (first (:state-history (deref state-atom))))))

(defn undo!
  [_]
  (let [undo-state (peek (:state-history (deref state-atom)))]
    ; remove state from :state-history
    (swap! state-atom update :state-history pop)
    ; add to :undo-states
    (swap! state-atom update :undo-states conj undo-state)
    (get-client-state (first (:state-history (deref state-atom))))))

(defn redo!
  [_]
  (let [redo-state (peek (:undo-states (deref state-atom)))]
    ; remove state from :undo-states
    (swap! state-atom update :undo-states pop)
    ; add to state-history
    (swap! state-atom update :state-history conj redo-state)
    (get-client-state (first (:state-history (deref state-atom))))))