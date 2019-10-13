(ns firestone.definition.card
  (:require [firestone.definitions :as definitions]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [create-game
                                         create-card
                                         create-minion
                                         deal-damage
                                         deal-damage-to-other-minions
                                         deal-damage-to-all-minions
                                         deal-damage-to-all-heroes
                                         replace-minion
                                         get-minion
                                         get-random-minion
                                         get-other-player-id
                                         give-taunt
                                         switch-minion-side
                                         update-minion
                                         update-seed]]
            [firestone.core-api :refer [draw-card]]))

(def card-definitions
  {

   "Mio"
   {:name       "Mio"
    :attack     1
    :health     2
    :mana-cost  1
    :properties #{}
    :type       :minion}

   "Ronja"
   {:name       "Ronja"
    :attack     3
    :health     2
    :mana-cost  2
    :properties #{}
    :type       :minion}

   "Kato"
   {:name        "Kato"
    :attack      4
    :health      1
    :mana-cost   4
    :type        :minion
    :properties  #{}
    :description "Battlecry: Deal 4 damage to the enemy hero."
    :battlecry   (fn [state player-id]
                   (let [target-player-id (get-other-player-id player-id)
                         new-damage-taken (+ (get-in state [:players target-player-id :hero :damage-taken]) 4)]
                     (assoc-in state [:players target-player-id :hero :damage-taken] new-damage-taken)))}

   "Emil"
   {:name        "Emil"
    :attack      2
    :health      5
    :mana-cost   4
    :type        :minion
    :properties  #{}
    :description "Battlecry: Draw a card."
    :battlecry   (fn [state player-id]
                   (draw-card state player-id))}

   "Jonatan"
   {:name        "Jonatan"
    :attack      3
    :health      6
    :mana-cost   4
    :type        :minion
    :properties  #{"Taunt"}
    :set         :custom
    :description "Taunt."}

   "Alfred"
   {:name        "Alfred"
    :attack      4
    :health      5
    :mana-cost   2
    :type        :minion
    :properties  #{"NoAttack"}
    :set         :custom
    :description "Can't Attack."}

   "Uncle Melker"
   {:name        "Uncle Melker"
    :attack      3
    :health      5
    :mana-cost   5
    :type        :minion
    :properties  #{"Divine Shield"}
    :set         :custom
    :description "Divine Shield."}

   "Pippi"
   {:name        "Pippi"
    :attack      2
    :health      4
    :mana-cost   3
    :type        :minion
    :properties  #{}
    :end-of-turn (fn [state id]
                   (deal-damage-to-other-minions state id 1))
    :set         :custom
    :description "At the end of your turn deal 1 damage to all other minions."}

   "Karlsson"
   {:name        "Karlsson"
    :attack      1
    :health      4
    :mana-cost   3
    :type        :minion
    :properties  #{}
    :end-of-turn (fn [state id]
                   give-taunt state (:id (get-random-minion state)))
    :set         :custom
    :description "At the end of your turn give a random minion taunt."}

   "Uncle Nilsson"
   {:name        "Uncle Nilsson"
    :attack      5
    :health      5
    :mana-cost   6
    :type        :minion
    :properties  #{}
    :set         :custom
    :description "Deathrattle: Take control of a random enemy minion."
    :deathrattle (fn [state minion-id]
                   (let [random-result (get-random-minion state (get-other-player-id (:owner-id (get-minion state minion-id))))]
                     (-> state
                         (switch-minion-side (:id (last random-result)))
                         (update-seed (first random-result)))))}

   "Elisabeth"
   {:name        "Elisabeth"
    :attack      1
    :health      1
    :mana-cost   1
    :type        :minion
    :properties  #{"Taunt", "Divine Shield"}
    :set         :custom
    :description "Taunt. Divine Shield."}

   "Madicken"
   {:name        "Madicken"
    :attack      1
    :health      2
    :mana-cost   2
    :type        :minion
    :properties  #{}
    
    :set         :custom
    :description "Deathrattle: Summon Elisabeth."
    :deathrattle (fn [state minion-id]
                   (replace-minion state (create-minion "Elisabeth" :id minion-id)))}

   "Ida"
   {:name        "Ida"
    :attack      2
    :health      4
    :mana-cost   3
    :type        :minion
    :properties  #{}
    :custom-timing (fn [state id]
                     (give-taunt state id))
    :set         :custom
    :description "Whenever a minion takes damage, gain taunt."}

   "Insect Swarm"

   {:name         "Insect Swarm"
    :mana-cost    2
    :type         :spell
    :set          :custom
    :spell-fn     (fn [state]
                    (-> (deal-damage-to-all-heroes state 2)
                    (deal-damage-to-all-minions 2)))
    :description  "Deal 2 damage to all characters."}

   "Radar Raid"
   {:name         "Radar Raid"
    :mana-cost    2
    :type         :spell
    :set          :custom
    :spell-fn     (fn [state character-id]
                    (deal-damage state character-id 3))
    :description   "Deal 3 damage to a character."}

   })


(definitions/add-definitions! card-definitions)


