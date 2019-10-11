(ns firestone.definition.card
  (:require [firestone.definitions :as definitions]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [create-game
                                         create-card
                                         get-minion
                                         update-minion
                                         deal-damage
                                         deal-damage-to-other-minions
                                         deal-damage-to-all-characters
                                         get-random-minion
                                         give-taunt]]
            [firestone.core-api :refer [do-battlecry-fn]]))

(def card-definitions
  {

   "Mio"
   {:name       "Mio"
    :attack     1
    :health     2
    :mana-cost  1
    :properties #{}
    :end-of-turn false
    :type       :minion}

   "Ronja"
   {:name       "Ronja"
    :attack     3
    :health     2
    :mana-cost  2
    :properties #{}
    :end-of-turn false
    :type       :minion}

   "Kato"
   {:name        "Kato"
    :attack      4
    :health      1
    :mana-cost   4
    :type        :minion
    :properties  #{}
    :end-of-turn false
    :description "Battlecry: Deal 4 damage to the enemy hero."
    :battlecry   (fn [state target-player-id]
                   (let [new-damage-taken (+ (get-in state [:players target-player-id :hero :damage-taken]) 4)]
                     (assoc-in state [:players target-player-id :hero :damage-taken] new-damage-taken)))}

   "Emil"
   {:name        "Emil"
    :attack      2
    :health      5
    :mana-cost   4
    :type        :minion
    :properties  #{}
    :end-of-turn false
    :description "Battlecry: Draw a card."
    :battlecry   "draw card"}

   "Jonatan"
   {:name        "Jonatan"
    :attack      3
    :health      6
    :mana-cost   4
    :type        :minion
    :properties  #{"Taunt"}
    :end-of-turn false
    :set         :custom
    :description "Taunt."}

   "Alfred"
   {:name        "Alfred"
    :attack      4
    :health      5
    :mana-cost   2
    :type        :minion
    :properties  #{"NoAttack"}
    :end-of-turn false
    :set         :custom
    :description "Can't Attack."}

   "Uncle Melker"
   {:name        "Uncle Melker"
    :attack      3
    :health      5
    :mana-cost   5
    :type        :minion
    :properties  #{"Divine Shield"}
    :end-of-turn false
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
                   deal-damage-to-other-minions state id 1)

    :set         :custom
    :description "At the end of your turn deal 1 damage to all other minions."}

   "Karlsson"
   {:name        "Karlsson"
    :attack      1
    :health      4
    :mana-cost   3
    :type        :minion
    :properties  #{}
    :end-of-turn (fn [state]
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
    :end-of-turn false
    :set         :custom
    :description "Deathrattle: Take control of a random enemy minion."}

   "Elisabeth"
   {:name        "Elisabeth"
    :attack      1
    :health      1
    :mana-cost   1
    :type        :minion
    :properties  #{"Taunt", "Divine Shield"}
    :end-of-turn false
    :set         :custom
    :description "Taunt. Divine Shield."}

   "Madicken"
   {:name        "Madicken"
    :attack      1
    :health      2
    :mana-cost   2
    :type        :minion
    :properties  #{}
    :end-of-turn false
    :set         :custom
    :description "Deathrattle: Summon Elisabeth."}

   "Ida"
   {:name        "Ida"
    :attack      2
    :health      4
    :mana-cost   3
    :type        :minion
    :properties  #{}
    :end-of-turn false
    :custom-timing (fn [state]
                     (update-in state [:players :minions :id]
                                (fn [id]
                                  (if (= (:name get-minion state id) "Ida")
                                    (give-taunt state id)
                                    state))))
    :set         :custom
    :description "Whenever a minion takes damage, gain taunt."}

   "Insect Swarm"
   {:name         "Insect Swarm"
    :mana-cost    2
    :type         :spell
    :set          :custom
    :spell-fn     (fn [state]
                    (deal-damage-to-all-characters state 2))
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

(let [battlecry (fn [state target-player-id] (update-in state [:players target-player-id :hero :damage-taken] (fn [damage-taken] (+ damage-taken 4))))]
   (->(create-game)
      (battlecry "p2")))

(definitions/add-definitions! card-definitions)