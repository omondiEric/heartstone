(ns firestone.definition.card
  (:require [firestone.definitions :as definitions]))
;[firestone.construct :refer [update-damage]]))

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
    :battlecry   (fn [x] (+ x 4))}

   "Emil"
   {:name        "Emil"
    :attack      2
    :health      5
    :mana-cost   4
    :type        :minion
    :properties  #{}
    :description "Battlecry: Draw a card."
    :battlecry   "draw card"}

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
    :properties  #{}
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
    :set         :custom
    :description "At the end of your turn deal 1 damage to all other minions."}

   "Karlsson"
   {:name        "Karlsson"
    :attack      1
    :health      4
    :mana-cost   3
    :type        :minion
    :properties  #{}
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
    :description "Deathrattle: Take control of a random enemy minion."}

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
    :description "Deathrattle: Summon Elisabeth."}

   "Ida"
   {:name        "Ida"
    :attack      2
    :health      4
    :mana-cost   3
    :type        :minion
    :properties  #{}
    :set         :custom
    :description "Whenever a minion takes damage, gain taunt."}

   "Insect Swarm"
   {:name         "Insect Swarm"
    :mana-cost    2
    :type         :spell
    :set          :custom
    :description  "Deal 2 damage to all characters."}

   "Radar Raid"
   {:name         "Radar Raid"
    :mana-cost    2
    :type         :spell
    :set          :custom
    :description   "Deal 3 damage to a character."}

   })

(definitions/add-definitions! card-definitions)