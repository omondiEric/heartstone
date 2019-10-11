(ns firestone.definition.hero-power
  (:require [firestone.definitions :as definitions]))

(def hero-definitions
  {

   "Blessing"
   {:name        "Blessing"
    :type        :hero-power
    :mana-cost   2
    :description "Give a minion Divine Shield."}

   "Strengthen"
   {:name        "Strengthen"
    :type        :hero-power
    :mana-cost   3
    :description "Deal 1 damage to two random friendly minions and give them +2 Attack."}

   })

(definitions/add-definitions! hero-definitions)