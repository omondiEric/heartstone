(ns firestone.definition.hero-power
  (:require [firestone.definitions :as definitions]
            [firestone.construct :refer [get-minions
                                         get-random-minion
                                         get-random-minions-distinct
                                         give-divine-shield
                                         minion?
                                         remove-minion
                                         modify-minion-attack]]
            [firestone.core :refer [deal-damage]]))

(def hero-definitions
  {

   "Blessing"
   {:name        "Blessing"
    :type        :hero-power
    :mana-cost   2
    :valid-target? (fn [state character]
                     (minion? character))
    :power-fn    (fn [state target-id]
                   (give-divine-shield state target-id))
    :description "Give a minion Divine Shield."}

   "Strengthen"
   {:name        "Strengthen"
    :type        :hero-power
    :mana-cost   3
    :power-fn    (fn [state player-id]
                   (let [minion-collection (map :id (take 2 (get-random-minions-distinct state 2 player-id)))
                         give-2-attack (fn [state minion] (modify-minion-attack state minion 2))]
                     (as-> state $
                           (reduce give-2-attack $ minion-collection)
                           (reduce deal-damage $ minion-collection))))
    :description "Deal 1 damage to two random friendly minions and give them +2 Attack."}

   })

(definitions/add-definitions! hero-definitions)