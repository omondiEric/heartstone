(ns firestone.definition.hero-power
  (:require [firestone.definitions :as definitions]
            [firestone.construct :refer [give-divine-shield
                                         deal-damage
                                         remove-minion
                                         give-attack
                                         get-random-minion]]))

(def hero-definitions
  {

   "Blessing"
   {:name        "Blessing"
    :type        :hero-power
    :mana-cost   2
    :power-fn (fn [state minion-id]
             (give-divine-shield state minion-id))}
    :description "Give a minion Divine Shield."


   "Strengthen"
   {:name        "Strengthen"
    :type        :hero-power
    :mana-cost   3
    :power-fn (fn [state player-id]
                (let [minion-1-id (:id (get-random-minion state player-id))
                      alternate-state (remove-minion state minion-1-id)
                      minion-2-id (:id (get-random-minion alternate-state player-id))]
                  (deal-damage state minion-1-id 1)
                  (deal-damage state minion-2-id 1)
                  (give-attack state minion-1-id 2)
                  (give-attack state minion-2-id 2)))
    :description "Deal 1 damage to two random friendly minions and give them +2 Attack."}

   })

(definitions/add-definitions! hero-definitions)