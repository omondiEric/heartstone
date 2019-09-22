(ns firestone.definition.card
  (:require [firestone.definitions :as definitions]))
;[firestone.construct :refer [update-damage]]))

(def card-definitions
  {

   "Mio"
   {:name      "Mio"
    :attack    1
    :health    2
    :mana-cost 1
    :type      :minion}

   "Ronja"
   {:name      "Ronja"
    :attack    3
    :health    2
    :mana-cost 2
    :type      :minion}

   "Kato"
   {:name        "Kato"
    :attack      4
    :health      1
    :mana-cost   4
    :type        :minion
    :description "Battlecry: Deal 4 damage to the enemy hero."
    :battlecry   (fn [x] (+ x 4))}

   "Emil"
   {:name        "Emil"
    :attack      2
    :health      5
    :mana-cost   4
    :type        :minion
    :description "Battlecry: Draw a card."
    :battlecry   "draw card"}

   })

(definitions/add-definitions! card-definitions)