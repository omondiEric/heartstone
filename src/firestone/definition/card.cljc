(ns firestone.definition.card
  (:require [firestone.definitions :as definitions]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [create-game
                                         create-card
                                         create-minion
                                         friendly-minions?
                                         get-minion
                                         get-random-minion
                                         get-other-player-id
                                         give-deathrattle
                                         give-taunt
                                         minion?
                                         modify-minion-stats
                                         replace-minion
                                         ida-present?
                                         switch-minion-side
                                         update-minion
                                         update-seed]]
            [firestone.core :refer [deal-damage
                                    deal-damage-to-other-minions
                                    deal-damage-to-all-minions
                                    deal-damage-to-all-heroes]]
            [firestone.core-api :refer [draw-card]]))

(def card-definitions
  {

   "Mio"
   {:name       "Mio"
    :attack     1
    :health     2
    :mana-cost  1
    :properties {:permanent     #{}
                 :temporary     {}
                 :stats         {}}
    :type       :minion}

   "Ronja"
   {:name       "Ronja"
    :attack     3
    :health     2
    :mana-cost  2
    :properties {:permanent     #{}
                 :temporary     {}
                 :stats         {}}
    :type       :minion}

   "Kato"
   {:name        "Kato"
    :attack      4
    :health      1
    :mana-cost   4
    :type        :minion
    :properties  {:permanent     #{}
                  :temporary     {}
                  :stats         {}}
    :description "Battlecry: Deal 4 damage to the enemy hero."
    :on-play   (fn [state player-id minion-id]
                   (let [target-hero-id (get-in state [:players (get-other-player-id player-id) :hero :id])]
                     (deal-damage state target-hero-id 4)))}

   "Emil"
   {:name        "Emil"
    :attack      2
    :health      5
    :mana-cost   4
    :type        :minion
    :properties  {:permanent     #{}
                  :temporary     {}
                  :stats         {}}
    :description "Battlecry: Draw a card."
    :on-play   (fn [state player-id minion-id]
                   (draw-card state player-id))}

   "Jonatan"
   {:name        "Jonatan"
    :attack      3
    :health      6
    :mana-cost   4
    :type        :minion
    :properties  {:permanent     #{"taunt"}
                  :temporary     {}
                  :stats         {}}
    :set         :custom
    :description "taunt."}

   "Alfred"
   {:name        "Alfred"
    :attack      4
    :health      5
    :mana-cost   2
    :type        :minion
    :properties  {:permanent     #{"NoAttack"}
                  :temporary     {}
                  :stats         {}}
    :set         :custom
    :description "Can't Attack."}

   "Uncle Melker"
   {:name        "Uncle Melker"
    :attack      3
    :health      5
    :mana-cost   5
    :type        :minion
    :properties  {:permanent     #{"divine-shield"}
                  :temporary     {}
                  :stats         {}}
    :set         :custom
    :description "Divine Shield."}

   "Pippi"
   {:name        "Pippi"
    :attack      2
    :health      4
    :mana-cost   3
    :type        :minion
    :properties  {:permanent     #{}
                  :temporary     {}
                  :stats         {}}
    :end-of-turn (fn [state minion-id]
                   (deal-damage-to-other-minions state minion-id 1))
    :set         :custom
    :description "At the end of your turn deal 1 damage to all other minions."}

   "Karlsson"
   {:name        "Karlsson"
    :attack      1
    :health      4
    :mana-cost   3
    :type        :minion
    :properties  {:permanent     #{}
                  :temporary     {}
                  :stats         {}}
    :end-of-turn (fn [state id]
                   (let [random-result (get-random-minion state)]
                     (let [state (first random-result)
                           random-minion (last random-result)]
                       (give-taunt state (:id random-minion)))))
    :set         :custom
    :description "At the end of your turn give a random minion taunt."}

   "Uncle Nilsson"
   {:name        "Uncle Nilsson"
    :attack      5
    :health      5
    :mana-cost   6
    :type        :minion
    :properties  {:permanent     #{"deathrattle"}
                  :temporary     {}
                  :stats         {}}
    :set         :custom
    :description "deathrattle: Take control of a random enemy minion."
    :deathrattle (fn [state minion-id]
                   (let [random-result (get-random-minion state (get-other-player-id (:owner-id (get-minion state minion-id))))]
                     (switch-minion-side state (:id (last random-result)))))}

   "Elisabeth"
   {:name        "Elisabeth"
    :attack      1
    :health      1
    :mana-cost   1
    :type        :minion
    :properties  {:permanent     #{"taunt", "divine-shield"}
                  :temporary     {}
                  :stats         {}}
    :set         :custom
    :description "taunt. Divine Shield."}

   "Madicken"
   {:name        "Madicken"
    :attack      1
    :health      2
    :mana-cost   2
    :type        :minion
    :properties  {:permanent     #{"deathrattle"}
                  :temporary     {}
                  :stats         {}}
    :set         :custom
    :description "deathrattle: Summon Elisabeth."
    :deathrattle (fn [state minion-id]
                   (replace-minion state (create-minion "Elisabeth" :id minion-id)))}

   "Ida"
   {:name          "Ida"
    :attack        2
    :health        4
    :mana-cost     3
    :type          :minion
    :properties    {:permanent     #{}
                    :temporary     {}
                    :stats         {}}
    :on-minion-damage (fn [state id]
                        (let [ida (ida-present? state)]
                          (if (nil? ida)
                            state
                            (give-taunt state (:id ida)))))
    :set              :custom
    :description      "Whenever a minion takes damage, gain taunt."}

   "Insect Swarm"
   {:name        "Insect Swarm"
    :mana-cost   2
    :type        :spell
    :set         :custom
    :spell-fn    (fn [state]
                   (-> (deal-damage-to-all-heroes state 2)
                       (deal-damage-to-all-minions 2)))
    :description "Deal 2 damage to all characters."}

   "Radar Raid"
   {:name        "Radar Raid"
    :mana-cost   2
    :type        :spell
    :set         :custom
    :spell-fn    (fn [state character-id]
                   (deal-damage state character-id 3))
    :description "Deal 3 damage to a character."}

   "Herr Nilsson"
   {:name        "Herr Nilsson"
    :attack      1
    :health      3
    :mana-cost   3
    :type        :minion
    :properties  {:permanent     #{"Poisonous"}
                  :temporary     {}
                 :stats         {}}
    :set         :custom
    :description "Poisonous"}

   "Rasmus"
   {:name        "Rasmus"
    :attack      3
    :health      5
    :mana-cost   5
    :type        :minion
    :properties  {:permanent     #{"windfury"}
                  :temporary     {}
                  :stats         {}}
    :set         :custom
    :description "windfury"}

   "Tjorven"
   {:name        "Tjorven"
    :attack      0
    :health      2
    :mana-cost   5
    :type        :minion
    :properties  {:permanent     #{}
                  :temporary     {}
                  :stats         {}}
    :set         :custom
    :description "Your other minions has windfury."
    :aura        #{"Friendly-windfury"}}

   "Astrid"
   {:name        "Astrid"
    :attack      3
    :health      3
    :mana-cost   4
    :type        :minion
    :properties  {:permanent     #{}
                  :temporary     {}
                  :stats         {}}
    :set         :custom
    :description "Battlecry: Copy another minions deathrattle."
    :on-play      (fn [state player-id minion-id target-id] (give-deathrattle state minion-id (:name (get-minion state target-id))))}

   "Skrallan"
   {:name        "Skrallan"
    :attack      2
    :health      2
    :mana-cost   3
    :type        :minion
    :properties  {:permanent     #{}
                  :temporary     {}
                  :stats         {}}
    :set         :custom
    :description "After a friendly minion loses Divine Shield, gain +2/+2."
    :on-divine-shield-removal (fn [state minion-id other-minion-id]
                                (if (friendly-minions? state minion-id other-minion-id)
                                  (modify-minion-stats state minion-id 2 2)
                                  state))}

   "Annika"
   {:name        "Annika"
    :attack      2
    :health      2
    :mana-cost   3
    :type        :minion
    :properties  {:permanent     #{}
                  :temporary     {}
                  :stats         {}}
    :set         :custom
    :description "Battlecry: Give a minion +2 Attack this turn."
    :on-play      (fn [state player-id minion-id target-id] (when (minion? (get-minion state target-id))
                                                              (modify-minion-stats state target-id 2 0 1)))}

   })


(definitions/add-definitions! card-definitions)


