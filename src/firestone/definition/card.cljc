(ns firestone.definition.card
  (:require [firestone.definitions :as definitions]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [add-minions-to-board
                                         add-minion-to-board
                                         add-secret-to-player
                                         buff-minion-card
                                         character?
                                         create-game
                                         create-card
                                         create-minion
                                         create-secret
                                         friendly-minions?
                                         get-active-secrets
                                         get-card
                                         get-deck
                                         get-all-played-cards-with-property
                                         get-character
                                         get-deck
                                         get-hand
                                         get-hero
                                         get-minion
                                         get-minions
                                         get-other-player-id
                                         get-player
                                         get-random-minion
                                         get-random-minion-conditional
                                         get-random-secret
                                         get-secret
                                         give-deathrattle
                                         give-taunt
                                         minion?
                                         modify-minion-stats
                                         replace-minion
                                         remove-card-from-deck
                                         remove-minion
                                         remove-secret
                                         switch-minion-side
                                         switch-secret-side
                                         update-minion
                                         update-seed
                                         valid-minion-effect-target?]]
            [firestone.core :refer [deal-damage
                                    deal-damage-to-other-minions
                                    deal-damage-to-all-minions
                                    deal-damage-to-all-heroes
                                    do-battlecry
                                    silence-minion]]
            [firestone.core-api :refer [draw-card
                                        play-card]]))

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
    :battlecry   (fn [state player-id _]
                   (let [target-hero-id (get-in state [:players (get-other-player-id player-id) :hero :id])]
                     (deal-damage state target-hero-id 4)))}

   "Emil"
   {:name        "Emil"
    :attack      2
    :health      5
    :mana-cost   4
    :type        :minion
    :description "Battlecry: Draw a card."
    :battlecry   (fn [state player-id _]
                   (draw-card state player-id))}

   "Jonatan"
   {:name        "Jonatan"
    :attack      3
    :health      6
    :mana-cost   4
    :type        :minion
    :properties  #{"taunt"}
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
    :properties  #{"divine-shield"}
    :set         :custom
    :description "Divine Shield."}

   "Pippi"
   {:name           "Pippi"
    :attack         2
    :health         4
    :mana-cost      3
    :type           :minion
    :on-end-of-turn (fn [state this-minion-id kvs]
                      (deal-damage-to-other-minions state this-minion-id 1))
    :set            :custom
    :description    "At the end of your turn deal 1 damage to all other minions."}

   "Karlsson"
   {:name           "Karlsson"
    :attack         1
    :health         4
    :mana-cost      3
    :type           :minion
    :on-end-of-turn (fn [state _ _]
                      (let [random-result (get-random-minion state)]
                        (let [state (first random-result)
                              random-minion (last random-result)]
                          (give-taunt state (:id random-minion)))))
    :set            :custom
    :description    "At the end of your turn give a random minion taunt."}

   "Uncle Nilsson"
   {:name        "Uncle Nilsson"
    :attack      5
    :health      5
    :mana-cost   6
    :type        :minion
    :set         :custom
    :description "Deathrattle: Take control of a random enemy minion."
    :deathrattle (fn [state minion-id]
                   (let [random-result (get-random-minion state (get-other-player-id (:owner-id (get-minion state minion-id))))]
                     (switch-minion-side state (:id (last random-result)))))}

   "Elisabeth"
   {:name        "Elisabeth"
    :attack      1
    :health      1
    :mana-cost   1
    :type        :minion
    :properties  #{"taunt", "divine-shield"}
    :set         :custom
    :description "Taunt. Divine Shield."}

   "Madicken"
   {:name        "Madicken"
    :attack      1
    :health      2
    :mana-cost   2
    :type        :minion
    :set         :custom
    :description "Deathrattle: Summon Elisabeth."
    :deathrattle (fn [state minion-id]
                   (let [position (:position (get-minion state minion-id))
                         owner-id (:owner-id (get-minion state minion-id))]
                     (add-minion-to-board state owner-id (create-minion "Elisabeth") position)))}

   "Ida"
   {:name             "Ida"
    :attack           2
    :health           4
    :mana-cost        3
    :type             :minion
    :on-minion-damage (fn [state this-minion-id kvs]
                        (give-taunt state this-minion-id))
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
   {:name          "Radar Raid"
    :mana-cost     2
    :type          :spell
    :set           :custom
    :valid-target? (fn [_ target]
                     (character? target))
    :spell-fn      (fn [state character-id]
                     (deal-damage state character-id 3))
    :description   "Deal 3 damage to a character."}

   "Herr Nilsson"
   {:name        "Herr Nilsson"
    :attack      1
    :health      3
    :mana-cost   3
    :type        :minion
    :properties  #{"poisonous"}
    :set         :custom
    :description "Poisonous"}

   "Rasmus"
   {:name        "Rasmus"
    :attack      3
    :health      5
    :mana-cost   5
    :type        :minion
    :properties  #{"windfury"}
    :set         :custom
    :description "Windfury"}

   "Tjorven"
   {:name        "Tjorven"
    :attack      0
    :health      2
    :mana-cost   5
    :type        :minion
    :set         :custom
    :description "Your other minions has windfury."
    :aura        #{"windfury"}}

   "Astrid"
   {:name          "Astrid"
    :attack        3
    :health        3
    :mana-cost     4
    :type          :minion
    :set           :custom
    :description   "Battlecry: Copy another minions deathrattle."
    :valid-target? (fn [state target]
                     (and (minion? target)
                          (some? (:deathrattle (get-minion state (:id target))))))
    :battlecry     (fn [state _ minion-id target-id]
                     (give-deathrattle state minion-id (:name (get-minion state target-id))))}

   "Skrallan"
   {:name                     "Skrallan"
    :attack                   2
    :health                   2
    :mana-cost                3
    :type                     :minion
    :set                      :custom
    :description              "After a friendly minion loses Divine Shield, gain +2/+2."
    :on-divine-shield-removal (fn [state this-minion-id kvs]
                                (if (friendly-minions? state this-minion-id (:target-id kvs))
                                  (modify-minion-stats state this-minion-id 2 2)
                                  state))}

   "Annika"
   {:name          "Annika"
    :attack        2
    :health        2
    :mana-cost     3
    :type          :minion
    :set           :custom
    :description   "Battlecry: Give a minion +2 Attack this turn."
    :valid-target? (fn [_ target] (minion? target))
    :battlecry     (fn [state _ _ target-id] (when (minion? (get-minion state target-id))
                                               (modify-minion-stats state target-id 2 0 1)))}

   "Al'Akir the Windlord"
   {:name        "Al'Akir the Windlord"
    :attack      3
    :health      5
    :mana-cost   8
    :type        :minion
    :properties  #{"windfury", "taunt", "divine-shield" "charge"}
    :set         :classic
    :rarity      :legendary
    :description "Windfury, Charge, Divine Shield, Taunt"}

   "Secretkeeper"
   {:name          "Secretkeeper",
    :attack        1
    :health        2
    :mana-cost     1
    :type          :minion
    :set           :classic
    :rarity        :rare
    :secret-played (fn [state minion-id]
                     (modify-minion-stats state minion-id 1 1))
    :description   "Whenever a Secret is played, gain +1/+1."}

   "Mad Scientist"
   {:name        "Mad Scientist"
    :attack      2
    :health      2
    :mana-cost   2
    :type        :minion
    :set         :curse-of-naxxramas
    :rarity      :common
    :description "Deathrattle: Put a Secret from your deck into the battlefield."
    :deathrattle (fn [state minion-id]
                   (let [owner-id (:owner-id (get-minion state minion-id))
                         secret-id (first (filter (fn [s] (= (:sub-type (get-definition (get-card state s))) :secret)) (map :id (get-deck state owner-id))))]
                     (as-> state $
                           (add-secret-to-player $ owner-id (create-secret (:name (get-card $ secret-id)) owner-id))
                           (remove-card-from-deck $ owner-id secret-id))))}

   "Eater of Secrets"
   {:name        "Eater of Secrets"
    :attack      2
    :health      4
    :mana-cost   4
    :type        :minion
    :set         :whispers-of-the-old-gods
    :rarity      :rare
    :battlecry   (fn [state player-id minion-id]
                   (reduce (fn [state secret-id]
                             (-> (remove-secret state secret-id)
                                 (modify-minion-stats minion-id 1 1)))
                           state
                           (map :id (get-active-secrets state (get-other-player-id player-id)))))
    :description "Battlecry: Destroy all enemy Secrets. Gain +1/+1 for each."}

   "Kezan Mystic"
   {:name        "Kezan Mystic"
    :attack      4
    :health      3
    :mana-cost   4
    :type        :minion
    :set         :goblins-vs-gnomes
    :rarity      :rare
    :battlecry   (fn [state player-id _]
                   (let [random-result (get-random-secret state (get-other-player-id player-id))]
                     (let [state (first random-result)
                           random-secret (last random-result)]
                       (if (some? random-secret)
                         (switch-secret-side state (:id random-secret))
                         state))))
    :description "Battlecry: Take control of a random enemy Secret."}

   "Stormwind Knight"
   {:name        "Stormwind Knight"
    :attack      2
    :health      5
    :mana-cost   4
    :set         :basic
    :type        :minion
    :properties  #{"charge"}
    :description "Charge"}

   "Leeroy Jenkins"
   {:name        "Leeroy Jenkins"
    :attack      6
    :health      2
    :mana-cost   5
    :type        :minion
    :properties  #{"charge"}
    :set         :classic
    :rarity      :legendary
    :description "Charge. Battlecry: Summon two 1/1 Whelps for your opponent."
    :battlecry   (fn [state player-id _]
                   (add-minions-to-board state (get-other-player-id player-id) [(create-minion "Whelp")
                                                                                (create-minion "Whelp")]))}

   "The Mistcaller"
   {:name        "The Mistcaller"
    :attack      4
    :health      4
    :mana-cost   6
    :type        :minion
    :set         :the-grand-tournament
    :rarity      :legendary
    :description "Battlecry: Give all minions in your hand and deck +1/+1."
    :battlecry   (fn [state player-id _]
                   (let [card-ids (concat (map :id (get-hand state player-id)) (map :id (get-deck state player-id)))]
                     (reduce (fn [new-state card-id] (buff-minion-card new-state card-id 1 1)) state card-ids)))}

   "Spellbreaker"
   {:name          "Spellbreaker"
    :attack        4
    :health        3
    :mana-cost     4
    :type          :minion
    :set           :classic
    :rarity        :common
    :description   "Battlecry: Silence a minion."
    :valid-target? (fn [_ target] (minion? target))
    :battlecry     (fn [state _ _ target-id]
                     (silence-minion state target-id))}

   "Shudderwock"
   {:name        "Shudderwock"
    :attack      6
    :health      6
    :mana-cost   9
    :type        :minion
    :set         :the-witchwood
    :rarity      :legendary
    :description "Battlecry: Repeat all other Battlecries from cards you played this game (targets chosen randomly)."
    :battlecry   (fn [state player-id minion-id]
                   (let [minion-defs (map (fn [card] (get-definition card)) (get-all-played-cards-with-property state :battlecry))
                         minion-defs-with-target (filter (fn [minion-def] (some? (:valid-target? minion-def))) minion-defs)
                         minion-defs-without-target (filter (fn [minion-def] (and (nil? (:valid-target? minion-def)) (not= (:name minion-def) "Shudderwock"))) minion-defs)]
                     (as-> state $
                           ;do battlecries that don't require target
                           (reduce (fn [state minion-def]
                                     (do-battlecry state player-id minion-id minion-def)) $ minion-defs-without-target)
                           ;;do battlecries that require a target
                           (reduce (fn [state minion-def]
                                     (let [random-result
                                           (get-random-minion-conditional state
                                                                          (fn [game-state minion-id]
                                                                            (valid-minion-effect-target? game-state minion-def minion-id)))
                                           new-state (first random-result)
                                           random-minion-id (:id (last random-result))]
                                       (do-battlecry new-state player-id minion-id minion-def random-minion-id))) $ minion-defs-with-target))))}
   "Silence"
   {:name          "Silence"
    :mana-cost     0
    :type          :spell
    :set           :classic
    :rarity        :common
    :description   "Silence a minion."
    :valid-target? (fn [_ target]
                     (character? target))
    :spell-fn      (fn [state minion-id]
                     (silence-minion state minion-id))}

   "Explosive Trap"
   {:name           "Explosive Trap"
    :mana-cost      2
    :type           :spell
    :sub-type       :secret
    :set            :classic
    :rarity         :common
    :description    "Secret: When your hero is attacked deal 2 damage to all enemies."
    :valid-trigger? (fn [state player-id attacker-id victim-id]
                      (= player-id (:owner-id (get-character state victim-id)))
                      (= (:entity-type (get-character state victim-id)) :hero))
    :on-attack      (fn [state player-id attacker-id target-id]
                      (let [attacker-owner-id (:owner-id (get-character state attacker-id))
                            victim-hero (get-hero state target-id)
                            enemy-player (get-player state attacker-owner-id)
                            enemy-hero (get-hero state (get-in state [:players (:id enemy-player) :hero :id]))
                            enemy-minions (get-minions state attacker-owner-id)]
                        (as-> state $
                              (reduce (fn [state minion]
                                        (deal-damage state (:id minion) 2))
                                      $
                                      enemy-minions)
                              (deal-damage $ (:id enemy-hero) 2))))}

   "Venomstrike Trap"
   {:name           "Venomstrike Trap"
    :mana-cost      2
    :type           :spell
    :sub-type       :secret
    :set            :knights-of-the-frozen-throne
    :rarity         :rare
    :description    "Secret: When one of your minions is attacked summon a 2/3 Poisonous Cobra."
    :valid-trigger? (fn [state player-id attacker-id victim-id]
                      (and
                        (= player-id (:owner-id (get-character state victim-id)))
                        (= (:entity-type (get-character state victim-id)) :minion)))
    :on-attack      (fn [state player-id attacker-id target-id]
                      (let [target-owner-id (:owner-id (get-character state target-id))]
                        (add-minion-to-board state target-owner-id (create-minion "Emperor Cobra") 0)))}

   "Vaporize"
   {:name           "Vaporize"
    :mana-cost      3
    :type           :spell
    :sub-type       :secret
    :set            :classic
    :rarity         :rare
    :description    "Secret: When a minion attacks your hero destroy it."
    :valid-trigger? (fn [state player-id attacker-id victim-id]
                      (and
                        (= player-id (:owner-id (get-character state victim-id)))
                        (= (:entity-type (get-character state victim-id)) :hero)
                        (= (:entity-type (get-character state attacker-id)) :minion)))
    :on-attack      (fn [state player-id attacker-id target-id]
                      (as-> state $
                            (let [victim-owner-id (:owner-id (get-character $ target-id))
                                  victim-player (get-player $ victim-owner-id)
                                  victim-hero (:hero victim-player)]
                              (if (= target-id (:id victim-hero))
                                (remove-minion $ attacker-id)
                                $))))}

   "Whelp"
   {:name      "Whelp"
    :attack    1
    :health    1
    :mana-cost 1
    :set       :classic
    :type      :minion
    :rarity    :common}

   "Emperor Cobra"
   {:name        "Emperor Cobra"
    :attack      2
    :health      3
    :mana-cost   3
    :properties  #{"poisonous"}
    :type        :minion
    :set         :classic
    :rarity      :rare
    :description "Poisonous."}

   ;; sprint 5 definitions

   "Acolyte of Pain"
   {:name        "Acolyte of Pain"
    :attack      1
    :health      3
    :mana-cost   3
    :type        :minion
    :rarity      :common
    :set         :classic
    :description "Whenever this minion takes damage, draw a card."
    :on-minion-damage (fn [state this-minion-id kvs]
                        (if kvs
                          (let [this-minion (get-minion state this-minion-id)
                                damaged-id (:damaged-id kvs)]
                            (if (and this-minion-id
                                     damaged-id
                                     (= this-minion-id damaged-id))
                              (draw-card state (:owner-id this-minion))
                              state))
                          state))}

   "Flesheating Ghoul"
   {:name        "Flesheating Ghoul"
    :attack      2
    :health      3
    :mana-cost   3
    :set         :classic
    :rarity      :common
    :type        :minion
    :description "Whenever a minion dies, gain +1 Attack."}

   "Hadronox"
   {:name        "Hadronox"
    :attack      3
    :health      7
    :mana-cost   9
    :type        :minion
    :set         :knights-of-the-frozen-throne
    :rarity      :legendary
    :description "Deathrattle: Summon your Taunt minions that died this game."}

   "Knife Juggler"
   {:name        "Knife Juggler"
    :attack      2
    :health      2
    :mana-cost   2
    :type        :minion
    :set         :classic
    :rarity      :rare
    :description "After you summon a minion, deal 1 damage to a random enemy."}

   "Snake"
   {:name      "Snake"
    :attack    1
    :health    1
    :mana-cost 1
    :type      :minion
    :set       :classic}

   "Snake Trap"
   {:name        "Snake Trap"
    :type        :spell
    :mana-cost   2
    :set         :classic
    :rarity      :epic
    :sub-type    :secret
    :description "Secret: When one of your minions is attacked summon three 1/1 Snakes."}

   })


(definitions/add-definitions! card-definitions)


