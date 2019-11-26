(ns firestone.definition.card_test
  (:require [ysera.test :refer [deftest is is-not is= error?]]
            [ysera.error :refer [error]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [add-minion-to-board
                                         add-secret-to-player
                                         create-game
                                         create-card
                                         create-secret
                                         create-minion
                                         do-game-event-functions
                                         get-active-secrets
                                         get-card
                                         get-character
                                         get-characters
                                         get-deck
                                         get-hand
                                         get-hero
                                         get-minion
                                         get-minions
                                         get-minion-card-stat-buffs
                                         get-minion-properties
                                         get-minion-stats
                                         get-other-player-id
                                         get-secret
                                         give-deathrattle
                                         has-divine-shield?
                                         has-taunt?
                                         minion?
                                         remove-card-from-deck
                                         update-mana
                                         update-minion
                                         valid-minion-effect-target?]]
            [firestone.core :refer [deal-damage
                                    do-battlecry
                                    do-deathrattle
                                    get-health
                                    silence-minion]]
            [firestone.core-api :refer [attack-minion
                                        attack-hero-or-minion
                                        draw-card
                                        end-turn
                                        play-card
                                        play-minion-card
                                        play-spell-card]]))

(deftest Kato
         "Battlecry deals 4 damage to enemy hero"
         (is= (as-> (create-game [{:hand [(create-card "Kato" :id "k")]}]) $
                    (play-minion-card $ "p1" "k" 0)
                    (:damage-taken (get-hero $ "h2")))
              4))

(deftest Emil
         "Battlecry draws a card"
         (is= (as-> (create-game [{:hand [(create-card "Emil" :id "e")]
                                   :deck [(create-card "Ronja" :id "r")]}]) $
                    (play-minion-card $ "p1" "e" 0)
                    (get-hand $ "p1")
                    (count $))
              1)
         "Fatigue should trigger if no more cards"
         (is= (as-> (create-game [{:hand [(create-card "Emil" :id "e")]}]) $
                    (play-minion-card $ "p1" "e" 0)
                    (get-hero $ "h1")
                    (:damage-taken $))
              1)
         )

(deftest Jonatan
         "Jonatan has taunt"
         (is (-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}])
                 (has-taunt? "j")
                 )))

(deftest Alfred
         "Alfred cannot attack"
         (error? (-> (create-game [{:minions [(create-minion "Alfred" :id "a")]}
                                   {:minions [(create-minion "Ronja" :id "r")]}])
                     (attack-minion "p1" "a" "r")
                     )))

(deftest Uncle-Melker
         "Uncle Melker has divine shield"
         (is (-> (create-game [{:minions [(create-minion "Uncle Melker" :id "m")]}])
                 (has-divine-shield? "m")
                 )))

(deftest Pippi
         "Pippi deals 1 damage to all other minions at the end of owner's turn"
         (is (as-> (create-game [{:minions [(create-minion "Pippi" :id "p")
                                            (create-minion "Ronja" :id "r")]}
                                 {:minions [(create-minion "Alfred" :id "a")
                                            (create-minion "Madicken" :id "m")]}]) $
                   (end-turn $ "p1")
                   ;(end-turn $ "p2")
                   ;(end-turn $ "p1")) "This results in a weird nil problem"
                   (do
                     (and (is= (:damage-taken (get-minion $ "p")) 0)
                          (is= (:damage-taken (get-minion $ "r")) 1)
                          (is= (:damage-taken (get-minion $ "a")) 1)
                          (is= (:damage-taken (get-minion $ "m")) 1))
                     (-> (end-turn $ "p2")
                         (do
                           (and (is= (:damage-taken (get-minion $ "p")) 0)
                                (is= (:damage-taken (get-minion $ "r")) 1)
                                (is= (:damage-taken (get-minion $ "a")) 1)
                                (is= (:damage-taken (get-minion $ "m")) 1))
                           ))))))

(deftest Karlsson
         "Gives random minion taunt at end of turn"
         (is (as-> (create-game [{:minions [(create-minion "Karlsson" :id "k")
                                            (create-minion "Ronja" :id "r")]}]) $
                   (end-turn $ "p1")
                   (has-taunt? $ "k"))))

(deftest Uncle-Nilsson
         "Take control of random enemy minion on death"
         (is= (-> (create-game [{:minions [(create-minion "Uncle Nilsson" :id "n")]}
                                {:minions [(create-minion "Mio" :id "m")]}])
                  (deal-damage "n" 10)
                  (get-minions "p2")
                  (count))
              0))

(deftest Madicken
         "Summons elisabeth on death"
         (is= (-> (create-game [{:minions [(create-minion "Madicken" :id "m")]}])
                  (deal-damage "m" 2)
                  (get-minions "p1")
                  (first)
                  (:name))
              "Elisabeth"))
(deftest Ida
         "When a minion takes damage, gain taunt"
         (is (as-> (create-game [{:minions [(create-minion "Alfred" :id "a")
                                            (create-minion "Ida" :id "i")]}]) $
                   (deal-damage $ "a" 1)
                   (has-taunt? $ "i"))))

(deftest Insect-Swarm
         "Deal 2 damage to all characters"
         (is= (as-> (create-game [{:minions [(create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]
                                   :hand    [(create-card "Insect Swarm" :id "is")]}]) $
                    (play-spell-card $ "p1" "is")
                    (get-characters $)
                    (map :damage-taken $))
              [2, 2, 2, 2]))

(deftest Radar-Raid
         "Deal 3 damage to a character"
         (is= (as-> (create-game [{:minions [(create-minion "Alfred" :id "a")
                                             (create-minion "Jonatan" :id "j")]
                                   :hand    [(create-card "Radar Raid" :id "rr")]}]) $
                    (play-spell-card $ "p1" "rr" "a")
                    (get-minion $ "a")
                    (:damage-taken $))
              3))

(deftest Annika
         "Give a minion +2 attack this turn"
         (is= (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]
                                   :hand    [(create-card "Annika" :id "a")]}]) $
                    (play-minion-card $ "p1" "a" 1 "j")
                    (get-minion-stats $ "j"))
              [5 6])
         (is (as-> (create-game [{:minions [(create-minion "Madicken" :id "m")]
                                  :hand    [(create-card "Annika" :id "a")]}]) $
                   (valid-minion-effect-target? $ "a" "m"))))

(deftest Astrid
         "Battlecry: copy another minion's deathrattle"
         (is= (as-> (create-game [{:minions [(create-minion "Madicken" :id "m")]
                                   :hand    [(create-card "Astrid" :id "a")]}]) $
                    (play-minion-card $ "p1" "a" 0 "m")
                    (do (is= (:deathrattle (get-minion $ "a")) "Madicken")
                        (deal-damage $ "a" 3))
                    (get-minion $ "m3")
                    (:name $))
              "Elisabeth")
         (is (as-> (create-game [{:minions [(create-minion "Madicken" :id "m")]
                                  :hand    [(create-card "Astrid" :id "a")]}]) $
                   (valid-minion-effect-target? $ "a" "m")))
         (is-not (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]
                                      :hand    [(create-card "Astrid" :id "a")]}]) $
                       (valid-minion-effect-target? $ "a" "m"))))

(deftest Skrallan
         "Gets +2/+2 whenever a friendly minion loses divine shield"
         (is= (as-> (create-game [{:minions [(create-minion "Skrallan" :id "s")
                                             (create-minion "Elisabeth" :id "e")]}]) $
                    (deal-damage $ "e")
                    (get-minion-stats $ "s"))
              [4 4]))

(deftest Herr-Nilsson
         "Has poisonous"
         (is (as-> (create-game [{:minions [(create-minion "Herr Nilsson" :id "h")]}
                                 {:minions [(create-minion "Jonatan" :id "j")]}]) $
                   (end-turn $ "p1")
                   (end-turn $ "p2")
                   (attack-minion $ "p1" "h" "j")
                   (get-minions $)
                   (empty? $))))

(deftest Rasmus
         "Has windfury"
         (is (as-> (create-game [{:minions [(create-minion "Rasmus" :id "r")]}
                                 {:minions [(create-minion "Jonatan" :id "j")]}]) $
                   (end-turn $ "p1")
                   (end-turn $ "p2")
                   (attack-minion $ "p1" "r" "j")
                   (attack-minion $ "p1" "r" "j")
                   (get-minions $)
                   (empty? $))))

(deftest Tjorven
         "Your other minions have windfury"
         (is= (as-> (create-game [{:minions [(create-minion "Tjorven" :id "t")
                                             (create-minion "Emil" :id "e")]}
                                  {:minions [(create-minion "Karlsson" :id "k")]}]) $
                    (end-turn $ "p1")
                    (end-turn $ "p2")
                    (attack-minion $ "p1" "e" "k")
                    (attack-minion $ "p1" "e" "k")
                    (get-minions $)
                    (map :id $))
              ["t" "e"]))

;todo update this once a secret card works
(deftest Secretkeeper
         "Gain +1/+1"
         (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")
                                           (create-minion "Secretkeeper" :id "s")]
                                 :active-secrets [(create-secret "Explosive Trap" "p1" :id "t")
                                                  (create-secret "Venomstrike Trap" "p1" :id "v")]}])
                  ;(play-minion-card "p1" "s" 0) should be play-secret-card instead
                  (get-minion-stats "s"))
              [1, 2]))

(deftest Leeroy-Jenkins
         "Battlecry: Summon two 1/1 Whelps for your opponent"
         (is= (as-> (create-game [{:minions [(create-minion "Tjorven" :id "t")]}
                                  {:hand [(create-card "Leeroy Jenkins" :id "l")]}]) $
                    (play-minion-card $ "p2" "l" 0)
                    (get-minions $ "p1")
                    (map :name $))
              ["Tjorven", "Whelp", "Whelp"]))

(deftest Kezan-Mystic
         "Battlecry: Take control of a random enemy Secret."
         (is= (as-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")
                                                    (create-secret "Venomstrike Trap" "p1" :id "v")]}
                                  {:hand [(create-card "Kezan Mystic" :id "s")]}]) $
                    (play-minion-card $ "p2" "s" 0)
                    (get-active-secrets $ "p2")
                    (count $))
              1))

(deftest Eater-of-Secrets
         "Battlecry: Destroy all enemy Secrets. Gain +1/+1 for each."
         (is= (as-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")
                                                    (create-secret "Venomstrike Trap" "p1" :id "v")]}
                                  {:hand [(create-card "Eater of Secrets" :id "s")]}]) $
                    (play-minion-card $ "p2" "s" 0)
                    (get-active-secrets $ "p1"))
              ())
         (is= (as-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")
                                                    (create-secret "Venomstrike Trap" "p1" :id "v")]}
                                  {:hand [(create-card "Eater of Secrets" :id "s")]}]) $
                    (play-minion-card $ "p2" "s" 0)
                    (get-minion-stats $ "s"))
              [4,6]))

;todo play-card will call play-secret-card which currently does not work
(deftest Vaporize
         "Secret: When a minion attacks your hero destroy it."
         (is= (as-> (create-game [{:active-secrets [(create-secret "Vaporize" "p1" :id "v")]}
                                  {:minions [(create-minion "Mio" :id "m")]}]) $
                    ((:on-attack (get-definition "Vaporize")) $ "p1" "m" "h1")
                    (get-minions $ "p2"))
              ()))

(deftest Mad-Scientist
         "Deathrattle: Put a Secret from your deck into the battlefield."
         (is= (as-> (create-game [{:deck    [(create-card "Vaporize" :id "v")
                                             (create-card "Radar Raid" :id "r")]
                                   :minions [(create-minion "Mad Scientist" :id "ms")]}]) $
                    (deal-damage $ "ms" 4)
                    (:name (first (get-active-secrets $ "p1"))))
              "Vaporize")
         )

(deftest Venomstrike-Trap
         "Secret: When one of your minions is attacked summon a 2/3 Poisonous Cobra."
         (is= (as-> (create-game [{:active-secrets [(create-secret "Venomstrike Trap" "p1" :id "vt")]
                                   :minions        [(create-minion "Ronja" :id "r")]}
                                  {:minions        [(create-minion "Leeroy Jenkins" :id "l")]}]) $
                    (end-turn $ "p1")
                    (attack-hero-or-minion $ "p2" "l" "r")
                    (:name (get-minion $ "m3")))
              "Emperor Cobra"))

(deftest Explosive-Trap
         "Secret: When your hero is attacked deal 2 damage to all enemies."
         (is= (as-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "et")]
                                   :minions        [(create-minion "Ronja" :id "r")]}
                                  {:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")]}]) $
                    ((:on-attack (get-definition "Explosive Trap")) $ "p1" "m" "h1")
                    (:damage-taken (get-minion $ "e")))
              2))

(deftest Silence
         "Silence a minion"
         (is= (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]
                                   :hand    [(create-card "Silence" :id "s")]}]) $
                    (play-spell-card $ "p1" "s" "j")
                    (get-minion-properties $ "j"))
              {:permanent #{"silenced"}, :temporary {}, :stats {}})
         )

(deftest Spellbreaker
         "Battlecry: Silence a minion"
         (is= (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}
                                  {:hand [(create-card "Spellbreaker" :id "s")]}]) $
                    (play-minion-card $ "p2" "s" 0 "j")
                    (get-minion-properties $ "j"))
              {:permanent #{"silenced"}, :temporary {}, :stats {}})
         )

(deftest The-Mistcaller
         "Battlecry: Give all minions in your hand and deck +1/+1."
         (as-> (create-game [{:hand [(create-card "Emil" :id "e")
                                     (create-card "The Mistcaller" :id "tm")]
                              :deck [(create-card "Madicken" :id "m")]}]) $
               (play-minion-card $ "p1" "tm" 0)
               (do
                 (is= (get-minion-card-stat-buffs $ "e")
                      [1 1])
                 (is= (get-minion-card-stat-buffs $ "m")
                      [1 1])
                 (play-minion-card $ "p1" "e" 1))
               (update-mana $ "p1" 10)
               (play-minion-card $ "p1" "m" 2)
               (do
                 (is= (get-minion-stats $ "e")
                      [3 6])
                 (is= (get-minion-stats $ "m")
                      [2 3]))))

(deftest Shudderwock
         "Battlecry: Repeat all battlecries played this game"
         ;only battlecry without target
         (as-> (create-game [{:hand [(create-card "Shudderwock" :id "s")]
                              :minions [(create-minion "Emil" :id "e")]}]
                            :cards-played-this-game [{:name "Kato", :entity-type :card, :id "k", :owner-id "p1"}
                                                     {:name "Ronja", :entity-type :card, :id "r", :owner-id "p1"}
                                                     {:name "Radar Raid", :entity-type :card, :id "rr", :owner-id "p2"}
                                                     ]) $
               (play-minion-card $ "p1" "s" 0)
               (is= (get-health $ "h2")
                      26))
         ;only battlecry with target
         (as-> (create-game [{:hand [(create-card "Shudderwock" :id "s")]
                              :minions [(create-minion "Emil" :id "e")]}]
                            :cards-played-this-game [{:name "Annika", :entity-type :card, :id "a1", :owner-id "p2"}
                                                     {:name "Annika", :entity-type :card, :id "a2", :owner-id "p2"}
                                                     {:name "Ronja", :entity-type :card, :id "r", :owner-id "p1"}
                                                     {:name "Radar Raid", :entity-type :card, :id "rr", :owner-id "p2"}
                                                     ]) $
               (play-minion-card $ "p1" "s" 0)
               (do
                 (is= (get-minion-stats $ "e")
                      [4,5])
                 (is= (get-minion-stats $ "s")
                      [8,6])))
         ;battlecries with both target and no target
         (as-> (create-game [{:hand [(create-card "Shudderwock" :id "s")]
                              :minions [(create-minion "Emil" :id "e")]}]
                            :cards-played-this-game [{:name "Kato", :entity-type :card, :id "k", :owner-id "p1"}
                                                     {:name "Kato", :entity-type :card, :id "k2", :owner-id "p1"}
                                                     {:name "Ronja", :entity-type :card, :id "r", :owner-id "p1"}
                                                     {:name "Annika", :entity-type :card, :id "a1", :owner-id "p2"}
                                                     {:name "Annika", :entity-type :card, :id "a2", :owner-id "p2"}
                                                     {:name "Radar Raid", :entity-type :card, :id "rr", :owner-id "p2"}
                                                     ]) $
               (play-minion-card $ "p1" "s" 0)
               (do
                 (is= (get-health $ "h2")
                      22)
                 (is= (get-minion-stats $ "e")
                      [4,5])
                 (is= (get-minion-stats $ "s")
                      [8,6])))
         )

;todo update this once a secret card works
(deftest Secretkeeper
         "Gain +1/+1"
         (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")
                                           (create-minion "Secretkeeper" :id "s")]
                                 :active-secrets [(create-secret "Explosive Trap" "p1" :id "t")
                                                  (create-secret "Venomstrike Trap" "p1" :id "v")]}])
                  ;(play-minion-card "p1" "s" 0) should be play-secret-card instead
                  (get-minion-stats "s"))
              [1, 2]))

(deftest Leeroy-Jenkins
         "Battlecry: Summon two 1/1 Whelps for your opponent"
         (is= (as-> (create-game [{:minions [(create-minion "Tjorven" :id "t")]}
                                {:hand [(create-card "Leeroy Jenkins" :id "l")]}]) $
                  (play-minion-card $ "p2" "l" 0)
                  (get-minions $ "p1")
                    (map :name $))
              ["Tjorven", "Whelp", "Whelp"]))

(deftest Kezan-Mystic
         "Battlecry: Take control of a random enemy Secret."
         (is= (as-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")
                                                    (create-secret "Venomstrike Trap" "p1" :id "v")]}
                                  {:hand [(create-card "Kezan Mystic" :id "s")]}]) $
                    (play-minion-card $ "p2" "s" 0)
                    (get-active-secrets $ "p2")
                    (count $))
              1))

(deftest Eater-of-Secrets
         "Battlecry: Destroy all enemy Secrets. Gain +1/+1 for each."
         (is= (as-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")
                                                    (create-secret "Venomstrike Trap" "p1" :id "v")]}
                                  {:hand [(create-card "Eater of Secrets" :id "s")]}]) $
                    (play-minion-card $ "p2" "s" 0)
                    (get-active-secrets $ "p1"))
              ())
         (is= (as-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")
                                                    (create-secret "Venomstrike Trap" "p1" :id "v")]}
                                  {:hand [(create-card "Eater of Secrets" :id "s")]}]) $
                    (play-minion-card $ "p2" "s" 0)
                    (get-minion-stats $ "s"))
              [4,6]))
