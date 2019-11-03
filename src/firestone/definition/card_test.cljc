(ns firestone.definition.card_test
  (:require [ysera.test :refer [deftest is is-not is= error?]]
            [ysera.error :refer [error]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [create-game
                                         create-card
                                         create-minion
                                         do-game-event-functions
                                         get-card
                                         get-characters
                                         get-hand
                                         get-hero
                                         get-minion
                                         get-minions
                                         get-minion-stats
                                         get-other-player-id
                                         give-deathrattle
                                         has-divine-shield?
                                         has-taunt?
                                         update-minion]]
            [firestone.core :refer [deal-damage
                                    do-on-play
                                    do-deathrattle
                                    get-health]]
            [firestone.core-api :refer [attack-minion
                                        draw-card
                                        end-turn
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
         "take control of random enemy minion on death"
         (is= (-> (create-game [{:minions [(create-minion "Uncle Nilsson" :id "n")]}
                                {:minions [(create-minion "Mio" :id "m")]}])
                  (do-deathrattle "n")
                  (get-minions "p2")
                  (count))
              0))

(deftest Madicken
         "summons elisabeth on death"
         (is= (-> (create-game [{:minions [(create-minion "Madicken" :id "m")]}])
                  (do-deathrattle "m")
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

(deftest Insect-swarm
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
              [5 6]))

(deftest Astrid
         "Battlecry: copy another minion's deathrattle"
         (is= (as-> (create-game [{:minions [(create-minion "Madicken" :id "m")]
                                   :hand    [(create-card "Astrid" :id "a")]}]) $
                    (play-minion-card $ "p1" "a" 0 "m")
                    (get-minion $ "a")
                    (:deathrattle $))
              "Madicken"))

(deftest Skrallan
         "Gets +2/+2 whenever a friendly minion loses divine shield"
         (is= (as-> (create-game [{:minions [(create-minion "Skrallan" :id "s")
                                             (create-minion "Elisabeth" :id "e")]}]) $
                    (deal-damage $ "e")
                    (get-minion-stats $ "s"))
                    [4 4]))
