(ns firestone.definition.hero-power-test
  (:require [ysera.test :refer [deftest is is-not is= error?]]
            [ysera.error :refer [error]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [create-game
                                         create-card
                                         create-hero
                                         create-minion
                                         get-characters
                                         get-hand
                                         get-hero
                                         get-minion
                                         get-minions
                                         get-minion-stats
                                         get-other-player-id
                                         has-divine-shield?
                                         has-taunt?]]
            [firestone.core :refer [deal-damage
                                    do-deathrattle
                                    get-health]]
            [firestone.core-api :refer [attack-minion
                                        do-hero-power
                                        draw-card
                                        end-turn
                                        play-minion-card
                                        play-spell-card]]))


(deftest Blessing
         "Gives divine shield to target minion"
         (is= (-> (create-game [{:minions [(create-minion "Kato" :id "k")]
                                 :hero    (create-hero "Carl")}])
                  (do-hero-power "p1" :target-id "k")
                  (get-minion "k")
                  (get-in [:properties :permanent])
                  (contains? "Divine Shield"))
              true))

(deftest Strengthen
         "Gives 2 random friendly minions 1 damage and +2 attack"
         ;check Gustaf's strengthen
         (as-> (create-game [{:minions [(create-minion "Emil" :id "e1")
                                        (create-minion "Emil" :id "e2")]
                              :hero    (create-hero "Gustaf")}]) $
               (do-hero-power $ "p1")
               (do (is= (->> (get-minions $ "p1")
                             (map :damage-taken))
                        [1, 1])
                   (is= (get-minion-stats $ "e1")
                        [4, 4])
                   (is= (get-minion-stats $ "e2")
                        [4, 4])))
         ;<2 minions available
         (as-> (create-game [{:minions [(create-minion "Emil" :id "e1")]
                              :hero    (create-hero "Gustaf")}]) $
               (do-hero-power $ "p1")
               (do (is= (->> (get-minions $ "p1")
                             (map :damage-taken))
                        [1])
                   (is= (get-minion-stats $ "e1")
                        [4, 4])))
         ;>2 minions available
         (as-> (create-game [{:minions [(create-minion "Emil" :id "e1")
                                        (create-minion "Emil" :id "e2")
                                        (create-minion "Emil" :id "e3")
                                        (create-minion "Emil" :id "e4")]
                              :hero    (create-hero "Gustaf")}]) $
               (do-hero-power $ "p1")
               (do (is= (->> (get-minions $ "p1")
                             (map :damage-taken))
                        [1, 0, 1, 0])
                   (is= (get-minion-stats $ "e1")
                        [4,4])
                   (is= (get-minion-stats $ "e2")
                        [2,5])
                   (is= (get-minion-stats $ "e3")
                        [4,4])
                   (is= (get-minion-stats $ "e4")
                        [2,5])))
         )
