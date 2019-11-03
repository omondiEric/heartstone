(ns firestone.core-api
  (:require [ysera.test :refer [is is= error?]]
            [ysera.error :refer [error]]
            [firestone.definitions :refer [get-definition]]
            [firestone.core :refer [do-on-play
                                    do-game-event-functions
                                    deal-damage
                                    deal-damage-to-all-heroes
                                    deal-damage-to-all-minions
                                    do-game-event-functions
                                    get-attack
                                    get-health
                                    pay-mana
                                    valid-attack?]]
            [firestone.construct :refer [add-card-to-hand
                                         add-minion-to-board
                                         change-player-in-turn
                                         create-card
                                         create-empty-state
                                         create-game
                                         create-hero
                                         create-minion
                                         draw-card-to-hand
                                         fatigue-hero
                                         inc-max-mana
                                         is-poisonous?
                                         get-card
                                         get-deck
                                         get-hand
                                         get-hero
                                         get-mana
                                         get-max-mana
                                         get-minion
                                         get-minions
                                         get-mana
                                         get-mana-cost
                                         get-random-minions-distinct
                                         get-player-id-in-turn
                                         get-players
                                         get-other-player-id
                                         give-divine-shield

                                         remove-card-from-deck
                                         remove-card-from-hand
                                         restore-mana
                                         update-mana
                                         update-max-mana
                                         update-minion]]))
(defn draw-card
  {:test (fn []
           ;a card should appear in the hand
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                    (draw-card "p1")
                    (get-hand "p1")
                    (first)
                    (:name))
                "Emil")
           ;draw cards consecutively
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")
                                          "Mio"]}])
                    (draw-card "p1")
                    (draw-card "p1")
                    (get-hand "p1")
                    (last)
                    (:name))
                "Mio")
           ;the card should be removed from the deck
           (is (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                   (draw-card "p1")
                   (get-deck "p1")
                   (empty?)))
           ;check hand size limit of 10 is maintained
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")]
                                   :hand [(create-card "Mio")
                                          "Mio"
                                          "Mio"
                                          "Mio"
                                          "Mio"
                                          "Mio"
                                          "Mio"
                                          "Mio"
                                          "Mio"
                                          "Mio"]}])
                    (draw-card "p1")
                    (get-hand "p1")
                    (count))
                10)
           ;check fatigue works
           (is= (-> (create-game)
                    (draw-card "p1")
                    (get-in [:players "p1" :hero :damage-taken]))
                1)
           (is= (-> (create-game)
                    (draw-card "p1")
                    (draw-card "p1")
                    (get-in [:players "p1" :hero :damage-taken]))
                3)
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                    (draw-card "p1")
                    (get-in [:players "p1" :hero :damage-taken]))
                0)
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                    (draw-card "p1")
                    (draw-card "p1")
                    (get-in [:players "p1" :hero :damage-taken]))
                1)
           )}
  [state player-id]
  (-> state
      (fatigue-hero player-id)
      (draw-card-to-hand player-id)))

(defn end-turn
  {:test (fn []
           (is= (-> (create-game)
                    (end-turn "p1")
                    (get-player-id-in-turn))
                "p2")
           (is= (-> (create-game)
                    (end-turn "p1")
                    (end-turn "p2")
                    (get-player-id-in-turn))
                "p1")
           (error? (-> (create-game)
                       (end-turn "p2")))

           (is= (-> (create-game)
                    (end-turn "p1")
                    (get-max-mana "p2"))
                10)
           (is= (-> (create-game)
                    (update-max-mana "p2" 5)
                    (end-turn "p1")
                    (get-max-mana "p2"))
                6)
           (is= (-> (create-game)
                    (update-max-mana "p2" 5)
                    (update-mana "p2" 0)
                    (end-turn "p1")
                    (get-mana "p2"))
                6))}

  [state player-id]
  (when-not (= (get-player-id-in-turn state) player-id)
    (error "The player with id " player-id " is not in turn."))

  (let [other-player-id (get-other-player-id player-id)]
    (-> state
        (do-game-event-functions player-id :end-of-turn)
        (assoc-in [:players player-id :hero :hero-power-used] false)
        (change-player-in-turn)
        (inc-max-mana other-player-id)
        (restore-mana other-player-id)
        (draw-card other-player-id))))



(defn play-minion-card
  {:test (fn []
           ; A minion should appear at the board
           (is= (-> (create-game [{:hand [(create-card "Ronja" :id "r")]}])
                    (play-minion-card "p1" "r" 0)
                    (get-minions "p1")
                    (first)
                    (:name))
                "Ronja")
           ; The card should be erased from hand
           (is (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                   (play-minion-card "p1" "e" 0)
                   (get-hand "p1")
                   (empty?)))
           ;
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")] :deck [(create-card "Mio")]}])
                    (play-minion-card "p1" "e" 0)
                    (get-hand "p1")
                    (count))
                1))}
  [state player-id card-id position]
  ;check if player has less than 7 minions on the board
  (when-not (< (count (get-minions state player-id)) 7)
    (error "The board is full."))
  (let [card (get-card state card-id)]
    (-> state
        (remove-card-from-hand player-id card-id)
        (add-minion-to-board player-id (create-minion (:name card)) position)
        (update-mana player-id (fn [old-value] (- old-value (get-mana-cost state card-id))))
        (do-on-play player-id card)
        )))

(defn kill-minion-fn
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Ronja" :id "r")]}])
                    (kill-minion-fn "r")
                    (get-minion "r"))
                nil))}
  [state minion-id]
  (let [minion-health (get-health state minion-id)]
    (deal-damage state minion-id minion-health)))

(defn attack-minion
  "Attacks the enemy minion"
  {:test (fn []
           ; Your minion's health should be updated
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}
                                  {:minions [(create-minion "Ronja" :id "r")]}])
                    (attack-minion "p1" "e" "r")            ;)
                    (get-health "e"))
                2)
           ; Target minion's health should be updated
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Ronja" :id "r")]}])
                    (attack-minion "p1" "m" "r")
                    (get-health "r"))
                1)
           ; Attack from poisonous attacker should kill the target minion
           (is= (-> (create-game [{:minions [(create-minion "Herr Nilsson" :id "hn")]}
                                  {:minions [(create-minion "Ronja" :id "r")]}])
                    (attack-minion "p1" "hn" "r")
                    (get-minion "r"))
                nil)
           ; Your minion's attacks for this turn should be updated
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}
                                  {:minions [(create-minion "Ronja" :id "r")]}])
                    (attack-minion "p1" "e" "r")
                    (get-minion "e")
                    (:attacks-performed-this-turn))
                1))}
  [state player-id attacker-id target-id]
  (let [attacker-attack-val (get-attack state attacker-id)
        target-attack-val (get-attack state target-id)]
    (if (valid-attack? state player-id attacker-id target-id) (as-> (update-minion state attacker-id :attacks-performed-this-turn inc) $
                                                                    ;if attacker is poisonous, kill target minion
                                                                    (if (is-poisonous? $ attacker-id)
                                                                      (kill-minion-fn $ target-id)
                                                                      (deal-damage $ target-id attacker-attack-val))
                                                                    (deal-damage $ attacker-id target-attack-val)))))

(defn attack-hero
  "Attacks the enemy hero"
  {:test (fn []
           ; The enemy hero's health should be updated
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (attack-hero "p1" "m" "h2")
                    (get-health "h2"))
                29)
           ; Your minion's attacks for this turn should be updated
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (attack-hero "p1" "e" "h2")
                    (get-minion "e")
                    (:attacks-performed-this-turn))
                1))}
  [state player-id attacker-id target-hero-id]
  (let [attacker-attack-val (get-attack state attacker-id)]
    (when (valid-attack? state player-id attacker-id target-hero-id)
      (-> (deal-damage state target-hero-id attacker-attack-val)
          (update-minion attacker-id :attacks-performed-this-turn inc)))))


(defn play-spell-card
  "Plays a spell card, removes it from hand"
  {:test (fn []
           ; The card should be erased from hand
           (is= (-> (create-game [{:hand    [(create-card "Radar Raid" :id "r")
                                             (create-card "Emil" :id "e")]
                                   :minions [(create-minion "Alfred" :id "a")]}])
                    (play-spell-card "p1" "r" "a")
                    (get-hand "p1")
                    (count))
                1)
           ; mana should be updated
           (is= (-> (create-game [{:hand    [(create-card "Radar Raid" :id "r")
                                             (create-card "Emil" :id "e")]
                                   :minions [(create-minion "Alfred" :id "a")]}])
                    (play-spell-card "p1" "r" "a")
                    (get-mana "p1"))
                8)
           ; Testing Insect Swarm
           (is= (-> (create-game [{:hand [(create-card "Insect Swarm" :id "i")] :deck [(create-card "Mio" :id "m")]}
                                  {:hand [(create-card "Emil" :id "e")] :minions [(create-minion "Alfred" :id "a")]}
                                  {:hero (create-hero "Carl" :id "h1")}])
                    (play-spell-card "p1" "i")
                    (get-hero "h1")
                    (:damage-taken))
                2)
           ; Testing Radar Raid
           (is= (-> (create-game [{:hand [(create-card "Radar Raid" :id "r")] :deck [(create-card "Mio" :id "m")]}
                                  {:hand [(create-card "Emil" :id "e")] :minions [(create-minion "Alfred" :id "a")]}])
                    (play-spell-card "p1" "r" "a")
                    (get-minion "a")
                    (:damage-taken))
                3))}
  ([state player-id card-id character-id]
   (-> ((:spell-fn (get-definition (get-card state card-id))) state character-id)
       (pay-mana player-id card-id)
       (remove-card-from-hand player-id card-id)))
  ([state player-id card-id]
   (-> ((:spell-fn (get-definition (get-card state card-id))) state)
       (pay-mana player-id card-id)
       (remove-card-from-hand player-id card-id))))

(defn do-hero-power
  {:test (fn []
           ;check Carl's blessing
           (is= (-> (create-game [{:minions [(create-minion "Kato" :id "k")]
                                   :hero    (create-hero "Carl")}])
                    (do-hero-power "p1" :target-id "k")
                    (get-minion "k")
                    (get-in [:properties :permanent])
                    (contains? "Divine Shield"))
                true)
           ;check mana is decreased
           (is= (-> (create-game [{:minions [(create-minion "Kato" :id "k")]
                                   :hero    (create-hero "Carl")}])
                    (do-hero-power "p1" :target-id "k")
                    (get-mana "p1"))
                8)
           ;check Gustaf's strengthen
           (as-> (create-game [{:minions [(create-minion "Emil" :id "e1")
                                          (create-minion "Emil" :id "e2")]
                                :hero    (create-hero "Gustaf")}]) $
                 (do-hero-power $ "p1")
                 (do (is= (->> (get-minions $ "p1")
                               (map :damage-taken))
                          [1, 1])
                     (is= (->> (get-minions $ "p1")
                               (map :attack))
                          [4, 4])))
           ;<2 minions available
           (as-> (create-game [{:minions [(create-minion "Emil" :id "e1")]
                                :hero    (create-hero "Gustaf")}]) $
                 (do-hero-power $ "p1")
                 (do (is= (->> (get-minions $ "p1")
                               (map :damage-taken))
                          [1])
                     (is= (->> (get-minions $ "p1")
                               (map :attack))
                          [4])))
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
                     (is= (->> (get-minions $ "p1")
                               (map :attack))
                          [4, 2, 4, 2])))
           ;can't perform twice in a turn
           (error? (-> (create-game [{:minions [(create-minion "Kato" :id "k")]
                                      :hero    (create-hero "Carl")}])
                       (do-hero-power "p1" :target-id "k")
                       (do-hero-power "p1" :target-id "k")))
           ;can perform in subsequent in a turn
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]
                                   :hero    (create-hero "Gustaf")}])
                    (do-hero-power "p1")
                    (end-turn "p1")
                    (end-turn "p2")
                    (do-hero-power "p1")
                    (get-minion "e")
                    (:damage-taken))
                2)
           )}
  ;target-id is needed for Carl's blessing but not Gustaf's strengthen
  ([state player-id & {:keys [target-id]}]
   (if (get-in state [:players player-id :hero :hero-power-used])
     (error "Hero power already used this turn")
     (let [hero-power (:hero-power (get-definition (get-in state [:players player-id :hero])))
           power-function (:power-fn (get-definition hero-power))]
       (as-> state $
             (update-mana $ player-id (fn [old-value] (- old-value (get-mana-cost state (get-in state [:players player-id :hero :id])))))
             (assoc-in $ [:players player-id :hero :hero-power-used] true)
             (if (empty? target-id)
               (power-function $ player-id)
               (give-divine-shield $ target-id))
             )))))

