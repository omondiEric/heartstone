(ns firestone.core-api
  (:require [ysera.test :refer [is is= error?]]
            [ysera.error :refer [error]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [add-card-to-hand
                                         add-minion-to-board
                                         change-player-in-turn
                                         create-card
                                         create-empty-state
                                         create-game
                                         create-minion
                                         draw-card-to-hand
                                         fatigue-hero
                                         inc-max-mana
                                         get-card
                                         get-deck
                                         get-hand
                                         get-mana
                                         get-max-mana
                                         get-minion
                                         get-minions
                                         get-mana
                                         get-mana-cost
                                         get-player-id-in-turn
                                         get-players
                                         get-other-player-id
                                         remove-card-from-deck
                                         remove-card-from-hand
                                         restore-mana
                                         update-mana
                                         update-max-mana
                                         remove-card-from-hand
                                         update-mana]]))

; call all the corresponding functions of all your minions at the end of your turn, return new state
(defn end-turn-card-functions
  {:test (fn []
           )}
  [state player-id]
  (let [minions (get-minions state player-id)]
    (reduce
      (fn [minion] ((:end-of-turn minion) state)) minions)))

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

  (let [other-player (get-other-player-id player-id)]
    (-> state
        (end-turn-card-functions player-id)
        (change-player-in-turn)
        (inc-max-mana other-player)
        (restore-mana other-player))))

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

(defn do-battlecry-fn
  "Returns the battlecry function of a minion or nil"
  {:test (fn []
           ;check that damage is taken
           (is= (-> (create-game [{:hand [(create-card "Kato" :id "k")]}])
                    (do-battlecry-fn "p2" (create-card "Kato"))
                    (do-battlecry-fn "p1" (create-card "Kato"))
                    (do-battlecry-fn "p1" (create-card "Kato"))
                    (do-battlecry-fn "p1" (create-card "Kato"))
                    (get-in [:players "p1" :hero :damage-taken]))
                12)
           ;check that card is drawn
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")] :deck [(create-card "Mio" :id "m")]}])
                    (do-battlecry-fn "p1" "p2" (create-card "Emil"))
                    (get-hand "p1")
                    (count))
                2)
           )}
  [state target-id card]
  ((:battlecry (get-definition card)) state target-id))
;(let [battlecry (:battlecry (get-definition card))]
;(battlecry state target-id)))
;(cond (= name "Kato")
;
;      (= name "Emil")
;      ;state)))
;        (draw-card state player-id))))

(let [battlecry #(:battlecry (get-definition (create-card "Kato" :id "k")))])

(defn play-minion-card
  {:test (fn []
           ; A minion should appear at the board
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (play-minion-card "p1" "e" 0)
                    (get-minions "p1")
                    (first)
                    (:name))
                "Emil")
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
  (let [card (get-card state card-id)
        other-player-id (get-other-player-id player-id)]
    (-> state
        (remove-card-from-hand player-id card-id)
        (add-minion-to-board player-id (create-minion (:name card)) position)
        (update-mana player-id (fn [old-value] (- old-value (get-mana-cost state card-id))))
        (do-battlecry-fn player-id other-player-id card)
        )))

(defn play-spell-card
  "Plays a spell card, removes it from hand"
  {:test (fn []
           ; Testing Insect Swarm

           ; Testing Radar Raid
           (is= (as-> (create-game [{:hand [(create-card "Radar Raid" :id "r")] :deck [(create-card "Mio" :id "m")]}
                                  {:hand [(create-card "Emil" :id "e")] :minions [(create-minion "Alfred" :id "a")]}]) $
                      ((:spell-fn (get-definition (get-card $ "r"))) $ "a") $
                      ;(play-spell-card "p1" "r" "a")
                      (get-minion $ "a")
                      (:damage-taken))
                3)
           ; The card should be erased from hand
           ;(is (-> (create-game [{:hand [(create-card "Radar Raid" :id "r")
           ;                              (create-card "Emil" :id "e")]}])
           ;        (play-spell-card "p1" "r" "e")
           ;        (get-hand "p1")
           ;        (empty?)))
           ;;
           ;(is= (-> (create-game [{:hand [(create-card "Radar Raid" :id "r")
           ;                               (create-card "Insect Swarm" :id "i")
           ;                               (create-card "Emil" :id "e")]}])
           ;         (play-spell-card "p1" "r" "e")
           ;         (get-hand "p1")
           ;         (count))
           ;     1)
           )}
  ; Plays spell card (not reliant on any character)
  ([state player-id card-id character-id]
   ((:spell-fn (get-definition (get-card state card-id))) state character-id)))
             ;(remove-card-from-hand player-id card-id)))
   ; call the spell card's function
  ; (let [card (get-card state card-id)]
  ;   (-> ((:spell-fn (get-definition card)) state)
  ;       (remove-card-from-hand player-id card-id))))
  ;; Plays spell card on specific character
  ;([state player-id card-id character-id]
  ; (let [card (get-card state card-id)]
  ;   (-> ((:spell-fn (get-definition card)) state character-id)
  ;       (remove-card-from-hand player-id card-id)))))
