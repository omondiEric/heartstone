(ns firestone.core-api
  (:require [ysera.test :refer [is is= error?]]
            [ysera.error :refer [error]]
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
                                         get-minions
                                         get-mana
                                         get-mana-cost
                                         get-player-id-in-turn
                                         get-players
                                         remove-card-from-deck
                                         remove-card-from-hand
                                         restore-mana
                                         update-mana
                                         update-max-mana
                                         remove-card-from-hand
                                         update-mana]]
            [firestone.core :refer [get-battlecry-fn]]))


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

  ;could be improved because it is a bit repetitive
  (let [player-change-fn {"p1" "p2"
                          "p2" "p1"}]
    (-> state
        (change-player-in-turn)
        (inc-max-mana (player-change-fn player-id))
        (restore-mana (player-change-fn player-id)))))


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
                   (empty?))))}
  [state player-id card-id position]
  ;check if player has less than 7 minions on the board
  (when-not (< (count (get-minions state player-id)) 7)
    (error "The board is full."))
  (let [card (get-card state card-id)]
    (-> state
        (remove-card-from-hand player-id card-id)
        (add-minion-to-board player-id (create-minion (:name card)) position)
        (update-mana player-id (fn [old-value] (- old-value (get-mana-cost state card-id))))
        ;(perform-battlecry player-id card)
        )))

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

