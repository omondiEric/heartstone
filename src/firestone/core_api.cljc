(ns firestone.core-api
  (:require [ysera.test :refer [is is= error?]]
            [ysera.error :refer [error]]
            [firestone.construct :refer [add-minion-to-board
                                         create-card
                                         create-empty-state
                                         create-game
                                         create-minion
                                         get-card
                                         get-hand
                                         get-minions
                                         get-mana
                                         get-mana-cost
                                         get-player-id-in-turn
                                         get-players
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
                       (end-turn "p2"))))}
  [state player-id]
  (when-not (= (get-player-id-in-turn state) player-id)
    (error "The player with id " player-id " is not in turn."))
  (let [player-change-fn {"p1" "p2"
                          "p2" "p1"}]
    (update state :player-id-in-turn player-change-fn)))

(defn perform-battlecry
  {:test (fn []
           (is= (-> (create-card "Kato")
                    (perform-battlecry "p1" "Kato")
                    (get-in [:players "p1" :hero :damage-taken]))
                4))}

  [state player-id card]
  (let [battlecry-fn (get-battlecry-fn card)]

    (when battlecry-fn
      ;apply battlecry function
      (battlecry-fn state player-id))))

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
