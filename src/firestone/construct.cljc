(ns firestone.construct
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.error :refer [error]]
            [ysera.random :as random]
            [firestone.definitions :refer [get-definition]]))

(defn create-hero
  "Creates a hero from its definition by the given hero name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-hero "Carl")
                {:name            "Carl"
                 :entity-type     :hero
                 :damage-taken    0
                 :hero-power-used false})
           (is= (create-hero "Carl" :damage-taken 10)
                {:name            "Carl"
                 :entity-type     :hero
                 :damage-taken    10
                 :hero-power-used false}))}
  [name & kvs]
  (let [hero {:name            name
              :entity-type     :hero
              :damage-taken    0
              :hero-power-used false}]
    (if (empty? kvs)
      hero
      (apply assoc hero kvs))))

(defn create-card
  "Creates a card from its definition by the given card name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-card "Mio" :id "m")
                {:id          "m"
                 :entity-type :card
                 :name        "Mio"}))}
  [name & kvs]
  (let [card {:name        name
              :entity-type :card}]
    (if (empty? kvs)
      card
      (apply assoc card kvs))))

(def game-event-fn-names #{:end-of-turn, :on-minion-damage})

(defn get-additional-minion-field
  {:test (fn []
           (is-not (nil? (get-additional-minion-field "Pippi" :end-of-turn)))
           (is (nil? (get-additional-minion-field "Mio" :end-of-turn))))}
  [minion-name game-event-key]
  (let [game-event-from-defn (if (contains? (get-definition minion-name) game-event-key)
                       (-> (get-definition minion-name) (game-event-key))
                       nil)]
    (when game-event-from-defn {game-event-key game-event-from-defn})))

(defn get-all-additional-minion-fields
  {:test (fn []
           (is-not (empty? (get-all-additional-minion-fields "Pippi")))
           (is (empty? (get-all-additional-minion-fields "Mio"))))}
  [minion-name]
  (reduce (fn [curr-map game-event-key]
            (merge curr-map (get-additional-minion-field minion-name game-event-key)))
          {}
          game-event-fn-names))

(defn create-minion
  "Creates a minion from its definition by the given minion name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-minion "Mio" :id "m" :attacks-performed-this-turn 1)
                {:attacks-performed-this-turn 1
                 :damage-taken                0
                 :attack                      1
                 :entity-type                 :minion
                 :properties                  {:permanent     #{}
                                               :temporary     #{}}
                 :name                        "Mio"
                 :id                          "m"})
           (is= (create-minion "Elisabeth" :id "e")
                {:attacks-performed-this-turn 0
                 :damage-taken                0
                 :attack                      1
                 :entity-type                 :minion
                 :properties                  {:permanent     #{"Divine Shield", "Taunt"}
                                               :temporary     #{}}
                 :name                        "Elisabeth"
                 :id                          "e"})
           (is= (create-minion "Ida" :id "i")
                {:attacks-performed-this-turn 0
                 :damage-taken                0
                 :attack                      2
                 :entity-type                 :minion
                 :properties                  {:permanent     #{}
                                               :temporary     #{}}
                 :name                        "Ida"
                 :id                          "i"
                 :on-minion-damage            (:on-minion-damage (get-definition "Ida"))})
           )}
  [name & kvs]
  (let [properties (-> (get-definition name) (:properties)) ; Will be used later
        attack (-> (get-definition name) (:attack))
        minion  (merge
                {:damage-taken                0
                :attack                      attack
                :properties                  properties
                :entity-type                 :minion
                :name                        name
                :attacks-performed-this-turn 0}
                (get-all-additional-minion-fields name)
                )]
    (if (empty? kvs)
      minion
      (apply assoc minion kvs))))

(defn create-empty-state
  "Creates an empty state with the given heroes."
  {:test (fn []
           (is= (create-empty-state [(create-hero "Carl")
                                     (create-hero "Carl")])
                (create-empty-state))

           (is= (create-empty-state [(create-hero "Carl" :id "c")
                                     (create-hero "Gustaf")])
                {:player-id-in-turn             "p1"
                 :players                       {"p1" {:id            "p1"
                                                       :mana          10
                                                       :max-mana      10
                                                       :deck          []
                                                       :hand          []
                                                       :minions       []
                                                       :fatigue-level 0
                                                       :hero          {:name            "Carl"
                                                                       :id              "c"
                                                                       :owner-id        "p1"
                                                                       :damage-taken    0
                                                                       :hero-power-used false
                                                                       :entity-type     :hero}}
                                                 "p2" {:id            "p2"
                                                       :mana          10
                                                       :max-mana      10
                                                       :deck          []
                                                       :hand          []
                                                       :minions       []
                                                       :fatigue-level 0
                                                       :hero          {:name            "Gustaf"
                                                                       :id              "h2"
                                                                       :owner-id        "p2"
                                                                       :damage-taken    0
                                                                       :hero-power-used false
                                                                       :entity-type     :hero}}}
                 :counter                       1
                 :seed                          0
                 :minion-ids-summoned-this-turn []}))}
  ([heroes]
   ; Creates Carl heroes if heroes are missing.
   (let [heroes (->> (concat heroes [(create-hero "Carl")
                                     (create-hero "Carl")])
                     (take 2))]
     {:player-id-in-turn             "p1"
      :players                       (->> heroes
                                          (map-indexed (fn [index hero]
                                                         {:id            (str "p" (inc index))
                                                          :mana          10
                                                          :max-mana      10
                                                          :deck          []
                                                          :hand          []
                                                          :minions       []
                                                          :fatigue-level 0
                                                          :hero          (if (contains? hero :id)
                                                                           (assoc hero :owner-id (str "p" (inc index)))
                                                                           (assoc hero :id (str "h" (inc index))
                                                                                       :owner-id (str "p" (inc index))))}))
                                          (reduce (fn [a v]
                                                    (assoc a (:id v) v))
                                                  {}))
      :counter                       1
      :seed                          0
      :minion-ids-summoned-this-turn []}))
  ([]
   (create-empty-state [])))

(defn update-seed
  {:test (fn []
           (is= (-> (create-empty-state)
                    (update-seed 20)
                    (:seed))
                20)
           )}
  [state seed]
  (assoc-in state [:seed] seed))

(defn get-player
  "Returns the player with the given id."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-player "p1")
                    (:id))
                "p1"))}
  [state player-id]
  (get-in state [:players player-id]))

(defn get-player-id-in-turn
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-player-id-in-turn))
                "p1"))}
  [state]
  (:player-id-in-turn state))

(defn get-other-player-id
  {:test (fn []
           (is= (get-other-player-id "p1")
                "p2")
           (is= (get-other-player-id "p2")
                "p1"))}
  [player-id]
  (if (= player-id "p1") "p2" "p1")
  )

(defn get-minions
  "Returns the minions on the board for the given player-id or for both players."
  {:test (fn []
           ; Getting minions is also tested in add-minion-to-board.
           (is= (-> (create-empty-state)
                    (get-minions "p1"))
                [])
           (is= (-> (create-empty-state)
                    (get-minions))
                []))}
  ([state player-id]
   (:minions (get-player state player-id)))
  ([state]
   (->> (:players state)
        (vals)
        (map :minions)
        (apply concat))))

(defn get-deck
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-deck "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :deck]))

(defn get-hand
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-hand "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :hand]))

(defn- generate-id
  "Generates an id and returns a tuple with the new state and the generated id."
  {:test (fn []
           (is= (generate-id {:counter 6})
                [{:counter 7} 6]))}
  [state]
  {:pre [(contains? state :counter)]}
  [(update state :counter inc) (:counter state)])

(defn- generate-time-id
  "Generates a number and returns a tuple with the new state and the generated number."
  {:test (fn []
           (is= (generate-id {:counter 6})
                [{:counter 7} 6]))}
  [state]
  {:pre [(contains? state :counter)]}
  [(update state :counter inc) (:counter state)])

(defn add-minion-to-board
  "Adds a minion with a given position to a player's minions and updates the other minions' positions."
  {:test (fn []
           ; Adding a minion to an empty board
           (is= (as-> (create-empty-state) $
                      (add-minion-to-board $ "p1" (create-minion "Mio" :id "m") 0)
                      (get-minions $ "p1")
                      (map (fn [m] {:id (:id m) :name (:name m)}) $))
                [{:id "m" :name "Mio"}])
           ; Adding a minion and update positions
           (let [minions (-> (create-empty-state)
                             (add-minion-to-board "p1" (create-minion "Elisabeth" :id "m1") 0)
                             (add-minion-to-board "p1" (create-minion "Mio" :id "m2") 0)
                             (add-minion-to-board "p1" (create-minion "Mio" :id "m3") 1)
                             (get-minions "p1"))]
             (is= (map :id minions) ["m1" "m2" "m3"])
             (is= (map :position minions) [2 0 1]))
           ; Generating an id for the new minion
           (let [state (-> (create-empty-state)
                           (add-minion-to-board "p1" (create-minion "Mio") 0))]
             (is= (-> (get-minions state "p1")
                      (first)
                      (:id))
                  "m1")
             (is= (:counter state) 3)))}
  [state player-id minion position]
  {:pre [(map? state) (string? player-id) (map? minion) (number? position)]}
  (let [[state id] (if (contains? minion :id)
                     [state (:id minion)]
                     (let [[state value] (generate-id state)]
                       [state (str "m" value)]))
        [state time-id] (generate-time-id state)
        ready-minion (assoc minion :position position
                                   :owner-id player-id
                                   :id id
                                   :added-to-board-time-id time-id)]
    (update-in state
               [:players player-id :minions]
               (fn [minions]
                 (conj (->> minions
                            (mapv (fn [m]
                                    (if (< (:position m) position)
                                      m
                                      (update m :position inc)))))
                       ready-minion)))))

(defn add-minions-to-board
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-minions-to-board $ "p1" [(create-minion "Mio")
                                                    (create-minion "Ronja")
                                                    (create-minion "Kato")])
                      (get-minions $ "p1")
                      (map :name $))
                ["Mio" "Ronja" "Kato"]))}
  [state player-id minions]
  (->> minions
       (reduce-kv (fn [state index minion]
                    (add-minion-to-board state
                                         player-id
                                         (if (string? minion)
                                           (create-minion minion)
                                           minion)
                                         index))
                  state)))

(defn- add-card-to
  "Adds a card to either the hand or the deck."
  {:test (fn []
           ; Adding cards to deck
           (is= (as-> (create-empty-state) $
                      (add-card-to $ "p1" "Mio" :deck)
                      (add-card-to $ "p1" "Kato" :deck)
                      (get-deck $ "p1")
                      (map :name $))
                ["Mio" "Kato"])
           ; Adding cards to hand
           (is= (as-> (create-empty-state) $
                      (add-card-to $ "p1" "Mio" :hand)
                      (add-card-to $ "p1" "Kato" :hand)
                      (get-hand $ "p1")
                      (map :name $))
                ["Mio" "Kato"]))}
  [state player-id card-or-name place]
  (let [card (if (string? card-or-name)
               (create-card card-or-name)
               card-or-name)
        [state id] (if (contains? card :id)
                     [state (:id card)]
                     (let [[state value] (generate-id state)]
                       [state (str "c" value)]))
        ready-card (assoc card :owner-id player-id
                               :id id)]
    (update-in state [:players player-id place] conj ready-card)))

(defn add-card-to-deck
  [state player-id card]
  (add-card-to state player-id card :deck))

(defn add-card-to-hand
  [state player-id card]
  (add-card-to state player-id card :hand))

(defn add-cards-to-deck
  [state player-id cards]
  (reduce (fn [state card]
            (add-card-to-deck state player-id card))
          state
          cards))

(defn add-cards-to-hand
  [state player-id cards]
  (reduce (fn [state card]
            (add-card-to-hand state player-id card))
          state
          cards))

(defn get-mana
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-mana "p1"))
                10))}
  [state player-id]
  (get-in state [:players player-id :mana]))

(defn update-mana
  {:test (fn []
           (is= (-> (create-empty-state)
                    (update-mana "p1" 4)
                    (get-mana "p1"))
                4)
           (is= (-> (create-empty-state)
                    (update-mana "p1" dec)
                    (get-mana "p1"))
                9))}
  [state player-id fn-or-value]
  (if (fn? fn-or-value)
    (update-in state [:players player-id :mana] fn-or-value)
    (assoc-in state [:players player-id :mana] fn-or-value)))

(defn get-max-mana
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-max-mana "p1"))
                10))}
  [state player-id]
  (get-in state [:players player-id :max-mana]))

(defn update-max-mana
  {:test (fn []
           (is= (-> (create-empty-state)
                    (update-max-mana "p1" 4)
                    (get-max-mana "p1"))
                4)
           (is= (-> (create-empty-state)
                    (update-max-mana "p1" dec)
                    (get-max-mana "p1"))
                9))}
  [state player-id fn-or-value]
  (if (fn? fn-or-value)
    (update-in state [:players player-id :max-mana] fn-or-value)
    (assoc-in state [:players player-id :max-mana] fn-or-value)))


(defn create-game
  "Creates a game with the given deck, hand, minions (placed on the board), and heroes."
  {:test (fn []
           (is= (create-game) (create-empty-state))

           (is= (create-game [{:hero (create-hero "Carl")}])
                (create-game [{:hero "Carl"}]))

           (is= (create-game [{:minions [(create-minion "Mio")]}])
                (create-game [{:minions ["Mio"]}]))

           (is= (create-game [{:minions ["Mio"]
                               :deck    ["Ronja"]
                               :hand    ["Emil"]
                               :mana    3}
                              {:hero "Carl"}]
                             :player-id-in-turn "p2")
                {:player-id-in-turn             "p2"
                 :players                       {"p1" {:id            "p1"
                                                       :mana          3
                                                       :max-mana      3
                                                       :deck          [{:entity-type :card
                                                                        :id          "c3"
                                                                        :name        "Ronja"
                                                                        :owner-id    "p1"}]
                                                       :hand          [{:entity-type :card
                                                                        :id          "c4"
                                                                        :name        "Emil"
                                                                        :owner-id    "p1"}]
                                                       :minions       [{:damage-taken                0
                                                                        :attack                      1
                                                                        :attacks-performed-this-turn 0
                                                                        :added-to-board-time-id      2
                                                                        :entity-type                 :minion
                                                                        :properties                  {:permanent     #{}
                                                                                                      :temporary     #{}}
                                                                        :name                        "Mio"
                                                                        :id                          "m1"
                                                                        :position                    0
                                                                        :owner-id                    "p1"}]
                                                       :fatigue-level 0
                                                       :hero          {:name            "Carl"
                                                                       :id              "h1"
                                                                       :owner-id        "p1"
                                                                       :entity-type     :hero
                                                                       :damage-taken    0
                                                                       :hero-power-used false}}
                                                 "p2" {:id            "p2"
                                                       :mana          10
                                                       :max-mana      10
                                                       :deck          []
                                                       :hand          []
                                                       :minions       []
                                                       :fatigue-level 0
                                                       :hero          {:name            "Carl"
                                                                       :id              "h2"
                                                                       :owner-id        "p2"
                                                                       :entity-type     :hero
                                                                       :damage-taken    0
                                                                       :hero-power-used false}}}
                 :counter                       5
                 :seed                          0
                 :minion-ids-summoned-this-turn []}))}
  ([data & kvs]
   (let [players-data (map-indexed (fn [index player-data]
                                     (assoc player-data :player-id (str "p" (inc index))))
                                   data)
         state (as-> (create-empty-state (map (fn [player-data]
                                                (cond (nil? (:hero player-data))
                                                      (create-hero "Carl")

                                                      (string? (:hero player-data))
                                                      (create-hero (:hero player-data))

                                                      :else
                                                      (:hero player-data)))
                                              data)) $
                     (reduce (fn [state {player-id :player-id
                                         mana      :mana
                                         minions   :minions
                                         deck      :deck
                                         hand      :hand}]
                               (-> (if mana
                                     (-> state
                                         (update-mana player-id mana)
                                         (update-max-mana player-id mana))
                                     state)
                                   (add-minions-to-board player-id minions)
                                   (add-cards-to-deck player-id deck)
                                   (add-cards-to-hand player-id hand)))
                             $
                             players-data))]
     (if (empty? kvs)
       state
       (apply assoc state kvs))))
  ([]
   (create-game [])))

(defn get-minion
  "Returns the minion with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-minion "m")
                    (:name))
                "Mio"))}
  [state id]
  (->> (get-minions state)
       (filter (fn [m] (= (:id m) id)))
       (first)))

(defn get-players
  {:test (fn []
           (is= (->> (create-game)
                     (get-players)
                     (map :id))
                ["p1" "p2"]))}
  [state]
  (->> (:players state)
       (vals)))

(defn get-heroes
  {:test (fn []
           (is= (->> (create-game)
                     (get-heroes)
                     (map :name))
                ["Carl" "Carl"]))}
  [state]
  (->> (get-players state)
       (map :hero)))

(defn replace-minion
  "Replaces a minion with the same id as the given new-minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (replace-minion (create-minion "Ronja" :id "m"))
                    (get-minion "m")
                    (:name))
                "Ronja"))}
  [state new-minion]
  (let [owner-id (or (:owner-id new-minion)
                     (:owner-id (get-minion state (:id new-minion))))]
    (update-in state
               [:players owner-id :minions]
               (fn [minions]
                 (map (fn [m]
                        (if (= (:id m) (:id new-minion))
                          new-minion
                          m))
                      minions)))))

(defn update-minion
  "Updates the value of the given key for the minion with the given id. If function-or-value is a value it will be the
   new value, else if it is a function it will be applied on the existing value to produce the new value."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (update-minion "m" :damage-taken inc)
                    (get-minion "m")
                    (:damage-taken))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (update-minion "m" :damage-taken 2)
                    (get-minion "m")
                    (:damage-taken))
                2))}
  [state id key function-or-value]
  (let [minion (get-minion state id)]
    (replace-minion state (if (fn? function-or-value)
                            (update minion key function-or-value)
                            (assoc minion key function-or-value)))))

(defn remove-minion
  "Removes a minion with the given id from the state."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (remove-minion "m")
                    (get-minions))
                []))}
  [state id]
  (let [owner-id (:owner-id (get-minion state id))]
    (update-in state
               [:players owner-id :minions]
               (fn [minions]
                 (remove (fn [m] (= (:id m) id)) minions)))))

(defn remove-minions
  "Removes the minions with the given ids from the state."
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                               (create-minion "Mio" :id "m2")]}
                                    {:minions [(create-minion "Mio" :id "m3")
                                               (create-minion "Mio" :id "m4")]}]) $
                      (remove-minions $ "m1" "m4")
                      (get-minions $)
                      (map :id $))
                ["m2" "m3"])
           )}
  [state & ids]
  (reduce remove-minion state ids)
  )


(defn switch-minion-side
  "Switches a minion from one player to the other"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                             (create-minion "Mio" :id "m2")]}
                                  {:minions [(create-minion "Mio" :id "m3")
                                             (create-minion "Mio" :id "m4")]}])
                    (switch-minion-side "m1")
                    (get-minions "p1")
                    (count))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                             (create-minion "Mio" :id "m2")]}
                                  {:minions [(create-minion "Mio" :id "m3")
                                             (create-minion "Mio" :id "m4")]}])
                    (switch-minion-side "m4")
                    (get-minions "p1")
                    (count))
                3)
           )}
  [state minion-id]
  (let [minion (get-minion state minion-id)
        player-id (get-other-player-id (minion :owner-id))]
    (-> state
        (remove-minion minion-id)
        (add-minion-to-board player-id minion 0))))


(defn get-cards
  ([state]
   (let [player-ids (->> (get-players state)
                         (map :id))]
     (apply concat (map (fn [player-id] (get-cards state player-id)) player-ids))))
  ([state player-id]
   (concat (get-hand state player-id)
           (get-deck state player-id))))

(defn get-card
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (get-card "e")
                    (:name))
                "Emil"))}
  [state card-id]
  (->> (get-cards state)
       (filter (fn [c] (= (:id c) card-id)))
       (first)))

(defn remove-card-from-hand
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")
                                          "Mio"
                                          "Ronja"]}])
                    (remove-card-from-hand "p1" "e")
                    (get-hand "p1")
                    (count))
                2))}
  [state player-id card-id]
  (update-in state [:players player-id :hand]
             (fn [cards]
               (->> cards
                    (remove (fn [c] (= (:id c) card-id)))))))

(defn get-hero
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-hero "h1")
                    (:id))
                "h1"))}
  [state hero-id]
  (->> (get-players state)
       (map :hero)
       (filter (fn [h] (= (:id h) hero-id)))
       (first)))

(defn card?
  {:test (fn []
           (is (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                   (card? "e")))
           (is-not (-> (create-empty-state)
                       (card? "hi")))
           )}
  [state id]
  (not= (get-card state id) nil))

(defn get-mana-cost
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (get-mana-cost "e"))
                4)
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (get-mana-cost "h1"))
                2))}
  [state id]
  (if (card? state id)
    (let [card (get-card state id)
          definition (get-definition card)]
      (:mana-cost definition))
    (let [hero (get-hero state id)
          definition (get-definition hero)]
      (:mana-cost (get-definition (:hero-power definition))))))

(defn remove-card-from-deck
  {:test (fn []
           (is (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                   (remove-card-from-deck "p1" "e")
                   (get-deck "p1")
                   (empty?))))}
  [state player-id card-id]
  (update-in state [:players player-id :deck]
             (fn [cards]
               (->> cards
                    (remove (fn [c] (= (:id c) card-id)))))))

(defn change-player-in-turn
  {:test (fn []
           (is= (-> (create-game)
                    (change-player-in-turn)
                    (get-player-id-in-turn))
                "p2")
           (is= (-> (create-game)
                    (change-player-in-turn)
                    (change-player-in-turn)
                    (get-player-id-in-turn))
                "p1"))}
  [state]
  (let [other-player (get-other-player-id (state :player-id-in-turn))]
    (assoc state :player-id-in-turn other-player)))

(defn inc-max-mana
  {:test (fn []
           (is= (-> (create-game)
                    (inc-max-mana "p1")
                    (get-max-mana "p1"))
                10)
           (is= (-> (create-game)
                    (update-max-mana "p1" 5)
                    (inc-max-mana "p1")
                    (get-max-mana "p1"))
                6))}
  [state player-id]
  (if (< (get-max-mana state player-id) 10)
    (update-max-mana state player-id inc)
    state))

(defn restore-mana
  {:test (fn []
           (is= (-> (create-game)
                    (update-mana "p1" 5)
                    (restore-mana "p1")
                    (get-mana "p1"))
                10)
           (is= (-> (create-game)
                    (update-max-mana "p1" 5)
                    (update-mana "p1" 0)
                    (restore-mana "p1")
                    (get-mana "p1"))
                5))}
  [state player-id]
  (assoc-in state [:players player-id :mana] (get-max-mana state player-id)))

(defn draw-card-to-hand
  {:test (fn []
           ;a card should appear in the hand
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                    (draw-card-to-hand "p1")
                    (get-hand "p1")
                    (first)
                    (:name))
                "Emil")
           ;draw cards consecutively
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")
                                          "Mio"]}])
                    (draw-card-to-hand "p1")
                    (draw-card-to-hand "p1")
                    (get-hand "p1")
                    (last)
                    (:name))
                "Mio")
           ;the card should be removed from the deck
           (is (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                   (draw-card-to-hand "p1")
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
                    (draw-card-to-hand "p1")
                    (get-hand "p1")
                    (count))
                10))}
  [state player-id]
  (if (> (count (get-in state [:players player-id :deck])) 0)
    (let [hand-size (count (get-hand state player-id))]
      (let [card-id (:id (first (get-deck state player-id)))]
        (if (< hand-size 10)
          (-> state
              (add-card-to-hand player-id (get-card state card-id))
              (remove-card-from-deck player-id card-id))
          ;burn card if over hand size limit
          (remove-card-from-deck state player-id card-id))))
    ;(error "Hand size limit reached")))
    state))

(defn fatigue-hero
  {:test (fn []
           (is= (-> (create-game)
                    (fatigue-hero "p1")
                    (get-in [:players "p1" :fatigue-level]))
                1)
           (is= (-> (create-game)
                    (fatigue-hero "p1")
                    (get-in [:players "p1" :hero :damage-taken]))
                1)
           (is= (-> (create-game)
                    (fatigue-hero "p1")
                    (fatigue-hero "p1")
                    (get-in [:players "p1" :fatigue-level]))
                2)
           (is= (-> (create-game)
                    (fatigue-hero "p1")
                    (fatigue-hero "p1")
                    (get-in [:players "p1" :hero :damage-taken]))
                3)
           ;only fatigues hero if deck is empty
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                    (fatigue-hero "p1")
                    (get-in [:players "p1" :hero :damage-taken]))
                0)
           )}
  [state player-id]
  (if (= (count (get-in state [:players player-id :deck])) 0)
    (let [new-fatigue-level (inc (get-in state [:players player-id :fatigue-level]))]
      (-> state
          (assoc-in [:players player-id :fatigue-level] new-fatigue-level)
          (update-in [:players player-id :hero :damage-taken] (fn [x] (+ x new-fatigue-level)))))
    state))

(defn random-nth
  [state coll]
  (let [[new-seed random-element] (random/random-nth (:seed state) coll)]
    [(assoc state :seed new-seed) random-element]))

(defn shuffle-with-seed
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                               (create-minion "Mio" :id "m2")]}
                                    {:minions [(create-minion "Mio" :id "m3")
                                               (create-minion "Mio" :id "m4")]}]) $
                      (shuffle-with-seed $ (get-minions $))
                      (last $)
                      (map :id $))
                ["m1" "m3" "m2" "m4"])
           )}
  [state coll]
  (let [[new-seed shuffled-collection] (random/shuffle-with-seed (:seed state) coll)]
    [(assoc state :seed new-seed) shuffled-collection]))


(defn get-random-minion
  {:test (fn []
           ;get a random minion from all
           (as-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                          (create-minion "Mio" :id "m2")
                                          (create-minion "Emil" :id "e1")
                                          (create-minion "Emil" :id "e2")]}
                               {:minions [(create-minion "Mio" :id "m3")
                                          (create-minion "Mio" :id "m4")
                                          (create-minion "Emil" :id "e3")
                                          (create-minion "Emil" :id "e4")]}]) $
                 (get-random-minion $)
                 (do (is= (:id (last $)) "m1")
                     (is (not= (:seed (first $)) 0))))
           ;get a random minion from specific player
           (as-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                          (create-minion "Mio" :id "m2")
                                          (create-minion "Emil" :id "e1")
                                          (create-minion "Emil" :id "e2")]}
                               {:minions [(create-minion "Mio" :id "m3")
                                          (create-minion "Mio" :id "m4")
                                          (create-minion "Emil" :id "e3")
                                          (create-minion "Emil" :id "e4")]}]) $
                 (get-random-minion $ "p1")
                 (do (is= (:id (last $)) "m1")
                     (is (not= (:seed (first $)) 0))))
           (as-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                          (create-minion "Mio" :id "m2")
                                          (create-minion "Emil" :id "e1")
                                          (create-minion "Emil" :id "e2")]}
                               {:minions [(create-minion "Mio" :id "m3")
                                          (create-minion "Mio" :id "m4")
                                          (create-minion "Emil" :id "e3")
                                          (create-minion "Emil" :id "e4")]}]) $
                 (get-random-minion $ "p2")
                 (do (is= (:id (last $)) "m3")
                     (is (not= (:seed (first $)) 0))))
           )}
  ([state]
   (->> (get-minions state)
        (random-nth state)))
  ([state player-id]
   (->> (get-minions state player-id)
        (random-nth state))))

(defn get-random-minions-distinct
  {:test (fn []
           ;get a random minion from all
           (as-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                          (create-minion "Mio" :id "m2")]}
                               {:minions [(create-minion "Mio" :id "m3")
                                          (create-minion "Mio" :id "m4")]}]) $
                 (get-random-minions-distinct $ 2)
                 (do (is= (map :id $) ["m1", "m3"])
                     (is (not= (:seed (first $)) 0))))
           ;get a random minion from specific player
           (as-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                          (create-minion "Mio" :id "m2")]}
                               {:minions [(create-minion "Mio" :id "m3")
                                          (create-minion "Mio" :id "m4")]}]) $
                 (get-random-minions-distinct $ 2 "p1")
                 (do (is= (map :id $) ["m1", "m2"])
                     (is (not= (:seed (first $)) 0))))
           )
   }
  ([state number]
   (let [minions-collection (last (shuffle-with-seed state (get-minions state)))]
     (take number minions-collection)))
  ([state number player-id]
   (let [minions-collection (last (shuffle-with-seed state (get-minions state player-id)))]
     (take number minions-collection))))

(defn give-taunt
  "Gives taunt to a minion card"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (give-taunt "m")
                    (get-minion "m")
                    (get-in [:properties :permanent])
                    (contains? "Taunt"))
                true))}
  [state minion-id]
  (update-minion state minion-id :properties (fn [properties-map]
                                               (update-in properties-map [:permanent] (fn [permanent-set]
                                                                                        (conj permanent-set "Taunt"))))))

(defn remove-taunt
  "Removes taunt from a minion card"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (remove-taunt "e")
                    (get-minion "e")
                    (get-in [:properties :permanent])
                    (contains? "Taunt"))
                false))}
  [state minion-id]
  (update-minion state minion-id :properties (fn [properties-map]
                                               (update-in properties-map [:permanent] (fn [permanent-set]
                                                                                        (disj permanent-set "Taunt"))))))

(defn player-has-active-aura-buff?
  "Looks for an active minion of the current player that has a specific aura buff"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Tjorven" :id "t")
                                             (create-minion "Madicken" :id "m")]}])
                    (player-has-active-aura-buff? "p1" "Friendly-Windfury"))
                true)
           (is= (-> (create-game [{:minions [(create-minion "Astrid" :id "a")]}])
                    (player-has-active-aura-buff? "p1" "Friendly-Windfury"))
                nil)
           )}
  [state player-id aura-buff]
  (some true?
        (->> (get-minions state player-id)
             (map (fn [m]
                    (contains? (:aura (get-definition (:name m))) aura-buff))))))

(defn has-windfury?
  "Checks if minion has windfury, either permanent or received by aura"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Rasmus" :id "r")]}])
                    (has-windfury? "r" "p1"))
                true)
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (has-windfury? "e" "p1"))
                nil))}
  [state minion-id player-id]
  (let [permanent-set (get-in (get-minion state minion-id) [:properties :permanent])]
    (or (contains? permanent-set "Windfury")
        (player-has-active-aura-buff? state player-id "Friendly-Windfury"))))

(defn has-taunt?
  "Checks if minion has taunt"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (has-taunt? "e"))
                true)
           (is= (-> (create-game [{:minions [(create-minion "Kato" :id "k")]}])
                    (has-taunt? "k"))
                false))}
  [state minion-id]
  (let [permanent-set (get-in (get-minion state minion-id) [:properties :permanent])]
    (contains? permanent-set "Taunt")))

(defn is-poisonous?
  "Checks if minion has taunt"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Herr Nilsson" :id "hn")]}])
                    (is-poisonous? "hn"))
                true)
           (is= (-> (create-game [{:minions [(create-minion "Kato" :id "k")]}])
                    (is-poisonous? "k"))
                false))}
  [state minion-id]
  (let [permanent-set (get-in (get-minion state minion-id) [:properties :permanent])]
    (contains? permanent-set "Poisonous")))

;Gives divine shield to a card
(defn give-divine-shield
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Kato" :id "k")]}])
                    ;(get-minion "k"))
                    (give-divine-shield "k")
                    (get-minion "k")
                    (get-in [:properties :permanent])
                    (contains? "Divine Shield"))
                true))}
  [state minion-id]
  (update-minion state minion-id :properties (fn [properties-map]
                                               (update-in properties-map [:permanent] (fn [permanent-set]
                                                                                        (conj permanent-set "Divine Shield"))))))

;Remove divine shield from a minion
(defn remove-divine-shield
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Uncle Melker" :id "um")]}])
                    (remove-divine-shield "um")
                    (get-minion "um")
                    (get-in [:properties :permanent])
                    (contains? "Divine Shield"))
                false))}
  [state minion-id]
  (update-minion state minion-id :properties (fn [properties-map]
                                               (update-in properties-map [:permanent] (fn [permanent-set]
                                                                                        (disj permanent-set "Divine Shield"))))))
(defn has-divine-shield
  {:test (fn []
           ;return true when minion has divine shield
           (is= (-> (create-game [{:minions [(create-minion "Uncle Melker" :id "um")]}])
                    (has-divine-shield "um"))
                true)
           ;return false when minion has no divine shield
           (is= (-> (create-game [{:minions [(create-minion "Kato" :id "k")]}])
                    (has-divine-shield "k"))
                false))}

  [state minion-id]
  (let [permanent-set (get-in (get-minion state minion-id) [:properties :permanent])]
    (contains? permanent-set "Divine Shield")))

(defn get-character
  "Returns the character with the given id from the state."
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Carl" :id "h1")}])
                    (get-character "h1")
                    (:name))
                "Carl")
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-character "m")
                    (:name))
                "Mio"))}
  [state id]
  (or (some (fn [m] (when (= (:id m) id) m))
            (get-minions state))
      (some (fn [h] (when (= (:id h) id) h))
            (get-heroes state))))

(defn minion?
  {:test (fn []
           (is= ((create-game [{:hero (create-hero "Carl" :id "h1")}])
                 (minion? "Carl"))
                nil))}
  [character]
  (if (= (:entity-type character) :minion)
    true))

(defn ida-present?
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Ida" :id "i")]}])
                    (ida-present?)
                    (:name))
                "Ida")
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (ida-present?))
                nil))}
  [state]
  (some (fn [m] (when (= (:name m) "Ida") m))
        (get-minions state)))

(defn give-attack
  "Gives attack to a minion"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (give-attack "e" 3)
                    (get-minion "e")
                    (:attack))
                5))}
  [state minion-id attack-amount]
  (update-minion state minion-id :attack (fn [x] (+ x attack-amount))))