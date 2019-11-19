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

(def game-event-fn-names #{:end-of-turn, :on-minion-damage, :deathrattle :on-divine-shield-removal})

(defn get-additional-minion-field
  {:test (fn []
           (is= (get-additional-minion-field "Pippi" :end-of-turn)
                {:end-of-turn "Pippi"})
           (is-not (get-additional-minion-field "Mio" :end-of-turn)))}
  [minion-name game-event-key]
  (let [game-event-from-defn (if (contains? (get-definition minion-name) game-event-key)
                               minion-name
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
                 :entity-type                 :minion
                 :properties                  {:permanent #{}
                                               :temporary {}
                                               :stats     {}}
                 :name                        "Mio"
                 :id                          "m"})
           (is= (create-minion "Elisabeth" :id "e")
                {:attacks-performed-this-turn 0
                 :damage-taken                0
                 :entity-type                 :minion
                 :properties                  {:permanent #{"divine-shield", "taunt"}
                                               :temporary {}
                                               :stats     {}}
                 :name                        "Elisabeth"
                 :id                          "e"})
           (is= (create-minion "Ida" :id "i")
                {:attacks-performed-this-turn 0
                 :damage-taken                0
                 :entity-type                 :minion
                 :properties                  {:permanent #{}
                                               :temporary {}
                                               :stats     {}}
                 :name                        "Ida"
                 :id                          "i"
                 :on-minion-damage            "Ida"})
           )}
  [name & kvs]
  (let [properties (-> (get-definition name) (:properties)) ; Will be used later
        minion (merge
                 {:damage-taken                0
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
                                                       :active-secrets []
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
                                                       :active-secrets []
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
                                                          :active-secrets []
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
           (is= (as-> (create-empty-state) $
                      (:minion-ids-summoned-this-turn $))
                [])
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
    (-> (update-in state
               [:players player-id :minions]
               (fn [minions]
                 (conj (->> minions
                            (mapv (fn [m]
                                    (if (< (:position m) position)
                                      m
                                      (update m :position inc)))))
                       ready-minion)))
        (update-in [:minion-ids-summoned-this-turn] (fn [list] (conj list (:id minion)))))))


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

;todo test please
(defn add-secret-to-player
  [state player-id secret]
  (update-in state [:players player-id :active-secrets] conj secret))

;todo test please
(defn add-secrets-to-player
  [state player-id secrets]
  (reduce (fn [state secret]
            (add-secret-to-player state player-id secret))
          state
          secrets))

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
                                                                        :attacks-performed-this-turn 0
                                                                        :added-to-board-time-id      2
                                                                        :entity-type                 :minion
                                                                        :properties                  {:permanent #{}
                                                                                                      :temporary {}
                                                                                                      :stats     {}}
                                                                        :name                        "Mio"
                                                                        :id                          "m1"
                                                                        :position                    0
                                                                        :owner-id                    "p1"}]
                                                       :active-secrets         []
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
                                                       :active-secrets []
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
                                         active-secrets :active-secrets
                                         deck      :deck
                                         hand      :hand}]
                               (-> (if mana
                                     (-> state
                                         (update-mana player-id mana)
                                         (update-max-mana player-id mana))
                                     state)
                                   (add-minions-to-board player-id minions)
                                   (add-secrets-to-player player-id active-secrets)
                                   (add-cards-to-deck player-id deck)
                                   (add-cards-to-hand player-id hand)
                                   (assoc-in [:minion-ids-summoned-this-turn] [])))
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

(defn get-characters
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                    {:minions [(create-minion "Emil" :id "e")]}]) $
                      (get-characters $)
                      (map :id $))
                ["h1", "h2", "m", "e"])
           )}
  [state]
  (concat (get-heroes state) (get-minions state)))

(defn get-deck-size
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Emil" :id "e")]}])
                    (get-deck-size "p1"))
                0)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m1")]
                                   :deck    [(create-minion "Emil" :id "e1")
                                             (create-minion "Emil" :id "e2")]}
                                  {:minions [(create-minion "Emil" :id "e3")]
                                   :deck    [(create-minion "Emil" :id "e4")
                                            (create-minion "Emil" :id "e5")]}])
                    (get-deck-size "p1"))
                2)
           )}
  [state player-id]
  (count (get-in state [:players player-id :deck])))

(defn replace-minion
  "Replaces a minion with the same id as the given new-minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (replace-minion (create-minion "Ronja" :id "m" :position 0 :added-to-board-time-id 2 :owner-id "p1"))
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
;position, added-to-board-time-id, owner-id

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

;TODO what if player has max minions?
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

;TODO allow multiple minions
(defn friendly-minions?
  "Returns whether minions are friendly"
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                            (create-minion "Mio" :id "m2")]}
                                 {:minions [(create-minion "Mio" :id "m3")
                                            (create-minion "Mio" :id "m4")]}])
                   (friendly-minions? "m1" "m2")))
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                                (create-minion "Mio" :id "m2")]}
                                     {:minions [(create-minion "Mio" :id "m3")
                                                (create-minion "Mio" :id "m4")]}])
                       (friendly-minions? "m1" "m3")))
           )}
  [state minion-id other-minion-id]
  (= (:owner-id (get-minion state minion-id)) (:owner-id (get-minion state other-minion-id))))



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
           (is= (-> (create-game [{:hero (create-hero "Carl" :id "h1")}])
                    (get-mana-cost "h1"))
                2)
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (get-mana-cost "h1"))
                2))}
  [state id]
  (if-not (card? state id)
    (let [hero (get-hero state id)
          definition (get-definition hero)]
      (:mana-cost (get-definition (:hero-power definition))))
    (let [card (get-card state id)
          definition (get-definition card)]
      (:mana-cost definition))))

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
    ;fatigue happens not in this function
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

; call all the functions of active minions corresponding to a game event eg. end-of-turn, on-minion-damage
(defn do-game-event-functions
  {:test (fn []
           ; testing end of turn function
           (is= (-> (create-game [{:minions [(create-minion "Pippi" :id "p")
                                             (create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e1")
                                             (create-minion "Emil" :id "e2")]}])
                    (do-game-event-functions :end-of-turn :player-id "p1")
                    (get-minion "e1")
                    (:damage-taken))
                1)
           (is= (as-> (create-game [{:minions [(create-minion "Ida" :id "i")
                                               (create-minion "Mio" :id "m")
                                               (create-minion "Emil" :id "e1")
                                               (create-minion "Emil" :id "e2")]}]) $
                      (do-game-event-functions $ :on-minion-damage)
                      (contains? (get-in (get-minion $ "i") [:properties :permanent]) "taunt"))
                true))}
  ([state game-event-key & {:keys [player-id target-id]}]
   (reduce
     (fn [state minion]
       (if-not (game-event-key minion)
         state
         (if target-id
           ((game-event-key (get-definition minion)) state (:id minion) target-id)
           ((game-event-key (get-definition minion)) state (:id minion)))
         ))
     state
     (if player-id
       (get-minions state player-id)
       (get-minions state)))))

(defn give-property
  "Gives a property (temporary or permanent) to a minion. Temporary properties must have no spaces because it is a map key"
  ;TODO make sure that if temporary buff is overwritten, it keeps the higher duration
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}])
                   (give-property "j" "divine-shield")
                   (get-minion "j")
                   (:properties)
                   (:permanent)
                   (contains? "divine-shield")))
           (is= (-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}])
                    (give-property "j" "divine-shield" 1)
                    (get-minion "j")
                    (:properties)
                    (:temporary)
                    (:divine-shield))
                1)
           )}
  ([state minion-id property]
   (update-minion state minion-id :properties (fn [properties-map]
                                                (update-in properties-map [:permanent]
                                                           (fn [permanent-set] (conj permanent-set property))))))

  ([state minion-id property duration]
   (update-minion state minion-id :properties (fn [properties-map]
                                                (assoc-in properties-map [:temporary (keyword property)] duration)))))

(defn get-minion-properties
  "Gets properties of a minion"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (get-minion-properties "e"))
                {:permanent #{"taunt" "divine-shield"}, :temporary {}, :stats {}})
           (is= (-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}])
                    (give-property "j" "divine-shield")
                    (get-minion-properties "j"))
                {:permanent #{"taunt" "divine-shield"}, :temporary {} :stats {}})
           (is= (-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}])
                    (give-property "j" "divine-shield" 1)
                    (get-minion-properties "j"))
                {:permanent #{"taunt"}, :temporary {:divine-shield 1} :stats {}})
           )}
  [state minion-id]
  (:properties (get-minion state minion-id)))


(defn has-property?
  "Check if minion has (temporary or permanent property"
  {:test (fn []
           (is (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}]) $
                     (has-property? $ "j" "taunt")))
           (is (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]}]) $
                     (give-property $ "e" "taunt" 1)
                     (has-property? $ "e" "taunt")))
           )}

  [state minion-id property]
  (let [permanent-set (:permanent (get-minion-properties state minion-id))
        temporary-set (:temporary (get-minion-properties state minion-id))]
    (or (contains? permanent-set property) (contains? temporary-set (keyword property)))))

(defn remove-property
  "Removes all instances of property from a minion (both temporary and permanent"
  {:test (fn []
           (is-not (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}]) $
                         (remove-property $ "j" "taunt")
                         (has-property? $ "j" "taunt")))
           (is-not (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}]) $
                         (give-property $ "j" "windfury" 1)
                         (remove-property $ "j" "windfury")
                         (has-property? $ "j" "windfury")))
           )}
  [state minion-id property]
  (-> state
      (update-minion minion-id :properties (fn [properties-map]
                                             (update-in properties-map [:permanent] (fn [permanent-set]
                                                                                      (disj permanent-set property)))))
      (update-minion minion-id :properties (fn [properties-map]
                                             (update-in properties-map [:temporary] (fn [temporary-set]
                                                                                      (dissoc temporary-set (keyword property))))))))

(defn give-taunt
  "Gives taunt to a minion"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (give-taunt "m")
                    (get-minion "m")
                    (get-in [:properties :permanent])
                    (contains? "taunt"))
                true))}
  [state id]
  (give-property state id "taunt"))

(defn remove-taunt
  "Removes taunt from a minion card"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (remove-taunt "e")
                    (get-minion "e")
                    (get-in [:properties :permanent])
                    (contains? "taunt"))
                false))}
  [state minion-id]
  (remove-property state minion-id "taunt"))

(defn player-has-active-aura-buff?
  "Looks for an active minion of the current player that has a specific aura buff"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Tjorven" :id "t")
                                             (create-minion "Madicken" :id "m")]}])
                    (player-has-active-aura-buff? "p1" "Friendly-windfury"))
                true)
           (is= (-> (create-game [{:minions [(create-minion "Astrid" :id "a")]}])
                    (player-has-active-aura-buff? "p1" "Friendly-windfury"))
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
           (is-not (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                       (has-windfury? "e" "p1")))
           )}
  [state minion-id player-id]
  (or (has-property? state minion-id "windfury")
      (player-has-active-aura-buff? state player-id "Friendly-windfury")))

(defn has-taunt?
  "Checks if minion has taunt"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (has-taunt? "e"))
                true)
           ;
           (is= (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]}]) $
                      (give-property $ "e" "taunt" 1)
                      (has-taunt? $ "e"))
                true)
           (is= (-> (create-game [{:minions [(create-minion "Kato" :id "k")]}])
                    (has-taunt? "k"))
                false)
           )}
  [state minion-id]
  (has-property? state minion-id "taunt"))

(defn has-poisonous
  "Checks if minion has taunt"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Herr Nilsson" :id "hn")]}])
                    (has-poisonous "hn"))
                true)
           (is= (-> (create-game [{:minions [(create-minion "Kato" :id "k")]}])
                    (has-poisonous "k"))
                false))}
  [state minion-id]
  (has-property? state minion-id "poisonous"))

;Gives divine shield to a card
(defn give-divine-shield
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Kato" :id "k")]}])
                    (give-divine-shield "k")
                    (get-minion-properties "k")
                    (:permanent)
                    (contains? "divine-shield"))
                true))}
  [state minion-id]
  (give-property state minion-id "divine-shield"))

(defn has-divine-shield?
  {:test (fn []
           ;return true when minion has divine shield
           (is (-> (create-game [{:minions [(create-minion "Uncle Melker" :id "um")]}])
                   (has-divine-shield? "um")))
           ;return false when minion has no divine shield
           (is-not (-> (create-game [{:minions [(create-minion "Kato" :id "k")]}])
                       (has-divine-shield? "k")))
           ;return true when minion has temporary divine shield
           (is (-> (create-game [{:minions [(create-minion "Kato" :id "k")]}])
                   (give-property "k" "divine-shield" 2)
                   (has-divine-shield? "k")))
           )}

  [state minion-id]
  (has-property? state minion-id "divine-shield"))

;Remove divine shield from a minion
(defn remove-divine-shield
  {:test (fn []
           (is-not (-> (create-game [{:minions [(create-minion "Uncle Melker" :id "um")]}])
                       (remove-divine-shield "um")
                       (get-minion "um")
                       (get-in [:properties :permanent])
                       (contains? "divine-shield")))
           (error? (-> (create-game [{:minions [(create-minion "Uncle Melker" :id "um")]}])
                       (remove-divine-shield "um")
                       (remove-divine-shield "um")))
           )}
  [state minion-id]
  (if (has-divine-shield? state minion-id)
    (-> state
        (remove-property minion-id "divine-shield")
        (do-game-event-functions :on-divine-shield-removal :target-id minion-id))
    (error "No divine shield to be removed")))


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


(defn give-deathrattle
  "Gives a minion deathrattle from another minion"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (give-deathrattle "m" "Madicken")
                    (get-minion "m")
                    (:deathrattle))
                "Madicken")
           )}
  [state minion-id name-source]
  (update-minion state minion-id :deathrattle name-source))


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


(defn get-minion-stats
  "Gets the stats of a minion"
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]}]) $
                      (get-minion-stats $ "e"))
                [2 5])
           )}
  [state minion-id]
  (let [minion-def (get-definition (get-minion state minion-id))
        minion-stat-properties (:stats (get-minion-properties state minion-id))]
    (let [base-attack (:attack minion-def)
          base-health (:health minion-def)
          attack-modifiers (reduce + (map :buff (:attack minion-stat-properties)))
          health-modifiers (reduce + (map :buff (:health minion-stat-properties)))
          damage-taken (:damage-taken (get-minion state minion-id) state)]
      [(+ base-attack attack-modifiers), (- (+ base-health, health-modifiers) damage-taken)])))

(defn modify-minion-attack
  "Modifies the attack stat property of a minion"
  {:test (fn []
           ;one buff
           (is= (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]}]) $
                      (modify-minion-attack $ "e" 3)
                      (get-minion-stats $ "e"))
                [5 5])
           ;two buffs stack
           (is= (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]}]) $
                      (modify-minion-attack $ "e" 1)
                      (modify-minion-attack $ "e" 3)
                      (get-minion-stats $ "e"))
                [6 5])
           ;one permanent and one temporary buff
           (is= (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]}]) $
                      (modify-minion-attack $ "e" 1)
                      (modify-minion-attack $ "e" 3 2)
                      (get-minion-stats $ "e"))
                [6 5])
           )}
  ([state minion-id value]
   (if (> value 0)
     (let [temporary (:temporary (get-minion-properties state minion-id))
           permanent (:permanent (get-minion-properties state minion-id))
           minion-stats (:stats (get-minion-properties state minion-id))]
       (if (empty? minion-stats)
         (update-minion state minion-id :properties
                        {:permanent permanent, :temporary temporary,
                         :stats     {:attack [{:buff value}]}})
         (update-minion state minion-id :properties
                        {:permanent permanent, :temporary temporary,
                         :stats     (conj minion-stats {:attack (conj (:attack minion-stats) {:buff value})})})))
     state))

  ([state minion-id value duration]
   (if (> value 0)
     (let [temporary (:temporary (get-minion-properties state minion-id))
           permanent (:permanent (get-minion-properties state minion-id))
           minion-stats (:stats (get-minion-properties state minion-id))]
       (if (empty? minion-stats)
         (update-minion state minion-id :properties
                        {:permanent permanent, :temporary temporary,
                         :stats     {:attack [{:buff value :duration duration}]}})
         (update-minion state minion-id :properties
                        {:permanent permanent, :temporary temporary,
                         :stats     (conj minion-stats
                                          {:attack (conj (:attack minion-stats) {:buff value :duration duration})})})))
     state)))

(defn modify-minion-max-health
  "Modifies the max health stat property of a minion"
  {:test (fn []
           ;one buff
           (is= (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]}]) $
                      (modify-minion-max-health $ "e" 3)
                      (get-minion-stats $ "e"))
                [2 8])
           ;two buffs stack
           (is= (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]}]) $
                      (modify-minion-max-health $ "e" 1)
                      (modify-minion-max-health $ "e" 3)
                      (get-minion-stats $ "e"))
                [2 9])
           ;one permanent and one temporary
           (is= (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]}]) $
                      (modify-minion-max-health $ "e" 1)
                      (modify-minion-max-health $ "e" 3, 2)
                      (get-minion-stats $ "e"))
                [2 9])
           )}
  ([state minion-id value]
   (if (> value 0)
     (let [temporary (:temporary (get-minion-properties state minion-id))
           permanent (:permanent (get-minion-properties state minion-id))
           minion-stats (:stats (get-minion-properties state minion-id))]
       (if (empty? minion-stats)
         (update-minion state minion-id :properties
                        {:permanent permanent, :temporary temporary,
                         :stats     {:health [{:buff value}]}})
         (update-minion state minion-id :properties
                        {:permanent permanent, :temporary temporary,
                         :stats     (conj minion-stats {:health (conj (:health minion-stats) {:buff value})})})))
     state))
  ([state minion-id value duration]
   (if (> value 0)
     (let [temporary (:temporary (get-minion-properties state minion-id))
           permanent (:permanent (get-minion-properties state minion-id))
           minion-stats (:stats (get-minion-properties state minion-id))]
       (if (empty? minion-stats)
         (update-minion state minion-id :properties
                        {:permanent permanent, :temporary temporary,
                         :stats     {:health [{:buff value :duration duration}]}})
         (update-minion state minion-id :properties
                        {:permanent permanent, :temporary temporary,
                         :stats     (conj minion-stats
                                          {:health (conj (:health minion-stats) {:buff value :duration duration})})})))
     state)))


(defn modify-minion-stats
  "Modifies the stat property of a minion"
  {:test (fn []
           ;give permanent attack buff
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (modify-minion-stats "e" 3 0)
                    (get-minion-stats "e"))
                [4, 1])
           ;give permanent health buff
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (modify-minion-stats "e" 0 1)
                    (get-minion-stats "e"))
                [1, 2])
           ;give permanent attack and health buff
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (modify-minion-stats "e" 2 2)
                    (get-minion-stats "e"))
                [3, 3])
           ;give temporary attack and health buff
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (modify-minion-stats "e" 2 2 4)
                    (get-minion-stats "e"))
                [3, 3])
           )}
  ([state minion-id attack health]
   (-> state
       (modify-minion-attack minion-id attack)
       (modify-minion-max-health minion-id health)))
  ([state minion-id attack health duration]
   (-> state
       (modify-minion-attack minion-id attack duration)
       (modify-minion-max-health minion-id health duration))))

; Creates an active-secret for a player, needs a name an an owner id
(defn create-secret
  [name owner-id & kvs]
  (let [secret {:name            name
                :type             :spell
                :sub-type       :secret
                :damage-taken    0
                :owner-id       owner-id
                :hero-power-used false}]
    (if (empty? kvs)
      secret
      (apply assoc secret kvs))))

; Gets all the active secrets
(defn get-active-secrets
  {:test (fn []
           (is= (as-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")]}]) $
                      (get-active-secrets $ "p1")
                      (map :name $))
                ["Explosive Trap"])
           (is= (-> (create-empty-state)
                    (get-active-secrets))
                []))}
  ([state player-id]
   (:active-secrets (get-player state player-id)))
  ([state]
   (->> (:players state)
        (vals)
        (map :active-secrets)
        (apply concat))))

; Gets a secret with a given id
(defn get-secret
  {:test (fn []
           (is= (-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")]}])
                    (get-secret "e")
                    (:name))
                "Explosive Trap"))}
  [state id]
  (->> (get-active-secrets state)
       (filter (fn [s] (= (:id s) id)))
       (first)))

; Removes a secret
(defn remove-secret
  {:test (fn []
           (is= (-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")]}])
                    (remove-secret "e")
                    (get-active-secrets))
                []))}
  [state id]
  (let [owner-id (:owner-id (get-secret state id))]
    (update-in state
               [:players owner-id :active-secrets]
               (fn [secrets]
                 (remove (fn [s] (= (:id s) id)) secrets)))))


; Gets a specific secret
(defn get-active-secret
  {:test (fn []
           (is= (-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")]}])
                    (get-active-secret "e")
                    (:name))
                "Explosive Trap"))}
  [state id]
  (->> (get-active-secrets state)
       (filter (fn [s] (= (:id s) id)))
       (first)))