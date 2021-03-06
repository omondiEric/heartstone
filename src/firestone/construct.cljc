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

(def game-event-fn-names #{:on-end-of-turn, :on-minion-damage, :deathrattle :on-divine-shield-removal})

(defn get-additional-minion-field
  {:test (fn []
           (is= (get-additional-minion-field "Pippi" :on-end-of-turn)
                {:on-end-of-turn "Pippi"})
           (is-not (get-additional-minion-field "Mio" :on-end-of-turn)))}
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
  (let [permanent (:properties (get-definition name))
        minion (merge
                 {:damage-taken                0
                  :entity-type                 :minion
                  :name                        name
                  :attacks-performed-this-turn 0
                  :properties                  (if (empty? permanent)
                                                 {:permanent #{}
                                                  :temporary {}
                                                  :stats     {}}
                                                 {:permanent permanent
                                                  :temporary {}
                                                  :stats     {}})}
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
                 :players                       {"p1" {:id             "p1"
                                                       :mana           10
                                                       :max-mana       10
                                                       :deck           []
                                                       :hand           []
                                                       :minions        []
                                                       :active-secrets []
                                                       :fatigue-level  0
                                                       :hero           {:name            "Carl"
                                                                        :id              "c"
                                                                        :owner-id        "p1"
                                                                        :damage-taken    0
                                                                        :hero-power-used false
                                                                        :entity-type     :hero}}
                                                 "p2" {:id             "p2"
                                                       :mana           10
                                                       :max-mana       10
                                                       :deck           []
                                                       :hand           []
                                                       :minions        []
                                                       :active-secrets []
                                                       :fatigue-level  0
                                                       :hero           {:name            "Gustaf"
                                                                        :id              "h2"
                                                                        :owner-id        "p2"
                                                                        :damage-taken    0
                                                                        :hero-power-used false
                                                                        :entity-type     :hero}}}
                 :counter                       1
                 :seed                          0
                 :minion-ids-summoned-this-turn []
                 :cards-played-this-game        []}))}
  ([heroes]
   ; Creates Carl heroes if heroes are missing.
   (let [heroes (->> (concat heroes [(create-hero "Carl")
                                     (create-hero "Carl")])
                     (take 2))]
     {:player-id-in-turn             "p1"
      :players                       (->> heroes
                                          (map-indexed (fn [index hero]
                                                         {:id             (str "p" (inc index))
                                                          :mana           10
                                                          :max-mana       10
                                                          :deck           []
                                                          :hand           []
                                                          :minions        []
                                                          :active-secrets []
                                                          :fatigue-level  0
                                                          :hero           (if (contains? hero :id)
                                                                            (assoc hero :owner-id (str "p" (inc index)))
                                                                            (assoc hero :id (str "h" (inc index))
                                                                                        :owner-id (str "p" (inc index))))}))
                                          (reduce (fn [a v]
                                                    (assoc a (:id v) v))
                                                  {}))
      :counter                       1
      :seed                          0
      :minion-ids-summoned-this-turn []
      :cards-played-this-game        []}))
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
        (update-in [:minion-ids-summoned-this-turn] (fn [list] (conj list id))))))


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

; Creates an active-secret for a player, needs a name an an owner id
(defn create-secret
  {:test (fn []
           (is= (create-secret "Vaporize" "p1" :id "v")
                {:name            "Vaporize"
                 :type            :spell
                 :sub-type        :secret
                 :damage-taken    0
                 :owner-id        "p1"
                 :hero-power-used false
                 :id              "v"}))}
  [name owner-id & kvs]
  (let [secret {:name            name
                :type            :spell
                :sub-type        :secret
                :damage-taken    0
                :owner-id        owner-id
                :hero-power-used false}]
    (if (empty? kvs)
      secret
      (apply assoc secret kvs))))

(defn add-secret-to-player
  [state player-id secret]
  (update-in state [:players player-id :active-secrets] conj secret))

(defn add-secrets-to-player
  [state player-id secrets]
  (reduce (fn [state secret]
            (add-secret-to-player state player-id secret))
          state
          secrets))

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
                 :players                       {"p1" {:id             "p1"
                                                       :mana           3
                                                       :max-mana       3
                                                       :deck           [{:entity-type :card
                                                                         :id          "c3"
                                                                         :name        "Ronja"
                                                                         :owner-id    "p1"}]
                                                       :hand           [{:entity-type :card
                                                                         :id          "c4"
                                                                         :name        "Emil"
                                                                         :owner-id    "p1"}]
                                                       :minions        [{:damage-taken                0
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
                                                       :active-secrets []
                                                       :fatigue-level  0
                                                       :hero           {:name            "Carl"
                                                                        :id              "h1"
                                                                        :owner-id        "p1"
                                                                        :entity-type     :hero
                                                                        :damage-taken    0
                                                                        :hero-power-used false}}
                                                 "p2" {:id             "p2"
                                                       :mana           10
                                                       :max-mana       10
                                                       :deck           []
                                                       :hand           []
                                                       :minions        []
                                                       :active-secrets []
                                                       :fatigue-level  0
                                                       :hero           {:name            "Carl"
                                                                        :id              "h2"
                                                                        :owner-id        "p2"
                                                                        :entity-type     :hero
                                                                        :damage-taken    0
                                                                        :hero-power-used false}}}
                 :counter                       5
                 :seed                          0
                 :minion-ids-summoned-this-turn []
                 :cards-played-this-game        []}))}
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
                     (reduce (fn [state {player-id      :player-id
                                         mana           :mana
                                         minions        :minions
                                         active-secrets :active-secrets
                                         deck           :deck
                                         hand           :hand}]
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

(defn decrement-minions-position
  "Decrements positions of all the minions on the board after a specific position"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")
                                             (create-minion "Annika" :id "a")]}])
                    (decrement-minions-position 1)
                    (get-minion "a")
                    (:position))
                1))}
  [state minion-pos]
  (reduce (fn [state minion]
            (if (> (:position minion) minion-pos)
              (update-minion state (:id minion) :position (dec (:position minion)))
              state))
          state
          (get-minions state)))

(defn remove-minion
  "Removes a minion with the given id from the state."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (remove-minion "m")
                    (get-minions))
                [])
           (as-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                          (create-minion "Ronja" :id "r1")
                                          (create-minion "Ronja" :id "r2")]}
                               {:minions [(create-minion "Emil" :id "e")]}]) $
                 (remove-minion $ "m")
                 (do (is= (:position (get-minion $ "r1"))
                          0)
                     (is= (:position (get-minion $ "r2"))
                          1)))
           )}
  [state id]
  (let [owner-id (:owner-id (get-minion state id))
        minion-pos (:position (get-minion state id))]
    (as-> (update-in state
                     [:players owner-id :minions]
                     (fn [minions]
                       (remove (fn [m] (= (:id m) id)) minions))) $
          (decrement-minions-position $ minion-pos))))

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
  (reduce remove-minion state ids))

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

(defn card-in-hand?
  "Returns whether a card is in the hand"
  {:test (fn []
           (is (as-> (create-game [{:hand [(create-card "Mio" :id "m")]}]) $
                     (card-in-hand? $ "m")))
           (is-not (-> (create-game [{:deck [(create-card "Mio" :id "m")]}])
                       (card-in-hand? "m")))
           )}
  [state card-id]
  (let [owner-id (:owner-id (get-card state card-id))
        hand-ids (map :id (get-hand state owner-id))]
    (not (nil? (some (fn [item] (= card-id item)) hand-ids)))))


(defn card-in-deck?
  "Returns whether a card is in the deck"
  {:test (fn []
           (is-not (as-> (create-game [{:hand [(create-card "Mio" :id "m")]}]) $
                         (card-in-deck? $ "m")))
           (is (-> (create-game [{:deck [(create-card "Mio" :id "m")]}])
                   (card-in-deck? "m")))
           )}
  [state card-id]
  (let [owner-id (:owner-id (get-card state card-id))
        deck-ids (map :id (get-deck state owner-id))]
    (not (nil? (some (fn [item] (= card-id item)) deck-ids)))))

(defn replace-card
  "Replaces a card with the same id as the given new-card."
  {:test (fn []
           ;card in hand
           (is= (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                    (replace-card (create-card "Ronja" :id "m"))
                    (get-card "m")
                    (:name))
                "Ronja")
           ;card in deck
           (is= (-> (create-game [{:deck [(create-card "Mio" :id "m")]}])
                    (replace-card (create-card "Ronja" :id "m"))
                    (get-card "m")
                    (:name))
                "Ronja")
           ;card not in hand or deck
           (error? (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                       (replace-card (create-card "Ronja" :id "e"))))
           )}
  [state new-card]
  (let [owner-id (or (:owner-id new-card)
                     (:owner-id (get-card state (:id new-card))))]
    (cond
      (card-in-hand? state (:id new-card))
      (update-in state
                 [:players owner-id :hand]
                 (fn [card]
                   (map (fn [m]
                          (if (= (:id m) (:id new-card))
                            new-card
                            m))
                        card)))
      (card-in-deck? state (:id new-card))
      (update-in state
                 [:players owner-id :deck]
                 (fn [card]
                   (map (fn [m]
                          (if (= (:id m) (:id new-card))
                            new-card
                            m))
                        card)))
      :else (error "Card not found"))))

(defn update-card
  "Like update-minion but for cards"
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Mio" :id "m" :attack-buff 1)]}])
                    (update-card "m" :attack-buff inc)
                    (get-card "m")
                    (:attack-buff))
                2)
           (is= (-> (create-game [{:hand [(create-card "Mio" :id "m")]}])
                    (update-card "m" :attack-buff 2)
                    (get-card "m")
                    (:attack-buff))
                2)
           )}
  [state id key function-or-value]
  (let [card (get-card state id)]
    (replace-card state (if (fn? function-or-value)
                          (update card key function-or-value)
                          (assoc card key function-or-value)))))

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

(defn get-random-minion-conditional
  "Gets a random minion from a list filtered by supplied function"
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
                 (get-random-minion-conditional $ (fn [state minion-id]
                                                    (some? (:battlecry (get-definition (get-minion state minion-id))))))
                 (do (is= (:id (last $)) "e1")
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
                 (get-random-minion-conditional $ (fn [state minion-id]
                                                    (some? (:battlecry (get-definition (get-minion state minion-id)))))
                                                "p2")
                 (do (is= (:id (last $)) "e3")
                     (is (not= (:seed (first $)) 0))))
           )}
  ([state condition-function]
   (->> (filter (fn [minion]
                  (condition-function state (:id minion))) (get-minions state))
        (random-nth state)))
  ([state condition-function player-id]
   (->> (filter (fn [minion]
                  (condition-function state (:id minion))) (get-minions state player-id))
        (random-nth state))))

; Creates an active-secret for a player, needs a name an an owner id
(defn create-secret
  {:test (fn []
           (is= (create-secret "Vaporize" "p1" :id "v")
                {:name         "Vaporize"
                 :type         :spell
                 :sub-type     :secret
                 :damage-taken 0
                 :owner-id     "p1"
                 :id           "v"}))}
  [name owner-id & kvs]
  (let [secret {:name         name
                :type         :spell
                :sub-type     :secret
                :damage-taken 0
                :owner-id     owner-id}]
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

(defn switch-secret-side
  "Switches a secret from one player to the other"
  {:test (fn []
           (is= (-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")
                                                    (create-secret "Venomstrike Trap" "p1" :id "v")]}
                                  {:hand [(create-card "Kezan Mystic" :id "s")]}])
                    (switch-secret-side "e")
                    (get-active-secrets "p2")
                    (count))
                1))}
  [state secret-id]
  (let [secret (get-secret state secret-id)
        player-id (get-other-player-id (secret :owner-id))]
    (-> state
        (remove-secret secret-id)
        (add-secret-to-player player-id secret))))


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

(defn get-card-or-character
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (get-card-or-character "e")
                    (:name))
                "Emil")
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (get-card-or-character "e")
                    (:name))
                "Emil")
           )}
  [state id]
  (if (card? state id)
    (get-card state id)
    (get-character state id)))

(defn valid-minion-effect-target?
  "Checks whether target is valid"
  {:test (fn []
           (is (as-> (create-game [{:minions [(create-minion "Astrid" :id "a")
                                              (create-minion "Madicken" :id "m")]}]) $
                     (valid-minion-effect-target? $ "a" "m")))
           (is (as-> (create-game [{:hand [(create-minion "Astrid" :id "a")]}
                                   {:minions [(create-minion "Madicken" :id "m")]}]) $
                     (valid-minion-effect-target? $ "a" "m")))
           (is-not (-> (create-game [{:minions [(create-minion "Astrid" :id "a")
                                                (create-minion "Emil" :id "e")]}])
                       (valid-minion-effect-target? "a" "e")))
           (is (as-> (create-game [{:minions [(create-minion "Astrid" :id "a")
                                              (create-minion "Madicken" :id "m")]}]) $
                     (valid-minion-effect-target? $ (get-definition "Astrid") "m")))
           )}
  [state id-or-def target-id]
  (let [valid-target-function (if (some? (:description id-or-def))
                                (:valid-target? id-or-def)
                                (:valid-target? (get-definition (get-card-or-character state id-or-def))))]
    (if (some? valid-target-function)
      (valid-target-function state (get-character state target-id))
      (error "No valid target function found"))))

(defn valid-secret-trigger?
  "Checks if a given attack should trigger a secret"
  {:test (fn []
           (is (as-> (create-game [{:active-secrets [(create-secret "Vaporize" "p1" :id "v")]
                                    :minions        [(create-minion "Madicken" :id "m")]}
                                   {:minions        [(create-minion "Emil" :id "e")]}]) $
                     (valid-secret-trigger? $ "v" "p1" "e" "h1")))
           )}
  [state secret-id player-id attacker-id target-id]
  (let [valid-target-function (:valid-trigger? (get-definition (get-secret state secret-id)))]
    (if (some? valid-target-function)
      (valid-target-function state player-id attacker-id target-id)
      (error "No valid target function found"))))

; call all the functions of active minions corresponding to a game event eg. end-of-turn, on-minion-damage
(defn do-game-event-functions
  {:test (fn []
           ; testing end of turn function
           (is= (-> (create-game [{:minions [(create-minion "Pippi" :id "p")
                                             (create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e1")
                                             (create-minion "Emil" :id "e2")]}])
                    (do-game-event-functions :on-end-of-turn :player-id "p1")
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

;todo trying to generalize do-game-event-fn to apply to all cards
(defn do-secret-game-event-functions
  {:test (fn []
           (is= (-> (create-game [{:minions        [(create-minion "Mio" :id "m")]
                                   :active-secrets [(create-secret "Vaporize" "p1" :id "v")]}
                                  {:minions [(create-minion "Kato" :id "k")]}])
                    (do-secret-game-event-functions :on-attack :player-id "p1" :attacker-id "k" :target-id "h1")
                    (get-minions "p2"))
                ()))}
  [state game-event-key & {:keys [player-id attacker-id target-id]}]
  (reduce
    (fn [state secret]
      (let [secret-def (get-definition (:name secret))]
        (if
          (and (game-event-key secret-def) (valid-secret-trigger? state (:id secret) player-id attacker-id target-id))
          (-> state
              ((game-event-key secret-def) player-id attacker-id target-id)
              (remove-secret (:id secret)))
          state)))
    state
    (if player-id
      (get-active-secrets state player-id)
      (get-active-secrets state))))

(defn give-property
  "Gives a property (temporary or permanent) to a minion. Temporary properties must have no spaces because it is a map key"
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

(defn remove-all-properties
  {:test (fn []
           (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}]) $
                 (give-property $ "j" "windfury" 1)
                 (remove-all-properties $ "j")
                 (do (is-not (has-property? $ "j" "taunt"))
                     (is-not (has-property? $ "j" "windfury"))))
           )}
  [state minion-id]
  (-> state
      (update-minion minion-id :properties (fn [properties-map]
                                             (assoc-in properties-map [:permanent] #{})))
      (update-minion minion-id :properties (fn [properties-map]
                                             (assoc-in properties-map [:temporary] {})))
      ))

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
                    (player-has-active-aura-buff? "p1" "windfury"))
                true)
           (is= (-> (create-game [{:minions [(create-minion "Astrid" :id "a")]}])
                    (player-has-active-aura-buff? "p1" "windfury"))
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
      (player-has-active-aura-buff? state player-id "windfury")))

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

(defn poisonous?
  "Checks if minion is poisonous"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Herr Nilsson" :id "hn")]}])
                    (poisonous? "hn"))
                true)
           (is= (-> (create-game [{:minions [(create-minion "Kato" :id "k")]}])
                    (poisonous? "k"))
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


(defn minion?
  {:test (fn []
           (is (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]}]) $
                     (minion? (get-minion $ "e"))))
           (is-not (as-> (create-game [{:hand [(create-card "Emil" :id "e")]}]) $
                         (minion? (get-card $ "e"))))
           (is-not ((create-game [{:hero (create-hero "Carl" :id "h1")}])
                    (minion? "Carl")))
           )}
  [character]
  (= (:entity-type character) :minion))

(defn character?
  {:test (fn []
           (is (as-> (create-game [{:minions [(create-minion "Emil" :id "e")]}]) $
                     (character? (get-minion $ "e"))))
           (is-not (as-> (create-game [{:hand [(create-card "Emil" :id "e")]}]) $
                         (character? (get-card $ "e"))))
           (is (as-> (create-game [{:hero (create-hero "Carl" :id "h1")}]) $
                     (character? (get-character $ "h1"))))
           )}
  [character]
  (or (= (:entity-type character) :minion) (= (:entity-type character) :hero)))

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

(defn get-random-secret-minion
  {:test (fn []
           ;get a random minion from specific player
           (as-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")
                                                 (create-secret "Venomstrike Trap" "p1" :id "v")]}
                               {:hand [(create-card "Kezan Mystic" :id "s")]}]) $
                 (get-random-secret-minion $ "p1")
                 (do (is= (:id (last $)) "e")
                     (is (not= (:seed (first $)) 0)))))}
  ([state]
   (->> (get-active-secrets state)
        (random-nth state)))
  ([state player-id]
   (->> (get-active-secrets state player-id)
        (random-nth state))))

(defn remove-minion-stat-buffs
  "Resets minion's stats to original"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (modify-minion-stats "e" 3 3)
                    (modify-minion-stats "e" 1 1 1)
                    (remove-minion-stat-buffs "e")
                    (get-minion-stats "e"))
                [1, 1])
           ;can add buffs after removal
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")]}])
                    (modify-minion-stats "e" 3 3)
                    (modify-minion-stats "e" 1 1 1)
                    (remove-minion-stat-buffs "e")
                    (modify-minion-stats "e" 3 3)
                    (get-minion-stats "e"))
                [4, 4])
           )}
  [state minion-id]
  (update-minion state minion-id :properties (fn [properties-map]
                                               (assoc-in properties-map [:stats] {}))))

(defn remove-minion-effects
  "Remove all deathrattle, on-turn, etc effects from a minion"
  {:test (fn []
           (is-not (-> (create-game [{:minions [(create-minion "Madicken" :id "e")]}])
                       (remove-minion-effects "e")
                       (get-minion "e")
                       (:deathrattle)))
           )}
  [state minion-id]
  (let [minion (get-minion state minion-id)
        standard-minion-keys (map first
                                  (filter (fn [x] (not (contains? game-event-fn-names (key x)))) minion))]
    (replace-minion state (select-keys minion standard-minion-keys))))


(defn get-minion-card-stat-buffs
  "Gets the stat buffs applied to a minion card"
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-card "Madicken" :id "e")]}])
                    (get-minion-card-stat-buffs "e"))
                [0 0])
           (is= (-> (create-game [{:hand [(create-card "Madicken" :id "e" :attack-buff 2 :health-buff 2)]}])
                    (get-minion-card-stat-buffs "e"))
                [2 2])
           )}
  [state card-id]
  (let [attack-buff (:attack-buff (get-card state card-id))
        health-buff (:health-buff (get-card state card-id))]
    (let [attack-buff (if (nil? attack-buff) 0 attack-buff)
          health-buff (if (nil? health-buff) 0 health-buff)]
      [attack-buff, health-buff])))

(defn buff-minion-card
  "Gives a minion card a stat buff"
  {:test (fn []
           ;card in hand
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (buff-minion-card "e" 2 2)
                    (get-minion-card-stat-buffs "e"))
                [2 2])
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (buff-minion-card "e" 2 0)
                    (get-minion-card-stat-buffs "e"))
                [2 0])
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (buff-minion-card "e" 0 2)
                    (get-minion-card-stat-buffs "e"))
                [0 2])
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (buff-minion-card "e" 0 0)
                    (get-minion-card-stat-buffs "e"))
                [0 0])
           ;card in deck
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                    (buff-minion-card "e" 2 2)
                    (get-minion-card-stat-buffs "e"))
                [2 2])
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                    (buff-minion-card "e" 2 0)
                    (get-minion-card-stat-buffs "e"))
                [2 0])
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                    (buff-minion-card "e" 0 2)
                    (get-minion-card-stat-buffs "e"))
                [0 2])
           (is= (-> (create-game [{:deck [(create-card "Emil" :id "e")]}])
                    (buff-minion-card "e" 0 0)
                    (get-minion-card-stat-buffs "e"))
                [0 0])
           ;multiple buffs
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (buff-minion-card "e" 2 2)
                    (buff-minion-card "e" 2 2)
                    (get-minion-card-stat-buffs "e"))
                [4 4])
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (buff-minion-card "e" 2 0)
                    (buff-minion-card "e" 2 0)
                    (get-minion-card-stat-buffs "e"))
                [4 0])
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (buff-minion-card "e" 0 2)
                    (buff-minion-card "e" 0 2)
                    (get-minion-card-stat-buffs "e"))
                [0 4])
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")]}])
                    (buff-minion-card "e" 0 2)
                    (buff-minion-card "e" 2 0)
                    (get-minion-card-stat-buffs "e"))
                [2 2])
           )}
  [state card-id attack health]
  (cond
    (and (> attack 0) (> health 0))
    (as-> state $
          (if (nil? (:attack-buff (get-card $ card-id)))
            (update-card $ card-id :attack-buff attack)
            (update-card $ card-id :attack-buff (fn [x] (+ attack x))))
          (if (nil? (:health-buff (get-card $ card-id)))
            (update-card $ card-id :health-buff health)
            (update-card $ card-id :health-buff (fn [x] (+ health x)))))
    (> attack 0)
    (if (nil? (:attack-buff (get-card state card-id)))
      (update-card state card-id :attack-buff attack)
      (update-card state card-id :attack-buff (fn [x] (+ attack x))))
    (> health 0)
    (if (nil? (:health-buff (get-card state card-id)))
      (update-card state card-id :health-buff health)
      (update-card state card-id :health-buff (fn [x] (+ health x))))
    :else state))

(defn add-card-to-cards-played
  "Adds a card to the state field"
  {:test (fn []
           (is= (as-> (create-game [{:hand [(create-card "Emil" :id "e")]}]) $
                      (add-card-to-cards-played $ (get-card $ "e"))
                      (get-in $ [:cards-played-this-game]))
                [{:name "Emil", :entity-type :card, :id "e", :owner-id "p1"}])
           (is= (as-> (create-game [{:hand [(create-card "Emil" :id "e")
                                            (create-card "Ronja" :id "r")]}]) $
                      (add-card-to-cards-played $ (get-card $ "e"))
                      (add-card-to-cards-played $ (get-card $ "r"))
                      (get-in $ [:cards-played-this-game]))
                [{:name "Emil", :entity-type :card, :id "e", :owner-id "p1"}
                 {:name "Ronja", :entity-type :card, :id "r", :owner-id "p1"}])
           )}
  [state card]
  (let [cards-played-this-game (get-in state [:cards-played-this-game])]
    (assoc-in state [:cards-played-this-game] (conj cards-played-this-game card))))

(defn get-all-played-cards-with-property
  "Returns list of all cards played this game with a certain property, e.g. battlecry"
  {:test (fn []
           (is= (as-> (create-game []
                                   :cards-played-this-game [{:name "Emil", :entity-type :card, :id "e", :owner-id "p1"}
                                                            {:name "Ronja", :entity-type :card, :id "r", :owner-id "p1"}
                                                            {:name "Spellbreaker", :entity-type :card, :id "s", :owner-id "p2"}
                                                            {:name "Radar Raid", :entity-type :card, :id "rr", :owner-id "p2"}
                                                            ]) $
                      (get-all-played-cards-with-property $ :battlecry))
                [{:name "Emil", :entity-type :card, :id "e", :owner-id "p1"}
                 {:name "Spellbreaker", :entity-type :card, :id "s", :owner-id "p2"}])
           (is= (as-> (create-game []
                                   :cards-played-this-game [{:name "Emil", :entity-type :card, :id "e", :owner-id "p1"}
                                                            {:name "Ronja", :entity-type :card, :id "r", :owner-id "p1"}
                                                            {:name "Spellbreaker", :entity-type :card, :id "s", :owner-id "p2"}
                                                            {:name "Radar Raid", :entity-type :card, :id "rr", :owner-id "p2"}
                                                            ]) $
                      (get-all-played-cards-with-property $ :battlecry "p1"))
                [{:name "Emil", :entity-type :card, :id "e", :owner-id "p1"}])
           )}
  ([state property]
   (let [cards-played-this-game (get-in state [:cards-played-this-game])]
     (filter
       (fn [card] (some? ((keyword property) (get-definition card))))
       cards-played-this-game)))
  ([state property player-id]
   (let [cards-played-this-game (get-in state [:cards-played-this-game])]
     (filter
       (fn [card] (and
                    (some? ((keyword property) (get-definition card)))
                    (= (:owner-id card) player-id)))
       cards-played-this-game))))


(defn get-random-secret
  {:test (fn []
           ;get a random minion from specific player
           (as-> (create-game [{:active-secrets [(create-secret "Explosive Trap" "p1" :id "e")
                                                 (create-secret "Venomstrike Trap" "p1" :id "v")]}
                               {:hand [(create-card "Kezan Mystic" :id "s")]}]) $
                 (get-random-secret $ "p1")
                 (do (is= (:id (last $)) "e")
                     (is (not= (:seed (first $)) 0)))))}
  ([state]
   (->> (get-active-secrets state)
        (random-nth state)))
  ([state player-id]
   (->> (get-active-secrets state player-id)
        (random-nth state))))
