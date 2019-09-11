(ns firestone.construct
  (:require [ysera.test :refer [is is-not is= error?]]
            [firestone.definitions :refer [get-definition]]
            [firestone.definitions-loader]))


(defn create-hero
  "Creates a hero from its definition by the given hero name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-hero "Carl")
                {:name         "Carl"
                 :entity-type  :hero
                 :damage-taken 0})
           (is= (create-hero "Carl" :damage-taken 10)
                {:name         "Carl"
                 :entity-type  :hero
                 :damage-taken 10}))}
  [name & kvs]
  (let [hero {:name         name
              :entity-type  :hero
              :damage-taken 0}]
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

(defn create-minion
  "Creates a minion from its definition by the given minion name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-minion "Mio" :id "m" :attacks-performed-this-turn 1)
                {:attacks-performed-this-turn 1
                 :damage-taken                0
                 :entity-type                 :minion
                 :name                        "Mio"
                 :id                          "m"}))}
  [name & kvs]
  (let [definition (get-definition name)                    ; Will be used later
        minion {:damage-taken                0
                :entity-type                 :minion
                :name                        name
                :attacks-performed-this-turn 0}]
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
                 :players                       {"p1" {:id      "p1"
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :hero    {:name         "Carl"
                                                                 :id           "c"
                                                                 :damage-taken 0
                                                                 :entity-type  :hero}}
                                                 "p2" {:id      "p2"
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :hero    {:name         "Gustaf"
                                                                 :id           "h2"
                                                                 :damage-taken 0
                                                                 :entity-type  :hero}}}
                 :counter                       1
                 :minion-ids-summoned-this-turn []}))}
  ([heroes]
   ; Creates Carl heroes if heroes are missing.
   (let [heroes (->> (concat heroes [(create-hero "Carl")
                                     (create-hero "Carl")])
                     (take 2))]
     {:player-id-in-turn             "p1"
      :players                       (->> heroes
                                          (map-indexed (fn [index hero]
                                                         {:id      (str "p" (inc index))
                                                          :deck    []
                                                          :hand    []
                                                          :minions []
                                                          :hero    (if (contains? hero :id)
                                                                     hero
                                                                     (assoc hero :id (str "h" (inc index))))}))
                                          (reduce (fn [a v]
                                                    (assoc a (:id v) v))
                                                  {}))
      :counter                       1
      :minion-ids-summoned-this-turn []}))
  ([]
   (create-empty-state [])))

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
                             (add-minion-to-board "p1" (create-minion "Mio" :id "m1") 0)
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

(defn create-game
  "Creates a game with the given deck, hand, minions (placed on the board), and heroes."
  {:test (fn []
           (is= (create-game) (create-empty-state))

           (is= (create-game [{:hero (create-hero "Anduin Wrynn")}])
                (create-game [{:hero "Anduin Wrynn"}]))

           (is= (create-game [{:minions [(create-minion "Mio")]}])
                (create-game [{:minions ["Mio"]}]))

           (is= (create-game [{:minions ["Mio"]
                               :deck    ["Ronja"]
                               :hand    ["Emil"]}
                              {:hero "Anduin Wrynn"}]
                             :player-id-in-turn "p2")
                {:player-id-in-turn             "p2"
                 :players                       {"p1" {:id      "p1"
                                                       :deck    [{:entity-type :card
                                                                  :id          "c3"
                                                                  :name        "Ronja"
                                                                  :owner-id    "p1"}]
                                                       :hand    [{:entity-type :card
                                                                  :id          "c4"
                                                                  :name        "Emil"
                                                                  :owner-id    "p1"}]
                                                       :minions [{:damage-taken                0
                                                                  :attacks-performed-this-turn 0
                                                                  :added-to-board-time-id      2
                                                                  :entity-type                 :minion
                                                                  :name                        "Mio"
                                                                  :id                          "m1"
                                                                  :position                    0
                                                                  :owner-id                    "p1"}]
                                                       :hero    {:name         "Carl"
                                                                 :id           "h1"
                                                                 :entity-type  :hero
                                                                 :damage-taken 0}}
                                                 "p2" {:id      "p2"
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :hero    {:name         "Anduin Wrynn"
                                                                 :id           "h2"
                                                                 :entity-type  :hero
                                                                 :damage-taken 0}}}
                 :counter                       5
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
                                         minions   :minions
                                         deck      :deck
                                         hand      :hand}]
                               (-> state
                                   (add-minions-to-board player-id minions)
                                   (add-cards-to-deck player-id deck)
                                   (add-cards-to-hand player-id hand)
                                   ; (add-secrets player-id secrets)
                                   ))
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
                ["m2" "m3"]))}
  [state & ids]
  (reduce remove-minion state ids))