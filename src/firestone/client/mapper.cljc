(ns firestone.client.mapper
  (:require [ysera.test :refer [is is-not is=]]
            [firestone.client.spec]
            [firestone.construct :refer [create-game
                                         create-card
                                         create-minion
                                         get-hand
                                         get-card
                                         get-player
                                         get-players
                                         get-mana-cost
                                         get-minion
                                         get-minions
                                         get-minion-properties]]
            [firestone.core :refer [get-health
                                    get-attack
                                    sleepy?]]
            [firestone.definitions :refer [get-definition]]
            [clojure.spec.alpha :as spec]
            [firestone.definitions-loader]))

(defn check-spec
  [spec value]
  (let [valid (spec/valid? spec value)]
    (if valid
      true
      (do (spec/explain spec value)
          false))))

(defn get-client-hero
  {:test (fn []
           (is (check-spec :firestone.client.spec/hero
                           (let [game (create-game)
                                 player (get-player game "p1")
                                 hero (:hero player)]
                             (get-client-hero game player hero)))))}
  [state player hero]
  {:armor            0
   :owner-id         (:id player)
   :entity-type      "hero"
   :attack           0
   :can-attack       false
   :health           (get-health hero)
   :id               (:id hero)
   :mana             (:mana player)
   :max-health       30
   :max-mana         (:max-mana player)
   :name             (:name hero)
   :states           []
   ;;:hero-power       (get-client-hero-power )
   :valid-attack-ids []})

(defn get-client-card
  {:test (fn []
           (is (check-spec :firestone.client.spec/card
                           (let [game (create-game [{:deck [(create-card "Emil" :id "e")]}])
                                 card (get-card game "e")]
                             (get-client-card game card)))))}
  [state card]
  (let [card-definition (get-definition card)] ;lets us get mana-cost
  {:entity-type "card"
   :name (:name card)
   :mana-cost (:mana-cost card-definition)     ;(get-mana-cost state (:id card))
   :original-mana-cost (:mana-cost card-definition)
   :id  (:id card)
   :playable true
   :description (:description card-definition)
   :type (name (:type card-definition))}))

(defn get-client-hand
  {:test (fn []
           (is (check-spec :firestone.client.spec/hand
                           (as-> (create-game [{:hand ["Emil"]}]) $
                                   (get-client-hand $ (get-player $ "p1"))))))}
  [state player]
  (->> (get-hand state (:id player))
       (map (fn [c]
              (get-client-card state c)))))

(defn get-minion-states
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}]) $
                    (get-minion-states $ (get-minion $ "j")))
                #{"taunt"}))}
  [state minion]
  (let [minion-properties (get-minion-properties state (:id minion))]
    (let [permanent-properties (:permanent minion-properties)
          temp-properties (reduce (fn [curr-set key]
                                  (conj curr-set (name key)))
                                  #{}
                                  (reduce conj #{} (clojure.core/keys (:temporary minion-properties))))]
    (clojure.set/union temp-properties permanent-properties))))

(defn get-client-minion
  {:test (fn []
           (is (check-spec :firestone.client.spec/minion
                           (let [game (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                                 minion (get-minion game "m")]
                             (get-client-minion game minion)))))}
  [state minion]
  (let [minion-defn (get-definition (:name minion))
        minion-permanent-set (get-in minion [:properties :permanent])]
    {:attack      (get-attack state (:id minion))
     :can-attack  (not (contains? minion-permanent-set "NoAttack"))
     :entity-type  "minion"
     :health    (get-health minion)
     :id        (:id minion)
     :name      (:name minion)
     :mana-cost  (:mana-cost minion-defn)
     :max-health  30   ; is this true?
     :original-attack  (:attack minion-defn)
     :original-health  (:health minion-defn)
     :owner-id         (:owner-id minion)
     :position         (:position minion)
     :sleepy           (sleepy? state (:id minion))
     :states           (get-minion-states state minion)
     :valid-attack-ids  []}))

(defn get-client-minions
  {:test (fn []
           (is (check-spec :firestone.client.spec/board-entities
                           (let [game (create-game [{:minions [(create-minion "Mio" :id "m")
                                                               (create-minion "Emil" :id "e")]}])]
                             (get-client-minions game "p1")))))}
  [state player]
  (->> (get-minions state (:id player))
       (map (fn [m]
              (get-client-card state m)))))


(defn get-client-player
  {:test (fn []
           (is (check-spec :firestone.client.spec/player
                           (as-> (create-game) $
                                 (get-client-player $ (get-player $ "p1"))))))}
  [state player]
  {:board-entities (get-client-minions state player)
   :active-secrets []
   :deck-size      16
   :hand           (get-client-hand state player)
   :hero           (get-client-hero state player (:hero player))
   :id             (:id player)})


(defn get-client-state
  {:test (fn []
           (is (check-spec :firestone.client.spec/game-states
                           (-> (create-game)
                               (get-client-state)))))}
  [state]
  [{:id             "the-game-id"
    :player-in-turn (:player-id-in-turn state)
    :players        (->> (get-players state)
                         (map (fn [p]
                                (get-client-player state p))))}])
