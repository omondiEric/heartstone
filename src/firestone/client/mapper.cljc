(ns firestone.client.mapper
  (:require [ysera.test :refer [is is-not is=]]
            [firestone.client.spec]
            [firestone.construct :refer [create-game
                                         create-card
                                         get-hand
                                         get-card
                                         get-player
                                         get-players]]
            [firestone.core :refer [get-health]]
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
   :valid-attack-ids []})

(defn get-client-card
  {:test (fn []
           (is (check-spec :firestone.client.spec/card
                           (let [game (create-game [{:deck [(create-card "Emil" :id "e")]}])
                                 card (get-card game "e")]
                             (get-client-card game (get-player game "p1") card)))))}
  [state player card]
  (let [card-definition (get-definition card)] ;lets us get mana-cost
  {:entity-type (:entity-type card)
   :name (:name card)
   :mana-cost (:mana-cost card-definition)
   :original-mana-cost 0})) ;not sure what the difference btwn mana-cost and original mana cost is

(defn get-client-hand
  {:test (fn []
           (is (check-spec :firestone.client.spec/hand
                           (as-> (create-game [{:hand ["Emil"]}]) $
                                 ;(let [hand (get-hand $ "p1")]
                                   (get-client-hand $ (get-player $ "p1"))))))}
  [state player]
  ;(get-hand state (:id player)))

  (->> (get-hand state (:id player))
       (map (fn [c]
              (get-client-card state player c)))))



(defn get-client-player
  {:test (fn []
           (is (check-spec :firestone.client.spec/player
                           (as-> (create-game) $
                                 (get-client-player $ (get-player $ "p1"))))))}
  [state player]
  {:board-entities []
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
