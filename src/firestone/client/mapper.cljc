(ns firestone.client.mapper
  (:require [ysera.test :refer [is is-not is=]]
            [firestone.client.spec]
            [firestone.construct :refer [create-game
                                         create-card
                                         create-minion
                                         get-card
                                         get-characters
                                         get-deck-size
                                         get-hand
                                         get-player
                                         get-players
                                         get-mana-cost
                                         get-minion
                                         get-minions
                                         get-minion-properties
                                         minion?]]
            [firestone.core :refer [get-health
                                    get-attack
                                    sleepy?
                                    valid-attack?]]
            [firestone.core-api :refer [attack-hero-or-minion
                                        end-turn
                                        play-card]]
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

(defn get-client-hero-power-target-ids
  {:test (fn []
           (is (check-spec :firestone.client.spec/valid-target-ids
                           (let [game (create-game)
                                 player (get-player game "p1")
                                 player-id (:id player)]
                             (get-client-hero-power-target-ids game player-id))))
           )}
  [state player-id]
  (->> (get-minions state player-id)
       (map :id)))

(defn get-client-hero-power
  {:test (fn []
           (is (check-spec :firestone.client.spec/hero-power
                           (let [game (create-game)
                                 player (get-player game "p1")
                                 hero (:hero player)
                                 hero-power (:hero-power (get-definition (:name hero)))]
                             (get-client-hero-power game player hero hero-power)))))}
  [state player hero hero-power]
  (let [hero-power-def (get-definition hero-power)]
    {:can-use            true
     :owner-id           (:id hero)
     :entity-type        (name (:type hero-power-def))
     :has-used-your-turn (:hero-power-used hero)
     :name               (:name hero-power-def)
     :description        (:description hero-power-def)
     :valid-target-ids   (when (= (:name hero-power-def) "Blessing")
                           (get-client-hero-power-target-ids state (:id player)))
     :type               (name (:type hero-power-def))}))

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
   :hero-power       (get-client-hero-power state player hero (:hero-power (get-definition (:name hero))))
   :valid-attack-ids []})


(defn get-valid-target-ids-for-card
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]
                                     :hand    [(create-card "Radar Raid" :id "rr")
                                               (create-card "Insect Swarm" :id "is")]}
                                    {:minions [(create-minion "Emil" :id "e")
                                               (create-minion "Ronja" :id "r")]}]) $
                      (get-valid-target-ids-for-card $ (get-card $ "rr") "p1"))
                ["h1" "h2" "j" "e" "r"])
           (is= (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]
                                     :hand    [(create-card "Annika" :id "a")]}
                                    {:minions [(create-minion "Emil" :id "e")
                                               (create-minion "Ronja" :id "r")]}]) $
                      (get-valid-target-ids-for-card $ (get-card $ "a") "p1"))
                ["j" "e" "r"])
           )}
  [state card player-id]
  (if (= (:type (get-definition card)) :spell)
    ;TODO generalize this
    (when (= (:name (get-definition card)) "Radar Raid")
      (let [spell-function (:spell-fn (get-definition card))
            valid-targets
              (filter (fn [c]
                        (spell-function state (:id c)))
                      (get-characters state))]
          (map :id valid-targets)))
    ;TODO generalize this too
    (when (or (= (:name (get-definition card)) "Annika") (= (:name (get-definition card)) "Astrid"))
      (let [on-play-function (:on-play (get-definition card))
            valid-targets
            (when on-play-function
              (filter (fn [c]
                        (on-play-function state player-id (:id card) (:id c)))
                      (get-characters state)))]
            (map :id valid-targets)))))

(defn get-client-card
  {:test (fn []
           (is (check-spec :firestone.client.spec/card
                           (let [game (create-game [{:deck [(create-card "Emil" :id "e")]}])
                                 card (get-card game "e")]
                             (get-client-card game card)))))}
  [state card]
  (let [card-definition (get-definition card)]              ;lets us get mana-cost
    {:entity-type        "card"
     :name               (:name card)
     :mana-cost          (:mana-cost card-definition)       ;(get-mana-cost state (:id card))
     :original-mana-cost (:mana-cost card-definition)
     :id                 (:id card)
     :playable           true
     :description        (:description card-definition)
     :type               (name (:type card-definition))
     ::valid-target-ids  (first (conj [] (get-valid-target-ids-for-card state card (:owner-id card))))}))

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

(defn get-valid-target-ids-for-minion
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]}
                                    {:minions [(create-minion "Emil" :id "e")
                                               (create-minion "Ronja" :id "r")]}]) $
                      (get-valid-target-ids-for-minion $ (get-minion $ "j") "p1"))
                ["h2" "e" "r"])
           (is= (as-> (create-game [{:hand [(create-card "Kato" :id "k1")]}
                                    {:hand [(create-card "Kato" :id "k2")]}]) $
                      (play-card $ "p1" "k1" 0)
                      (end-turn $ "p1")
                      (play-card $ "p2" "k2" 0)
                      (end-turn $ "p2")
                      (get-valid-target-ids-for-minion $ (get-minion $ "k1") "p1"))
                ["h2" "k2"])
           )}
  [state minion player-id]
  (let [valid-targets
        (filter (fn [c]
                  (valid-attack? state player-id (:id minion) (:id c)))
                (get-characters state))]
    (map :id valid-targets)))

(defn get-client-minion
  {:test (fn []
           (is (check-spec :firestone.client.spec/minion
                           (let [game (create-game [{:minions [(create-minion "Mio" :id "m1")]}
                                                    {:minions [(create-minion "Mio" :id "m2")]}])
                                 minion (get-minion game "m1")]
                             (-> game (end-turn "p1")
                                 (end-turn "p2")
                                 (get-client-minion minion))))))}
  [state minion]
  (let [minion-defn (get-definition (:name minion))
        minion-permanent-set (get-in minion [:properties :permanent])]
    {:attack           (get-attack state (:id minion))
     :can-attack       (not (contains? minion-permanent-set "NoAttack"))
     :entity-type      "minion"
     :health           (get-health minion)
     :id               (:id minion)
     :name             (:name minion)
     :mana-cost        (:mana-cost minion-defn)
     :max-health       30                                   ; is this true?
     :original-attack  (:attack minion-defn)
     :original-health  (:health minion-defn)
     :owner-id         (:owner-id minion)
     :position         (:position minion)
     :sleepy           (sleepy? state (:id minion))
     :states           (get-minion-states state minion)
     :valid-attack-ids (first (conj [] (get-valid-target-ids-for-minion state minion (:owner-id minion))))}))

(defn get-client-minions
  {:test (fn []
           (is (check-spec :firestone.client.spec/board-entities
                           (let [game (create-game [{:minions [(create-minion "Mio" :id "m")
                                                               (create-minion "Emil" :id "e")]}])]
                             (get-client-minions game "p1")))))}
  [state player]
  (->> (get-minions state (:id player))
       (map (fn [m]
              (get-client-minion state m)))))


(defn get-client-player
  {:test (fn []
           (is (check-spec :firestone.client.spec/player
                           (as-> (create-game [{:deck [(create-minion "Mio" :id "m1")
                                                       (create-minion "Emil" :id "e1")]}
                                               {:deck [(create-minion "Mio" :id "m2")
                                                       (create-minion "Emil" :id "e2")]}]) $
                                 (get-client-player $ (get-player $ "p1")))
                           )))}
  [state player]
  {:board-entities (get-client-minions state player)
   :active-secrets []
   :deck-size      (get-deck-size state (:id player))
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
