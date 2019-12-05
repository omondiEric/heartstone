(ns firestone.client.mapper
  (:require [ysera.test :refer [is is-not is=]]
            [firestone.client.spec]
            [firestone.construct :refer [create-card
                                         create-game
                                         create-minion
                                         create-secret
                                         get-active-secrets
                                         get-card
                                         get-characters
                                         get-deck-size
                                         get-hand
                                         get-mana-cost
                                         get-minion
                                         get-minions
                                         get-minion-card-stat-buffs
                                         get-minion-properties
                                         get-secret
                                         minion?
                                         get-player
                                         get-players]]
            [firestone.core :refer [get-attack
                                    get-health
                                    get-minion-max-health
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

(defn get-target-ids
  {:test (fn []
           (is (check-spec :firestone.client.spec/valid-target-ids
                           (let [game (create-game)
                                 player (get-player game "p1")
                                 hero (:hero player)
                                 hero-power (:hero-power (get-definition (:name hero)))]
                             (get-target-ids game hero-power))))
           (is= (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]
                                     :hand    [(create-card "Radar Raid" :id "rr")
                                               (create-card "Insect Swarm" :id "is")]}
                                    {:minions [(create-minion "Emil" :id "e")
                                               (create-minion "Ronja" :id "r")]}]) $
                      (get-target-ids $ (get-card $ "rr")))
                ["h1" "h2" "j" "e" "r"])
           (is= (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]
                                     :hand    [(create-card "Annika" :id "a")]}
                                    {:minions [(create-minion "Emil" :id "e")
                                               (create-minion "Ronja" :id "r")]}]) $
                      (get-target-ids $ (get-card $ "a")))
                ["j" "e" "r"])
           )}
  [state card-or-hero-power]
  (when (some? (:valid-target? (get-definition card-or-hero-power)))
    (let [valid-target-function (:valid-target? (get-definition card-or-hero-power))
          valid-targets
          (filter (fn [character]
                    (valid-target-function state character))
                  (get-characters state))]
      (map :id valid-targets))))

(defn get-client-hero-power
  {:test (fn []
           (is (check-spec :firestone.client.spec/hero-power
                           (let [game (create-game)
                                 player (get-player game "p1")
                                 hero (:hero player)
                                 hero-power (:hero-power (get-definition (:name hero)))]
                             (get-client-hero-power game hero hero-power)))))}
  [state hero hero-power]
  (let [hero-power-def (get-definition hero-power)]
    {:can-use            true
     :owner-id           (:id hero)
     :entity-type        (name (:type hero-power-def))
     :has-used-your-turn (:hero-power-used hero)
     :name               (:name hero-power-def)
     :description        (:description hero-power-def)
     :valid-target-ids   (get-target-ids state hero-power)
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
   :hero-power       (get-client-hero-power state hero (:hero-power (get-definition (:name hero))))
   :valid-attack-ids []})


(defn get-valid-target-ids-for-card
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]
                                     :hand    [(create-card "Radar Raid" :id "rr")
                                               (create-card "Insect Swarm" :id "is")]}
                                    {:minions [(create-minion "Emil" :id "e")
                                               (create-minion "Ronja" :id "r")]}]) $
                      (get-valid-target-ids-for-card $ (get-card $ "rr")))
                ["h1" "h2" "j" "e" "r"])
           (is= (as-> (create-game [{:minions [(create-minion "Jonatan" :id "j")]
                                     :hand    [(create-card "Annika" :id "a")]}
                                    {:minions [(create-minion "Emil" :id "e")
                                               (create-minion "Ronja" :id "r")]}]) $
                      (get-valid-target-ids-for-card $ (get-card $ "a")))
                ["j" "e" "r"])
           )}
  [state card]
  (when (some? (:valid-target? (get-definition card)))
    (let [valid-target-function (:valid-target? (get-definition card))
          valid-targets
          (filter (fn [character]
                    (valid-target-function state character))
                  (get-characters state))]
      (map :id valid-targets))))

(defn get-client-card
  {:test (fn []
           (is (check-spec :firestone.client.spec/card
                           (let [game (create-game [{:deck [(create-card "Emil" :id "e")]}])
                                 card (get-card game "e")]
                             (get-client-card game card)))))}
  [state card]
  (let [card-definition (get-definition card)]
    {:entity-type        "card"
     :name               (:name card)
     :mana-cost          (:mana-cost card-definition)
     :original-mana-cost (:mana-cost card-definition)
     :id                 (:id card)
     :playable           true
     :description        (:description card-definition)
     :type               (name (:type card-definition))
     :owner-id           (:owner-id card)
     :attack             (when (= (:type card-definition) :minion)
                           (+ (:attack card-definition) (first (get-minion-card-stat-buffs state (:id card)))))
     :health             (when (= (:type card-definition) :minion)
                           (+ (:health card-definition) (last (get-minion-card-stat-buffs state (:id card)))))
     :original-attack    (:attack card-definition)
     :original-health    (:health card-definition)
     ::valid-target-ids  (first (conj [] (get-target-ids state card)))}))

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
                #{"taunt"})
           (is= (as-> (create-game [{:minions [(create-minion "Madicken" :id "m")]}]) $
                      (get-minion-states $ (get-minion $ "m")))
                #{"deathrattle"})
           (is= (as-> (create-game [{:minions [(create-minion "Ida" :id "i")]}]) $

                      (get-minion-states $ (get-minion $ "i")))
                #{"effect"})
           )}
  [state minion]
  (let [minion-properties (get-minion-properties state (:id minion))
        minion-def (get-definition minion)]
    (let [permanent-properties (:permanent minion-properties)
          temp-properties (reduce (fn [curr-set key]
                                    (conj curr-set (name key)))
                                  #{}
                                  (reduce conj #{} (clojure.core/keys (:temporary minion-properties))))
          other-properties (cond
                             (some? (:deathrattle minion-def))
                             #{"deathrattle"}
                             (not-empty (filter (fn [property] (clojure.string/starts-with? (name (first property)) "on")) minion-def))
                             #{"effect"}
                             :else
                             #{})]
      (clojure.set/union temp-properties permanent-properties other-properties))))


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
        valid-attack-ids (first (conj [] (get-valid-target-ids-for-minion state minion (:owner-id minion))))]
    {:attack           (get-attack state (:id minion))
     :can-attack       (not (empty? valid-attack-ids))
     :entity-type      "minion"
     :health           (get-health state (:id minion))
     :id               (:id minion)
     :name             (:name minion)
     :mana-cost        (:mana-cost minion-defn)
     :max-health       (get-minion-max-health state (:id minion))
     :original-attack  (:attack minion-defn)
     :original-health  (:health minion-defn)
     :owner-id         (:owner-id minion)
     :position         (:position minion)
     :sleepy           (sleepy? state (:id minion))
     :states           (get-minion-states state minion)
     :valid-attack-ids valid-attack-ids
     :description      (if-not (:description minion-defn)
                         ""
                         (:description minion-defn))}))

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

(defn get-client-secret
  {:test (fn []
           (is (check-spec :firestone.client.spec/secret
                           (let [game (create-game [{:active-secrets [(create-secret "Vaporize" "p1" :id "v")]}])]
                             (get-client-secret game "v"))))
           )}
  [state secret-id]
  (let [secret (get-secret state secret-id)
        secret-def (get-definition (:name secret))]
    {:name (:name secret-def)
     :owner-id (:owner-id secret)
     :id (:id secret)
     :entity-type "secret"
     :original-mana-cost (:mana-cost secret-def)
     :description (:description secret-def)
     })
  )

(defn get-client-secrets
  {:test (fn []
           (is (check-spec :firestone.client.spec/active-secrets
                           (as-> (create-game [{:active-secrets [(create-secret "Vaporize" "p1" :id "v")]}]) $
                             (get-client-secrets $ (get-player $ "p1")))))
           )}
  [state player]
  (->> (get-active-secrets state (:id player))
       (map (fn [s]
              (get-client-secret state (:id s))))))


(defn get-client-player
  {:test (fn []
           (is (check-spec :firestone.client.spec/player
                           (as-> (create-game [{:active-secrets [(create-secret "Vaporize" "p1" :id "v")]
                                                :minion [(create-minion "Mio" :id "m1")
                                                       (create-minion "Emil" :id "e1")]}
                                               {:minion [(create-minion "Mio" :id "m2")
                                                       (create-minion "Emil" :id "e2")]}]) $
                                 (get-client-player $ (get-player $ "p1")))
                           )))}
  [state player]
  {:board-entities (get-client-minions state player)
   :active-secrets (get-client-secrets state player)
   :deck-size      (get-deck-size state (:id player))
   :hand           (get-client-hand state player)
   :hero           (get-client-hero state player (:hero player))
   :id             (:id player)})


(defn get-client-state
  {:test (fn []
           (is (check-spec :firestone.client.spec/game-states
                           (-> (create-game [{:active-secrets [(create-secret "Vaporize" "p1" :id "v")]
                                              :minion [(create-minion "Mio" :id "m1")
                                                       (create-minion "Emil" :id "e1")]}
                                             {:minion [(create-minion "Mio" :id "m2")
                                                       (create-minion "Emil" :id "e2")]}])
                               (get-client-state)))))}
  [state]
  [{:id             "the-game-id"
    :player-in-turn (:player-id-in-turn state)
    :players        (->> (get-players state)
                         (map (fn [p]
                                (get-client-player state p))))
    :supports-undo true
    :supports-redo true
    :action-index (:action-index state)
    :turn-index (:turn-number state)}])