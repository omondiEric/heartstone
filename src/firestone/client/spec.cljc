(ns firestone.client.spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::non-negative-int (s/and int?
                                 (comp not neg?)))
(s/def ::positive-int (s/and int?
                             pos?))

;; Attributes
(s/def ::name string?)
(s/def ::display-name string?)
(s/def ::external-id string?)
(s/def ::id string?)
(s/def ::owner-id string?)

(s/def ::entity-type #{"card"
                       "hero"
                       "hero-power"
                       "minion"
                       "permanent"
                       "player"
                       "quest"
                       "secret"
                       "weapon"})

(s/def :firestone.client.card.spec/type #{"hero"
                                          "hero-power"
                                          "minion"
                                          "spell"
                                          "weapon"})

;; Minions
(s/def ::position (s/and int? (fn [x] (<= 0 x 6))))
(s/def ::attack ::non-negative-int)
(s/def ::original-attack ::non-negative-int)
(s/def ::health int?)
(s/def ::max-health ::non-negative-int)
(s/def ::original-health pos?)
(s/def ::sleepy boolean?)
(s/def ::can-attack boolean?)
(s/def ::description string?)
(s/def ::valid-attack-ids (s/coll-of ::id))
(s/def ::states (s/coll-of #{"aura"
                             "deathrattle"
                             "divine-shield"
                             "effect"
                             "elusive"
                             "enrage"
                             "frozen"
                             "immune"
                             "inspire"
                             "lifesteal"
                             "mega-windfury"
                             "poisonous"
                             "silenced"
                             "spell-damage"
                             "stealth"
                             "taunt"
                             "windfury"}))

;; Cards
(s/def ::original-mana-cost ::non-negative-int)
(s/def ::playable boolean?)
(s/def ::special-effect-active boolean?)
(s/def ::valid-target-ids (s/coll-of ::id))

;; Heroes
(s/def ::armor ::non-negative-int)
(s/def ::mana-cost ::non-negative-int)

;; Weapons
(s/def ::durability ::positive-int)
(s/def ::max-durability ::positive-int)
(s/def ::original-durability ::positive-int)

;; Hero powers
(s/def ::can-use boolean?)
(s/def ::has-used-your-turn boolean?)

;; Entities
(s/def ::hero-power (s/and (s/keys :req-un [::can-use
                                            ::owner-id
                                            ::entity-type
                                            ::has-used-your-turn
                                            ::name
                                            ::description]
                                   :opt-un [::mana-cost
                                            ::original-mana-cost
                                            ::valid-target-ids])
                           (fn [hero-power]
                             (= (:entity-type hero-power) "hero-power"))))

(s/def ::weapon (s/and (s/keys :re-un [::name
                                       ::owner-id
                                       ::durability
                                       ::entity-type
                                       ::original-durability
                                       ::max-durability
                                       ::attack
                                       ::original-attack
                                       ::states])
                       (fn [weapon]
                         (= (:entity-type weapon) "weapon"))))

(s/def ::hero (s/keys :req-un [::armor
                               ::owner-id
                               ::entity-type
                               ::attack
                               ::can-attack
                               ::health
                               ::id
                               ::mana
                               ::max-health
                               ::max-mana
                               ::name
                               ::states
                               ::valid-attack-ids]
                      :opt-un [::weapon
                               ::hero-power]))

(s/def ::card (s/and (s/keys :req-un [::entity-type
                                      ::name
                                      ::mana-cost
                                      ::original-mana-cost
                                      :firestone.client.card.spec/type]
                             :opt-un [::owner-id
                                      ::id
                                      ::attack
                                      ::original-attack
                                      ::health
                                      ::original-health
                                      ::armor
                                      ::playable
                                      ::description
                                      ::valid-target-ids
                                      ::special-effect-active])
                     (fn [x]
                       (= (:entity-type x) "card"))))

(s/def ::secret (s/and (s/keys :req-un [::name
                                        ::owner-id
                                        ::id
                                        ::entity-type
                                        ::original-mana-cost
                                        ::description])
                       (fn [secret]
                         (= (:entity-type secret) "secret"))))

(s/def ::reward-card ::card)

(s/def ::counter ::non-negative-int)
(s/def ::goal ::non-negative-int)

(s/def ::progress (s/and (s/keys :req-un [::counter
                                          ::goal])))

(s/def ::quest (s/and (s/keys :req-un [::id
                                       ::owner-id
                                       ::entity-type
                                       ::progress
                                       ::card
                                       ::reward-card])
                      (fn [x]
                        (= (:entity-type x) "quest"))))

(s/def ::permanent (s/and (s/keys :req-un [::entity-type
                                           ::id
                                           ::name
                                           ::owner-id
                                           ::position]
                                  :opt-un [::description])
                          (fn [x]
                            (= (:entity-type x) "permanent"))))

(s/def ::minion (s/and (s/keys :req-un [::attack
                                        ::can-attack
                                        ::entity-type
                                        ::health
                                        ::id
                                        ::name
                                        ::mana-cost
                                        ::max-health
                                        ::original-attack
                                        ::original-health
                                        ::owner-id
                                        ::position
                                        ::sleepy
                                        ::states
                                        ::valid-attack-ids]
                               :opt-un [::description])
                       (fn [minion]
                         (= (:entity-type minion) "minion"))))

(s/def ::board-entities (s/and (s/coll-of (s/or :minion ::minion
                                                :permanent ::permanent))
                               (fn [board-entities]
                                 (let [board-entities (map second board-entities)]
                                   (= board-entities (sort-by :position board-entities))))))

(s/def ::active-secrets (s/coll-of ::secret))
(s/def ::active-quest ::quest)
(s/def ::deck-size ::non-negative-int)
(s/def ::hand (s/coll-of ::card))

(s/def ::player (s/keys :req-un [::board-entities
                                 ::active-secrets
                                 ::deck-size
                                 ::hand
                                 ::hero
                                 ::id]
                        :opt-un [::active-quest]))

(s/def ::player-in-turn ::id)

(s/def ::players (s/and (s/coll-of ::player)
                        (fn [x] (>= (count x) 2))))

(s/def ::action-index ::non-negative-int)
(s/def ::turn-index ::non-negative-int)
(s/def ::supports-redo boolean?)
(s/def ::supports-undo boolean?)

(s/def ::game-state (s/keys :req-un [::id
                                     ::supports-undo
                                     ::supports-redo
                                     ::action-index
                                     ::turn-index
                                     ::player-in-turn
                                     ::players]))

(s/def ::game-states (s/and vector?
                            (s/coll-of ::game-state)))