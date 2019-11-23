(ns firestone.core
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.error :refer [error]]
            [ysera.collections :refer [seq-contains?]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [create-card
                                         create-game
                                         create-hero
                                         create-minion
                                         draw-card-to-hand
                                         do-game-event-functions
                                         get-card
                                         get-character
                                         get-heroes
                                         get-hero
                                         get-hand
                                         get-mana
                                         get-mana-cost
                                         get-minion
                                         get-minions
                                         get-minion-properties
                                         get-minion-stats
                                         get-other-player-id
                                         get-random-minion
                                         give-divine-shield
                                         give-property
                                         give-taunt
                                         has-property?
                                         has-divine-shield?
                                         has-taunt?
                                         has-windfury?
                                         ida-present?
                                         modify-minion-attack
                                         modify-minion-stats
                                         minion?
                                         replace-minion
                                         remove-card-from-deck
                                         remove-divine-shield
                                         remove-minion
                                         remove-minions
                                         switch-minion-side
                                         update-mana
                                         update-minion]]))

(defn get-health
  "Returns the health of the character."
  {:test (fn []
           ; The health of minions
           (is= (-> (create-minion "Ronja")
                    (get-health))
                2)
           (is= (-> (create-minion "Ronja" :damage-taken 1)
                    (get-health))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-health "m"))
                2)
           ; The health of heroes
           (is= (-> (create-hero "Carl")
                    (get-health))
                30)
           (is= (-> (create-hero "Carl" :damage-taken 2)
                    (get-health))
                28)
           (is= (-> (create-game [{:hero (create-hero "Carl" :id "h1")}])
                    (get-health "h1"))
                30))}
  ([character]
   {:pre [(map? character) (contains? character :damage-taken)]}
   (let [definition (get-definition character)]
     (- (:health definition) (:damage-taken character))))
  ([state id]
   (if (minion? (get-character state id))
     (last (get-minion-stats state id))
     (get-health (get-character state id)))))

(defn get-minion-max-health
  "Get max health of minion"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-minion-max-health "m"))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m" :damage-taken 1)]}])
                    (get-minion-max-health "m"))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m" :damage-taken 1)]}])
                    (modify-minion-stats "m" 2 2)
                    (get-minion-max-health "m"))
                4)
           )}
  [state minion-id]
  (+ (get-health state minion-id) (:damage-taken (get-minion state minion-id))))


(defn get-attack
  "Returns the attack of the minion with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (get-attack "e"))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (modify-minion-attack "e" 3)
                    (get-attack "e"))
                5)
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (modify-minion-attack "e" 3 2)
                    (get-attack "e"))
                5)
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (modify-minion-attack "e" 3 2)
                    (modify-minion-attack "e" 1 1)
                    (get-attack "e"))
                6)
           )}
  [state minion-id]
  (first (get-minion-stats state minion-id)))

(defn sleepy?
  "Checks if the minion with given id is sleepy."
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]
                                :minion-ids-summoned-this-turn ["m"])
                   (sleepy? "m")))
           (is-not (-> (create-game [{:minions [(create-minion "Stormwind Knight" :id "a")]}])
                   (sleepy? "a")))
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]
                                    :minion-ids-summoned-this-turn [])
                       (sleepy? "m"))))}
  [state id]
  (and (seq-contains? (:minion-ids-summoned-this-turn state) id)
      (not (has-property? state id "charge"))))

(defn refresh-minion-attacks
  "Changes attacks-performed-this-turn for all friendly minions to 0"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m1" :attacks-performed-this-turn 1)
                                            (create-minion "Mio" :id "m2" :attacks-performed-this-turn 1)]}])
                   (refresh-minion-attacks "p1")
                   (get-minion "m1")
                   (:attacks-performed-this-turn))
               0)
           )}
  [state player-id]
  (reduce (fn [state minion]
            (update-minion state (:id minion) :attacks-performed-this-turn 0))
          state
          (get-minions state player-id)))

(defn filter-dead-minions
  "Filters dead minions at a given state and returns all living minions"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                             (create-minion "Mio" :id "m2" :damage-taken 2)]}
                                  {:minions [(create-minion "Mio" :id "m3" :damage-taken 3)
                                             (create-minion "Mio" :id "m4" :damage-taken 1)]}])
                    (->> (filter-dead-minions)
                         (map :id))
                    )
                ["m1" "m4"]))}
  [state]
  (->> (get-minions state)
       (filter (fn [m] (> (->> (:id m) (get-health state)) 0)))))

(defn get-dead-minions
  "Gets dead minions at a given state"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                             (create-minion "Mio" :id "m2" :damage-taken 2)]}
                                  {:minions [(create-minion "Mio" :id "m3" :damage-taken 3)
                                             (create-minion "Mio" :id "m4" :damage-taken 1)]}])
                    (->> (get-dead-minions)
                         (map :id))
                    )
                ["m2" "m3"]))}
  [state]
  (->> (get-minions state)
       (filter (fn [m] (<= (->> (:id m) (get-health state)) 0)))))

(defn valid-attack?
  "Checks if the attack is valid"
  {:test (fn []
           ; Should be able to attack an enemy minion
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                 {:minions [(create-minion "Ronja" :id "r")]}]
                                :minion-ids-summoned-this-turn [])
                   (valid-attack? "p1" "m" "r")))
           ; Should be able to attack an enemy hero
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]
                                  :minion-ids-summoned-this-turn [])
                   (valid-attack? "p1" "m" "h2")))
           ; Should not be able to attack your own minions
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                                (create-minion "Ronja" :id "r")]}])
                       (valid-attack? "p1" "m" "r")))
           ; Should not be able to attack if it is not your turn
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                     {:minions [(create-minion "Ronja" :id "r")]}]
                                    :player-id-in-turn "p2")
                       (valid-attack? "p1" "m" "r")))
           ; Should not be able to attack if you are sleepy
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                     {:minions [(create-minion "Ronja" :id "r")]}]
                                    :minion-ids-summoned-this-turn ["m"])
                       (valid-attack? "p1" "m" "r")))
           ; Should not be able to attack if you already attacked this turn
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m" :attacks-performed-this-turn 1)]}
                                     {:minions [(create-minion "Ronja" :id "r")]}])
                       (valid-attack? "p1" "m" "r")))
           ; Should not be able to attack if you have "NoAttack" property
           (is-not (-> (create-game [{:minions [(create-minion "Alfred" :id "a")]}
                                     {:minions [(create-minion "Ronja" :id "r")]}])
                       (valid-attack? "p1" "a" "r")))
           ; Should be able to attack if target minion has taunt
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                 {:minions [(create-minion "Jonatan" :id "j")]}]
                                  :minion-ids-summoned-this-turn [])
                   (valid-attack? "p1" "m" "j")))
           ; Should not be able to attack if target minion does not have taunt, but other enemy minions do
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                     {:minions [(create-minion "Ronja" :id "r")
                                                (create-minion "Elisabeth" :id "e")]}])
                       (valid-attack? "p1" "m" "r"))))}
  [state player-id attacker-id target-id]
  (let [attacker (get-minion state attacker-id)
        target (get-character state target-id)
        attacker-permanent-set (get-in attacker [:properties :permanent])]
    (and attacker
         target
         ; either the target has taunt
         (or (has-taunt? state target-id)
             ; or no targets have taunt
             (nil? (some true?
                         (->> (get-minions state (get-other-player-id player-id))
                              (map (fn [m]
                                     (has-taunt? state (:id m))))))))
         ; check for "NoAttack" property
         (not (contains? attacker-permanent-set "NoAttack"))
         (= (:player-id-in-turn state) player-id)
         ; should only be able to attack once, or twice if minion has windfury
         (or (and (< (:attacks-performed-this-turn attacker) 1)
                  (not (has-windfury? state attacker-id player-id)))
             (and (< (:attacks-performed-this-turn attacker) 2)
                  (has-windfury? state attacker-id player-id)))
         (not (sleepy? state attacker-id))
         (> (get-attack state attacker-id) 0)
         (not= (:owner-id attacker) (:owner-id target)))))

(defn on-secret-played-fns
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")
                                             (create-minion "Secretkeeper" :id "s")]}])
                    (on-secret-played-fns)
                    (get-minion-stats "s"))
                [2, 3]))}
  [state]
  (reduce (fn [state minion]
            (if (contains? (get-definition minion) :secret-played)
              (let [secret-played-fn (:secret-played (get-definition minion))]
                (secret-played-fn state (:id minion)))
              state))
          state
          (get-minions state)))

(defn do-on-play
  "Returns the on-play function of a minion or nil"
  {:test (fn []
           ;check that damage is taken for Kato
           (is= (as-> (create-game [{:hand [(create-card "Kato" :id "k")]}]) $
                      (do-on-play $ "p1" "k" (get-definition (get-card $ "k")))
                      (do-on-play $ "p2" "k" (get-definition (get-card $ "k")))
                      (do-on-play $ "p2" "k" (get-definition (get-card $ "k")))
                      (do-on-play $ "p2" "k" (get-definition (get-card $ "k")))
                      (get-in $ [:players "p1" :hero :damage-taken]))
                12)
           ;check that card is drawn for Emil
           (is= (as-> (create-game [{:hand [(create-card "Emil" :id "e")] :deck [(create-card "Mio" :id "m")]}]) $
                      (do-on-play $ "p1" "e" (get-definition (get-card $ "e")))
                      (get-hand $ "p1")
                      (count $))
                2)
           ;check that Annika can target a minion
           (is= (as-> (create-game [{:hand    [(create-card "Annika" :id "a")]
                                     :minions [(create-minion "Emil" :id "e")]}]) $
                      (do-on-play $ "p1" "a" (get-definition (get-card $ "a")) "e")
                      (get-minion-stats $ "e"))
                [4, 5])
           ;check that Ronja will cause no errors
           (is= (as-> (create-game [{:hand [(create-card "Ronja" :id "r")]}]) $
                      (do-on-play $ "p1" "r" (get-definition (get-card $ "r")))
                      (get-hand $ "p1")
                      (count $))
                1)
           )}
  ;definition is required because it gets around the problem of here card-id is for a card, but in play-minion-card
  ;the card will be played and becomes a minion
  ([state player-id card-id minion-def]
   (if (contains? minion-def :on-play)
     (let [on-play-fn (:on-play minion-def)]
       (if (and (contains? minion-def :sub-type)
                (= (:sub-type minion-def) :secret))
         (-> (on-play-fn state player-id card-id)
             (on-secret-played-fns))
         (on-play-fn state player-id card-id)))
     state))
  ([state player-id card-id minion-def target-id]
   (if (contains? minion-def :on-play)
     (let [on-play-fn (:on-play minion-def)]
       (if (and (contains? minion-def :sub-type)
                (= (:sub-type minion-def) :secret))
         (-> (on-play-fn state player-id card-id target-id)
             (on-secret-played-fns))
         (on-play-fn state player-id card-id target-id)))
     state)))

(defn has-deathrattle
  {:test (fn []
           ;without state and with card
           (is (-> (create-card "Madicken" :id "m")
                   (has-deathrattle)))
           (is-not (-> (create-card "Mio")
                       (has-deathrattle)))
           ;with state and card-id
           (is (-> (create-game [{:minions [(create-card "Madicken" :id "m")]}])
                   (has-deathrattle "m")))
           (is-not (-> (create-game [{:minions [(create-card "Mio" :id "m")]}])
                       (has-deathrattle "m")))
           )}
  ([card]
   (let [permanent-set (get-in (get-definition card) [:properties :permanent])]
     (contains? permanent-set "deathrattle")))
  ([state card-id]
   (let [permanent-set (get-in (get-definition (get-minion state card-id)) [:properties :permanent])]
     (contains? permanent-set "deathrattle"))))

(defn get-minions-with-deathrattle
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-card "Madicken" :id "ma")]}
                                  {:minions [(create-card "Mio" :id "mi")
                                             (create-card "Uncle Nilsson" :id "n")]}])
                    (get-minions-with-deathrattle)
                    (count))
                2)
           (is= (-> (create-game [{:minions [(create-card "Ronja" :id "r")]}
                                  {:minions [(create-card "Mio" :id "m")
                                             (create-card "Emil" :id "e")]}])
                    (get-minions-with-deathrattle)
                    (count))
                0)
           )}
  [state]
  (->> (get-minions state)
       (filter has-deathrattle)))

(defn get-dead-minions-with-deathrattle
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-card "Madicken" :id "ma" :damage-taken 3)]}
                                    {:minions [(create-card "Mio" :id "mi" :damage-taken 4)
                                               (create-card "Uncle Nilsson" :id "n" :damage-taken 0)]}]) $
                      (get-dead-minions-with-deathrattle $)
                      (map :id $))
                ["ma"])
           (is (as-> (create-game [{:minions [(create-card "Madicken" :id "ma" :damage-taken 0)]}
                                   {:minions [(create-card "Mio" :id "mi" :damage-taken 0)
                                              (create-card "Uncle Nilsson" :id "n" :damage-taken 0)]}]) $
                     (get-dead-minions-with-deathrattle $)
                     (map :id $)
                     (empty? $)))
           )}
  [state]
  (let [dead-minions (get-dead-minions state)]
    (filter has-deathrattle dead-minions)))


;performs deathrattle for a minion that has a deathrattle
(defn do-deathrattle
  {:test (fn []
           ;check madicken summons elisabeth
           (is= (-> (create-game [{:minions [(create-minion "Madicken" :id "m")]}])
                    (do-deathrattle "m")
                    (get-minions "p1")
                    (first)
                    (:name))
                "Elisabeth")
           ;check Uncle Nilsson
           (is= (-> (create-game [{:minions [(create-minion "Uncle Nilsson" :id "n")]}
                                  {:minions [(create-minion "Mio" :id "m")]}])
                    (do-deathrattle "n")
                    (get-minions "p2")
                    (count))
                0)
           )}

  [state card-id]
  (let [deathrattle (:deathrattle (get-definition (:name (get-minion state card-id))))]
    (deathrattle state card-id)))

(defn do-deathrattles
  {:test (fn []
           ;check madicken summons elisabeth
           (is= (as-> (create-game [{:minions [(create-card "Madicken" :id "m1")
                                               (create-card "Madicken" :id "m2")]}]) $
                      (do-deathrattles $ "m1" "m2")
                      (get-minions $ "p1")
                      (map :name $))
                ["Elisabeth" "Elisabeth"])
           ;check Uncle Nilsson
           (is= (-> (create-game [{:minions [(create-card "Uncle Nilsson" :id "n")]}
                                  {:minions [(create-card "Mio" :id "m")]}])
                    (do-deathrattles "n")
                    (get-minions "p2")
                    (count))
                0)
           )}

  [state & card-ids]
  (reduce do-deathrattle state card-ids))

(defn remove-dead-minions
  "Removes dead minions from the state and applies deathrattles recursively"
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                               (create-minion "Mio" :id "m2" :damage-taken 2)]}
                                    {:minions [(create-minion "Mio" :id "m3" :damage-taken 3)
                                               (create-minion "Mio" :id "m4" :damage-taken 1)]}]) $
                      (remove-dead-minions $)
                      (get-minions $)
                      (map :id $))
                ["m1" "m4"])
           (is= (as-> (create-game [{:minions [(create-minion "Madicken" :id "m1" :damage-taken 2)]}
                                    {:minions [(create-minion "Mio" :id "m2" :damage-taken 1)]}]) $
                      (remove-dead-minions $)
                      (get-minions $)
                      (map :name $))
                ["Elisabeth" "Mio"])
           )}
  [state]
  (let [dead-minions (map :id (get-dead-minions state))]
      (let [dead-deathrattle-minions (map :id (get-dead-minions-with-deathrattle state))]
        (as-> state $
              (reduce do-deathrattles $ dead-deathrattle-minions)
              (reduce remove-minions $ dead-minions)))))
  ;            (reduce remove-minions $ dead-minions)
  ;(let [dead-minions (map :id (get-dead-minions state))]
  ;  (let [dead-deathrattle-minions (map :id (get-dead-minions-with-deathrattle state))]
  ;    (if (empty? dead-deathrattle-minions)
  ;      (reduce remove-minions state dead-minions)
  ;      (as-> state $
  ;            (reduce do-deathrattles $ dead-deathrattle-minions)
  ;            (reduce remove-minions $ dead-minions)
  ;            (remove-dead-minions $)
  ;            )))))

;deal damage to a character and remove dead minions
(defn deal-damage
  {:test (fn []
           ;test when damage amount is not provided
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (deal-damage "e")
                    (get-minion "e")
                    (:damage-taken))
                1)
           ;test when damage amount is provided
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (deal-damage "e" 3)
                    (get-minion "e")
                    (:damage-taken))
                3)
           ;test to see if amount of damage taken is updated well
           (is= (-> (create-game [{:minions [(create-minion "Jonatan" :id "j" :damage-taken 1)]}])
                    (deal-damage "j" 3)
                    (get-minion "j")
                    (:damage-taken))
                4)
           ;test to see if dead minions are removed
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (deal-damage "m" 3)
                    (get-minions)
                    (count))
                0)
           ;test that minion is not damaged when it has divine shield, AND that the shield is removed
           (is= (-> (create-game [{:minions [(create-minion "Ronja" :id "r")]}])
                    (give-divine-shield "r")
                    (deal-damage "r")
                    (get-minion "r")
                    (:damage-taken))
                0)
           (is= (-> (create-game [{:minions [(create-minion "Madicken" :id "m")]}])
                    (deal-damage "m" 2)
                    (get-minion "m")
                    (:damage-taken))
                0)
           ;test to see that Ida gets taunt when a minion is damaged
           (is= (-> (create-game [{:minions [(create-minion "Pippi" :id "p")
                                             (create-minion "Ida" :id "i")]}])
                    (deal-damage "p")
                    (has-taunt? "i"))
                true)

           ;test to see that Ida does not get taunt when a minion with divine shield is attacked
           (is= (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")
                                             (create-minion "Ida" :id "i")]}])
                    (deal-damage "e")
                    (has-taunt? "i"))
                false)

           (is= (-> (create-game [{:hero (create-hero "Carl")}])
                    (deal-damage "h1")
                    (get-hero "h1")
                    (:damage-taken))
                1)
           )}
  ([state character-id]
   (deal-damage state character-id 1))
  ;Deal specified amount of damage to the given character
  ([state character-id damage-amount]
   (as-> state $
         (let [character (get-character $ character-id)]
           (if (minion? character)
             ;character has no divine shield
             (if-not (has-divine-shield? $ character-id)
               (-> (update-minion $ character-id :damage-taken (fn [x] (+ x damage-amount)))
                   (do-game-event-functions :on-minion-damage)
                   (remove-dead-minions))
               ;minion has divine shield
               (remove-divine-shield $ character-id))
             ;when character is hero
             (update-in $ [:players (:owner-id character) :hero :damage-taken] (fn [x] (+ x damage-amount))))))))


(defn deal-damage-to-all-heroes
  "Deals damage to all heroes"
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Carl" :id "h1")}])
                    (deal-damage-to-all-heroes 1)
                    (get-hero "h1")
                    (:damage-taken))
                1))}
  [state damage]
  (reduce (fn [state hero]
            (deal-damage state (:id hero) damage))
          state
          (get-heroes state)))

(defn deal-damage-to-all-minions
  "Deals damage to all minions"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")]}])
                    (deal-damage-to-all-minions 2)
                    (get-minion "e")
                    (:damage-taken))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")]}])
                    (deal-damage-to-all-minions 1)
                    (get-minion "m")
                    (:damage-taken))
                1))}
  [state damage]
  (reduce (fn [state minion]
            (deal-damage state (:id minion) damage))
          state
          (get-minions state)))

(defn deal-damage-to-other-minions
  "Deals damage to all other minions except given one"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")]}])
                    (deal-damage-to-other-minions "m" 2)
                    (get-minion "e")
                    (:damage-taken))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                             (create-minion "Emil" :id "e")]}])
                    (deal-damage-to-other-minions "m" 2)
                    (get-minion "m")
                    (:damage-taken))
                0))}
  [state minion-id damage]
  (reduce (fn [state minion]
            (if (= (:id minion) minion-id)
              state
              (deal-damage state (:id minion) damage)))
          state
          (get-minions state)))

(defn pay-mana
  {:test (fn []
           ; minion card
           (is= (-> (create-game [{:hand    [(create-card "Radar Raid" :id "r")
                                             (create-card "Emil" :id "e")]
                                   :minions [(create-minion "Alfred" :id "a")]}])
                    (pay-mana "p1" "e")
                    (get-mana "p1"))
                6)
           ; spell card
           (is= (-> (create-game [{:hand    [(create-card "Radar Raid" :id "r")
                                             (create-card "Emil" :id "e")]
                                   :minions [(create-minion "Alfred" :id "a")]}])
                    (pay-mana "p1" "r")
                    (get-mana "p1"))
                8)
           ;hero power
           (is= (-> (create-game [{:hand    [(create-card "Radar Raid" :id "r")
                                             (create-card "Emil" :id "e")]
                                   :minions [(create-minion "Alfred" :id "a")]}])
                    (pay-mana "p1" "h2")
                    (get-mana "p1"))
                8))}
  [state player-id id]
  (update-mana state player-id (fn [old-value] (- old-value (get-mana-cost state id)))))

(defn decrement-minion-temporary-property-durations
  "Decrements the duration of all temporary properties of a minion and removes if = 0"
  {:test (fn []
           ;duration != 0 after decrement, so property should stay
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                      (give-property $ "m" "taunt" 2)
                      (give-property $ "m" "divine-shield" 2)
                      (decrement-minion-temporary-property-durations $ "m")
                      (get-minion-properties $ "m")
                      (:temporary $)
                      (map last $))
                [1, 1])
           ;duration = 0 after decrement, so property should be removed
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                      (give-property $ "m" "taunt" 1)
                      (give-property $ "m" "divine-shield" 1)
                      ;(:temporary (get-minion-properties $ "m"))
                      ;(reduce (fn [property-list property]
                      ;          (update property-list property dec))
                      ;        $ (map first $))
                      ;(select-keys $ (map first (filter #(> (last %) 0) $))))
                      ;(filter #(> (last %) 0) $)
                      (decrement-minion-temporary-property-durations $ "m")
                      (get-minion-properties $ "m")
                      (:temporary $))
                {})
           )}
  [state minion-id]
  (let [minion-temporary-properties (:temporary (get-minion-properties state minion-id))]
    (let [new-properties (reduce (fn [property-list property]
                                   (update property-list property dec))
                                 minion-temporary-properties (map first minion-temporary-properties))]
      (let [filtered-new-properties (select-keys new-properties (map first (filter #(> (last %) 0) new-properties)))]
        (update-minion state minion-id :properties
                       (fn [properties-map]
                         (assoc properties-map :temporary filtered-new-properties)))))))

(defn decrement-minion-temporary-stat-durations
  "Decrements the duration of all temporary stats of a minion and removes if = 0"
  {:test (fn []
           ;duration != 0 after decrement, so property should stay
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                      (modify-minion-stats $ "m" 2 2 2)
                      (modify-minion-stats $ "m" 2 2)
                      ;(map :duration (:attack(:stats (get-properties $ "m")))))
                      ;(get-minion-properties $ "m")
                      ;(:stats $)
                      ;(:attack $)
                      ;(first $)
                      ; (update-in (first (:attack (:stats (get-minion-properties $ "m")))) [:duration] dec))
                      ;(:attack (:stats (get-minion-properties $ "m")))
                      ;(first $)
                      ;(:duration $))
                      ;(map (fn [buff] (if (:duration buff)(update-in buff [:duration] dec) buff)) (:attack (:stats (get-minion-properties $ "m")))))
                      ;(filter #(> (last (last %)) 0) $))

                      ;(reduce (fn [property-list property]
                      ;          (update-in property [:duration property] dec))
                      ;  (:attack (:stats (get-minion-properties $ "m"))) (:attack(:stats (get-minion-properties $ "m")))))
                      ;;(get-properties $ "m")
                      ;;(:stats $)
                      ;;(:attack $)
                      ;;(first $)
                      ;;(:duration $))

                      (decrement-minion-temporary-stat-durations $ "m")
                      (get-minion-stats $ "m"))
                [5, 6])
           ;duration = 0 after decrement, so property should be removed
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                      (modify-minion-stats $ "m" 2 2 1)
                      (modify-minion-stats $ "m" 2 2)
                      (decrement-minion-temporary-stat-durations $ "m")
                      (get-minion-stats $ "m"))
                [3, 4])
           )}
  [state minion-id]
  (let [minion-stats (:stats (get-minion-properties state minion-id))]
    (let [attack-stats (:attack minion-stats)
          health-stats (:health minion-stats)]
      (let [new-attack (map (fn [buff] (if (:duration buff) (update-in buff [:duration] dec) buff))
                            attack-stats)
            new-health (map (fn [buff] (if (:duration buff) (update-in buff [:duration] dec) buff))
                            health-stats)]
        (let [filtered-new-attack (filter #(> (last (last %)) 0) new-attack)
              filtered-new-health (filter #(> (last (last %)) 0) new-health)]
          (update-minion state minion-id :properties
                         (fn [properties-map]
                           (assoc properties-map :stats
                                                 {:attack filtered-new-attack :health filtered-new-health}))))))))

(defn decrement-minion-temporary-durations
  "Decrements all minion temporary properties"
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                      (modify-minion-stats $ "m" 2 2 2)
                      (decrement-minion-temporary-durations $ "m")
                      (get-minion-stats $ "m"))
                [3, 4])
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                      (give-property $ "m" "taunt" 2)
                      (decrement-minion-temporary-durations $ "m")
                      (get-minion-properties $ "m")
                      (:temporary $)
                      (first $))
                [:taunt 1])
           (is= (as-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]) $
                      (give-property $ "m" "taunt" 1)
                      (decrement-minion-temporary-durations $ "m")
                      (get-minion-properties $ "m")
                      (:temporary $))
                {})
           )}
  [state minion-id]
  (-> state
      (decrement-minion-temporary-property-durations minion-id)
      (decrement-minion-temporary-stat-durations minion-id)))

(defn decrement-all-player-minion-temporary-durations
  "Decrements the temporary properties of all minions belonging to a player"
  {:test (fn []
           (as-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                          (create-minion "Emil" :id "e")]}
                               {:minions [(create-minion "Ronja" :id "r")]}]) $
                 (modify-minion-stats $ "m" 2 2 2)
                 (modify-minion-stats $ "r" 2 2 1)
                 (decrement-all-player-minion-temporary-durations $ "p1")
                 (do (is= (get-minion-stats $ "m") [3 4])
                     (is= (get-minion-stats $ "e") [2 5])
                     (is= (get-minion-stats $ "r") [5, 4])))
           (as-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                          (create-minion "Emil" :id "e")]}
                               {:minions [(create-minion "Ronja" :id "r")]}]) $
                 (modify-minion-stats $ "m" 2 2 1)
                 (modify-minion-stats $ "e" 2 2 1)
                 (modify-minion-stats $ "r" 2 2 1)
                 (decrement-all-player-minion-temporary-durations $ "p1")
                 (do (is= (get-minion-stats $ "m") [1 2])
                     (is= (get-minion-stats $ "e") [2 5])
                     (is= (get-minion-stats $ "r") [5, 4])))
           (as-> (create-game [{:minions [(create-minion "Mio" :id "m")
                                          (create-minion "Emil" :id "e")]}
                               {:minions [(create-minion "Ronja" :id "r")]}]) $
                 (give-property $ "m" "taunt" 1)
                 (give-property $ "e" "taunt" 1)
                 (give-property $ "r" "taunt" 1)
                 (decrement-all-player-minion-temporary-durations $ "p1")
                 (do (is-not (has-taunt? $ "m"))
                     (is-not (has-taunt? $ "e"))
                     (is (has-taunt? $ "r"))))
           )}
  [state player-id]
  (let [minion-ids (map :id (get-minions state player-id))]
    (reduce (fn [state-map minion-id] (decrement-minion-temporary-durations state-map minion-id)) state minion-ids)))