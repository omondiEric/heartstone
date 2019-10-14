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
                                         get-card
                                         get-heroes
                                         get-hero
                                         get-hand
                                         get-minion
                                         get-minions
                                         get-other-player-id
                                         get-random-minion
                                         give-divine-shield
                                         give-taunt
                                         has-divine-shield
                                         has-taunt?
                                         ida-present?
                                         minion?
                                         replace-minion
                                         remove-card-from-deck
                                         remove-divine-shield
                                         remove-minion
                                         remove-minions
                                         switch-minion-side
                                         update-minion]]))

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
   (get-health (get-character state id))))

(defn get-attack
  "Returns the attack of the minion with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (get-attack "m"))
                1))}
  [state id]
  (let [minion (get-minion state id)]
    (:attack minion)))

(defn sleepy?
  "Checks if the minion with given id is sleepy."
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}]
                                :minion-ids-summoned-this-turn ["m"])
                   (sleepy? "m")))
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                       (sleepy? "m"))))}
  [state id]
  (seq-contains? (:minion-ids-summoned-this-turn state) id))

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
                                 {:minions [(create-minion "Ronja" :id "r")]}])
                   (valid-attack? "p1" "m" "r")))
           ; Should be able to attack an enemy hero
           (is (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
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
                                 {:minions [(create-minion "Jonatan" :id "j")]}])
                   (valid-attack? "p1" "m" "j")))
           ; Should not be able to attack if target minion does not have taunt, but other enemy minions do
           (is-not (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                     {:minions [(create-minion "Ronja" :id "r")
                                                (create-minion "Elisabeth" :id "e")]}])
                       (valid-attack? "p1" "m" "r"))))}
  [state player-id attacker-id target-id]
  (let [attacker (get-minion state attacker-id)
        target (get-character state target-id)]
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
         (not (contains? (:properties attacker) "NoAttack"))
         (= (:player-id-in-turn state) player-id)
         (< (:attacks-performed-this-turn attacker) 1)
         (not (sleepy? state attacker-id))
         (> (get-attack state attacker-id) 0)
         (not= (:owner-id attacker) (:owner-id target)))))

(defn do-battlecry
  "Returns the battlecry function of a minion or nil"
  {:test (fn []
           ;check that damage is taken for Kato
           (is= (-> (create-game [{:hand [(create-card "Kato" :id "k")]}])
                    (do-battlecry "p1" (create-card "Kato"))
                    (do-battlecry "p2" (create-card "Kato"))
                    (do-battlecry "p2" (create-card "Kato"))
                    (do-battlecry "p2" (create-card "Kato"))
                    (get-in [:players "p1" :hero :damage-taken]))
                12)
           ;check that card is drawn for Emil
           (is= (-> (create-game [{:hand [(create-card "Emil" :id "e")] :deck [(create-card "Mio" :id "m")]}])
                    (do-battlecry "p1" (create-card "Emil"))
                    (get-hand "p1")
                    (count))
                2)
           ;check that Ronja will cause no errors
           (is= (-> (create-game)
                    (do-battlecry "p1" (create-card "Ronja"))
                    (get-hand "p1")
                    (count))
                0)
           )}
  [state player-id card]
  (if (contains? (get-definition card) :battlecry)
    (let [battlecry (:battlecry (get-definition card))]
      (battlecry state player-id))
    state))

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
   (contains? (get-definition card) :deathrattle))
  ([state card-id]
   (contains? (get-definition (get-minion state card-id)) :deathrattle)))

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
           (is= (-> (create-game [{:minions [(create-card "Madicken" :id "m")]}])
                    (do-deathrattle "m")
                    (get-minions "p1")
                    (first)
                    (:name))
                "Elisabeth")
           ;check Uncle Nilsson
           (is= (-> (create-game [{:minions [(create-card "Uncle Nilsson" :id "n")]}
                                  {:minions [(create-card "Mio" :id "m")]}])
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
      (if (empty? dead-deathrattle-minions)
        (reduce remove-minions state dead-minions)
        (as-> state $
              (reduce do-deathrattles $ dead-deathrattle-minions)
              (remove-dead-minions $)
              )))))


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
           ;test to see that Ida gets taunt when a minion is damaged
           (is (-> (create-game [{:minions [(create-minion "Pippi" :id "p")
                                            (create-minion "Ida" :id "i")]}])
                   (deal-damage "p")
                   (get-minion "i")
                   (:properties)
                   (contains? "Taunt")))

           ;test to see that Ida does not get taunt when a minion with divine shield is attacked
           (is-not (-> (create-game [{:minions [(create-minion "Elisabeth" :id "e")
                                                (create-minion "Ida" :id "i")]}])
                       (deal-damage "e")
                       (get-minion "i")
                       (:properties)
                       (contains? "Taunt")))

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
             ;when character is minion and has no divine shield
             (if-not (has-divine-shield $ character-id)
               (do
                 ;ida not on board
                 (if (= (ida-present? $) nil)
                   (-> (update-minion $ character-id :damage-taken (fn [x] (+ x damage-amount)))
                       (remove-dead-minions))
                   ;ida on board
                   (-> (update-minion $ character-id :damage-taken (fn [x] (+ x damage-amount)))
                       (give-taunt (:id (ida-present? $))))))

               ;minion has divine shield
               (remove-divine-shield $ character-id))
             ;when character is hero
             (do
               (update-in $ [:players (:owner-id character) :hero :damage-taken] (fn [x] (+ x damage-amount)))))))))

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

(-> (create-game)
    (get-random-minion))