(ns firestone.core
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.error :refer [error]]
            [ysera.collections :refer [seq-contains?]]
            [firestone.definitions :refer [get-definition]]
            [firestone.core-api :refer [draw-card]]
            [firestone.construct :refer [create-card
                                         create-game
                                         create-hero
                                         create-minion
                                         draw-card-to-hand
                                         has-taunt?
                                         get-heroes
                                         get-hero
                                         get-hand
                                         get-minion
                                         get-minions
                                         get-other-player-id
                                         remove-card-from-deck
                                         remove-minion
                                         remove-minions
                                         remove-taunt
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

; currently does not work D:
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

(defn attack-minion
  "Attacks the enemy minion"
  {:test (fn []
           ; Your minion's health should be updated
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}
                                  {:minions [(create-minion "Ronja" :id "r")]}])
                    (attack-minion "p1" "e" "r")
                    (get-health "e"))
                2)
           ; Target minion's health should be updated
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Ronja" :id "r")]}])
                    (attack-minion "p1" "m" "r")
                    (get-health "r"))
                1)
           ; Taunt should be removed after being attacked
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}
                                  {:minions [(create-minion "Jonatan" :id "j")]}])
                    (attack-minion "p1" "m" "j")
                    (has-taunt? "j"))
                false)
           ; Your minion's attacks for this turn should be updated
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}
                                  {:minions [(create-minion "Ronja" :id "r")]}])
                    (attack-minion "p1" "e" "r")
                    (get-minion "e")
                    (:attacks-performed-this-turn))
                1))}
  [state player-id attacker-id target-id]
  (let [attacker-attack-val (get-attack state attacker-id)
        target-attack-val (get-attack state target-id)]
    (if (valid-attack? state player-id attacker-id target-id) (-> (update-minion state attacker-id :damage-taken (+ target-attack-val))
                                                                  (update-minion target-id :damage-taken (+ attacker-attack-val))
                                                                  (remove-taunt target-id)
                                                                  (update-minion attacker-id :attacks-performed-this-turn inc)))))
(defn remove-dead-minions
  "Removes dead minions at a given state and returns all living minions"
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m1")
                                             (create-minion "Mio" :id "m2" :damage-taken 2)]}
                                  {:minions [(create-minion "Mio" :id "m3" :damage-taken 3)
                                             (create-minion "Mio" :id "m4" :damage-taken 1)]}])
                    (->> (remove-dead-minions)
                         (map :id))
                    )
                ["m1" "m4"]))}
  [state]
  (->> (get-minions state)
       (filter (fn [m] (> (->> (:id m) (get-health state)) 0)))))

(defn attack-hero
  "Attacks the enemy hero"
  {:test (fn []
           ; The enemy hero's health should be updated
           (is= (-> (create-game [{:minions [(create-minion "Mio" :id "m")]}])
                    (attack-hero "p1" "m" "h2")
                    (get-health "h2"))
                29)
           ; Your minion's attacks for this turn should be updated
           (is= (-> (create-game [{:minions [(create-minion "Emil" :id "e")]}])
                    (attack-hero "p1" "e" "h2")
                    (get-minion "e")
                    (:attacks-performed-this-turn))
                1))}
  [state player-id attacker-id target-hero-id]
  (let [attacker-attack-val (get-attack state attacker-id)
        target-player-id (get-other-player-id player-id)]
    (when (valid-attack? state player-id attacker-id target-hero-id)
      (-> (update-in state
                     [:players target-player-id :hero :damage-taken]
                     (fn [old-damage-taken]
                       (+ old-damage-taken attacker-attack-val)))
          (update-minion attacker-id :attacks-performed-this-turn inc)))))