(ns firestone.core
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.error :refer [error]]
            [ysera.collections :refer [seq-contains?]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [create-card
                                         create-game
                                         create-hero
                                         create-minion
                                         get-heroes
                                         get-minion
                                         get-minions]]))

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
  (let [minion (get-minion state id)
        definition (get-definition (:name minion))]
    (:attack definition)))

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
                       (valid-attack? "p1" "m" "r"))))}
  [state player-id attacker-id target-id]
  (let [attacker (get-minion state attacker-id)
        target (get-character state target-id)]
    (and attacker
         target
         (= (:player-id-in-turn state) player-id)
         (< (:attacks-performed-this-turn attacker) 1)
         (not (sleepy? state attacker-id))
         (not= (:owner-id attacker) (:owner-id target)))))
