(ns firestone.definitions
  (:require [ysera.test :refer [is is= is-not error?]]
            [ysera.error :refer [error]]))

; Here is where the definitions are stored
(defonce definitions-atom (atom {}))

(defn add-definitions!
  "Adds the given definitions to the game."
  [definitions]
  (swap! definitions-atom merge definitions))

(defn get-definitions
  "Returns all definitions in the game."
  []
  (vals (deref definitions-atom)))

(defn get-definition
  "Gets the definition identified by the name."
  {:test (fn []
           (is= (get-definition "Mio")
                {:name "Mio"
                 :attack 1
                 :health 2
                 :mana-cost 1
                 :properties #{}
                 :end-of-turn false
                 :type :minion})
           ; The name can be present in a map with :name as a key
           (is= (get-definition {:name "Mio"})
                (get-definition "Mio"))

           (error? (get-definition "Something that does not exist")))}
  [name-or-entity]
  {:pre [(or (string? name-or-entity)
             (and (map? name-or-entity)
                  (contains? name-or-entity :name)))]}
  (let [name (if (string? name-or-entity)
               name-or-entity
               (:name name-or-entity))
        definitions (deref definitions-atom)
        definition (get definitions name)]
    (when (nil? definition)
      (error (str "The name " name-or-entity " does not exist. Are the definitions loaded?")))
    definition))