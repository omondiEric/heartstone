(ns firestone.definition.hero
  (:require [firestone.definitions :as definitions]))

(def hero-definitions
  {

   "Carl"
   {:name       "Carl"
    :type       :hero
    :health     30}

   "Gustaf"
   {:name       "Gustaf"
    :type       :hero
    :health     30}

   })

(definitions/add-definitions! hero-definitions)