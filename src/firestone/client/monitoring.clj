(ns firestone.client.monitoring
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :refer [instrument]]
            [firestone.client.endpoints]
            [firestone.client.spec]))


(s/fdef firestone.client.endpoints/create-response
        :args (s/cat :game-states :firestone.client.spec/game-states))

(instrument 'firestone.client.endpoints/create-response)