(ns firestone.client.endpoints
  (:require [clojure.string :refer [starts-with?]]
            [clojure.data.json :refer [read-json write-str]]
            [firestone.client.edn-api :refer [create-game!
                                              end-turn!
                                              play-minion-card!
                                              play-spell-card!]]))



(defn create-response
  [response]
  {:status  200
   :headers {"Content-Type"                 "text/json; charset=utf-8"
             "Access-Control-Allow-Origin"  "*"
             "Access-Control-Allow-Methods" "*"}
   :body    (write-str response)})

(defn handler [request]

  (let [uri (:uri request)]

    (cond (starts-with? uri "/createGame")
          (time (create-response (create-game!)))

          (starts-with? uri "/endTurn")
          (let [params (read-json (slurp (:body request)))
                player-id (:playerId params)]
            (create-response (end-turn! player-id)))

          (starts-with? uri "/playMinionCard")
          (let [params (read-json (slurp (:body request)))
                player-id (:playerId params)
                card-id (:cardId params)
                target-id (:targetId params)
                position (:position params)]
            (create-response (play-minion-card! player-id card-id position target-id)))
          (starts-with? uri "/playSpellCard")
          (let [params (read-json (slurp (:body request)))
                player-id (:playerId params)
                card-id (:cardId params)
                target-id (:targetId params)]
            (create-response (play-spell-card! player-id card-id target-id)))

          :else (create-response (clojure.string/lower-case (:uri request))))))

