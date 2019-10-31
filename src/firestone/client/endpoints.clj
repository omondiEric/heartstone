(ns firestone.client.endpoints
  (:require [clojure.string :refer [starts-with?]]
            [clojure.data.json :refer [read-json write-str]]
            [firestone.client.edn-api :refer [create-game!]]))



(defn create-response
  [response]
  {:status  200
   :headers {"Content-Type" "text/json; charset=utf-8"
             "Access-Control-Allow-Origin"  "*"
             "Access-Control-Allow-Methods" "*"}
   :body    (write-str response)})

(defn handler [request]

  (let [uri (:uri request)]

    (cond (starts-with? uri "/createGame")
          (time (create-response (create-game!)))

          (starts-with? uri "/endTurn")
          (let [params (read-json (slurp (:body request)))]
            (create-response (clojure.string/upper-case (:uri request)))))))

