(ns firestone.client.server
  (:require [org.httpkit.server :refer [run-server]]
            [firestone.client.endpoints :refer [handler]]))

(defonce server-atom (atom nil))

(defn start! []
  (reset! server-atom (run-server (fn [request]
                                    (handler request))
                                  {:port 8001})))

(defn stop! []
  (when (deref server-atom)
    ((deref server-atom) :timeout 100)
    (reset! server-atom nil)))

(comment

  (start!)

  (stop!)

  )