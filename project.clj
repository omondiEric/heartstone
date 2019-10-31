(defproject firestone "firestone"
  :description "A toy project at Williams College restricted to students of the course."
  :license {}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/test.check "0.9.0"]
                 [ysera "2.0.1"]
                 [http-kit "2.3.0"]
                 [org.clojure/data.json "0.2.6"]]
  :main ^:skip-aot firestone.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
