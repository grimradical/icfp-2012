(defproject icfp "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [quil "1.6.0"]
                 [org.clojure/data.priority-map "0.0.1"]
                 [org.clojure/math.combinatorics "0.0.3"]]

  :aot [icfp.core]
  :main icfp.core
)
