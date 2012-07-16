(ns icfp.driver
  (:gen-class)
  (:require [icfp.io :as io]
            [icfp.ai :as ai]))

(defn -main
  [& args]
  (let [game-state (io/read-game-state *in*)]
    (try
      (ai/a*-ai game-state nil)
      (println)
      (flush)
      (catch Exception e
        (binding [*out* *err*]
          (.printStackTrace e))))))
