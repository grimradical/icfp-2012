(ns icfp.driver
  (:gen-class)
  (:require [icfp.io :as io]
            [icfp.ai :as ai]))

(defn -main
  [& args]
  (.addShutdownHook (Runtime/getRuntime) (Thread. #(flush)))
  (let [game-ref (ref (io/read-game-state *in*))]
    (try
      (let [game-state (ai/run-ai ai/a*-ai game-ref)
            moves      (apply str (map #(str (name %)) (:moves @game-ref)))]
        (print moves))
      (catch Exception e
        (binding [*out* *err*]
          (.printStackTrace e))))))
