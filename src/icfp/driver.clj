(ns icfp.driver
  (:gen-class)
  (:require [icfp.io :as io]
            [icfp.ai :as ai]))

(defn -main
  [& args]
  (let [game-ref (ref (io/read-game-state *in*))]
    (try

      (let [result (ai/run-ai ai/a*-ai game-ref)
            moves  (apply str (map #(str (name %)) (:moves @game-ref)))]
        (println moves))

      (catch Exception e
        (binding [*out* *err*]
          (.printStackTrace e))))))
