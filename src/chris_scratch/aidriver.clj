(ns chris_scratch.aidriver
  (:use [icfp.io :only [read-game-state-from-file]]
         [icfp.ai :only [run-ai stupid-1]]))

(defn assert-function-and-meta [f]
  (assert (map? f))
  (assert (fn? (:f f)))
  (assert (map? (:m f)))
  (assert (symbol? (:name (:m f))))
  true)

(defn assert-functions-and-meta [fs]
  (every? assert-function-and-meta fs))

(defn to-function-and-meta-hash [fn-sym]
  { :pre [(symbol? fn-sym)]
    :post [(fn? (:f %))
           (map? (:m %))]}
  (hash-map :f (eval fn-sym) :m (meta (resolve fn-sym))))

(defn pretty-run-result [result-hash]
  (let [game (:game result-hash)]
    (format "%-20s %-12s %-12s (score: %5s) (moves: #%s %s)"
       (.getName (clojure.java.io/file (:map result-hash)))
       (:ai result-hash)
       (str (:elapsed result-hash) "ms")
       (:score game)
       (count (:moves game))
       (clojure.string/join (map name (:moves game)))
    )))

(defn run-multiple-maps*
  [fs map-paths]
  { :pre [(assert-functions-and-meta fs)] }
  (println
    (clojure.string/join "\n"
      (map pretty-run-result
        (for [f fs
                map-path map-paths]
          (let [final-result
                (let [start (java.lang.System/currentTimeMillis)
                      finish-expression '(java.lang.System/currentTimeMillis)
                      result (let [game-ref (ref (read-game-state-from-file map-path))]
                        {
                          :start start
                          :game (run-ai (:f f) game-ref)
                          :map map-path
                          :ai (:name (:m f))
                        })]
                    (assoc result :finish (eval finish-expression)))]
              (assoc final-result :elapsed (- (:finish final-result) (:start final-result)))
    ;           [final-result (:finish final-result) (:start final-result)]
            ))))))

(defn run-multiple-maps [fn-syms map-paths]
  { :pre [(every? symbol? fn-syms)]}
  (run-multiple-maps* (map to-function-and-meta-hash fn-syms) map-paths))

(defn run-all-local-maps* [fs]
  { :pre [(assert-functions-and-meta fs)] }
  (run-multiple-maps* fs (map #(.getCanonicalPath %) (sort (.listFiles (clojure.java.io/file "./resources/maps"))))))

(defn run-all-local-maps [fn-syms]
  { :pre [(every? symbol? fn-syms)]}
  (run-all-local-maps* (map to-function-and-meta-hash fn-syms)))

(defn stupid-2 [& args] (apply stupid-1 args))
