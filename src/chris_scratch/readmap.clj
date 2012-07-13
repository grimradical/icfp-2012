(ns icfp.chris_scratch.readmap)

(defn convert-char [c]
  (keyword (str c)))

(defn line-to-char-array [line]
  (loop [result []
         c (first line)
         line (rest line)]
    (if (empty? line) result
      (recur
        (concat result [(convert-char c)])
        (first line)
        (rest line)))))

(defn read-map [rdr]
;  (println "Entering read-map")
;  (println
    (loop [result []
           line (.readLine rdr)]
      (if (nil? line) result
        (recur
          (concat result [(line-to-char-array line)])
          (.readLine rdr))))
;  )
;  (println "Exiting read-map")
  )

;(read-map (clojure.java.io/reader *in*))
(with-open
  [rdr (clojure.java.io/reader "/home/cprice/work/puppet/icfp/icfp-2012/resources/maps/map1.txt")]
  (println (read-map rdr)))