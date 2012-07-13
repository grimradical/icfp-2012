(ns icfp.chris_scratch.readmap)

(defn char-to-keyword [c]
  (keyword
    (condp = c
      " " "_"
      "\\" ">"
      c)))

(defn line-to-keyword-seq [line]
  (map char-to-keyword (map str line)))

(defn read-nested-seqs [rdr]
    (map line-to-keyword-seq (clojure.string/split-lines (slurp rdr))))

(defn line-seq-to-hash [line]
  (into {} (map-indexed #(vec [(+ 1 %1) %2]) line)))

(defn read-map [rdr]
  (let [myarr (read-nested-seqs rdr)]
    (into {} (map-indexed #(vec [(- (count myarr) %1) (line-seq-to-hash %2)]) myarr))))

(with-open
  [rdr (clojure.java.io/reader *in*)]
  (println (read-map rdr)))
;
;(with-open
;  [rdr (clojure.java.io/reader "/home/cprice/work/puppet/icfp/icfp-2012/resources/maps/map1.txt")]
;  (println (read-map rdr)))