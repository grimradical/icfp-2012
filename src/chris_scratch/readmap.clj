(ns icfp.chris_scratch.readmap
  (:require icfp.core))

(def valid-map-chars #{"#" "\\" "." " " "R" "L" "*"})

(defn is-valid-map-char? [c]
  (let [valid (not (nil? (valid-map-chars c)))]
    (if-not valid (println (format "Invalid map char '%s'" c)))
    valid))

(defn char-to-keyword [c]
  {  :pre [(is-valid-map-char? c)] }
  (keyword
    (condp = c
      ; convert a few of the input symbols to things that are easier to
      ;  represent as literals in clojure
      " " "_"
      "\\" ">"
      c)))

(defn line-to-keyword-seq [line]
  (map char-to-keyword (map str line)))

;(defstruct map-file-lines :map-lines :metadata-lines)

(defn find-empty-line-index [lines]
  (let [filtered (first (filter #(re-matches #"^\s*$" (second %)) (map-indexed vector lines)))]
    (if (empty? filtered)
      (count lines)
      (first filtered))))

(defn get-map-file-lines [rdr]
  (let [lines (clojure.string/split-lines (slurp rdr))
       empty-line-index (find-empty-line-index lines)]
    {
      :map-lines (subvec lines 0 empty-line-index)
      :metadata-lines (if (= (count lines) empty-line-index)
                        []
                        (subvec lines (+ 1 empty-line-index) (count lines)))
    }))

(defn line-seq-to-hash [line]
  (into {} (map-indexed #(vec [(+ 1 %1) %2]) line)))

(defn split-metadata-line [line]
  (clojure.string/split line #"\s+"))

(defn metadata-lines-to-map [lines]
  (into {} (map #(vec (split-metadata-line %1)) lines)))

(defn read-game-state [rdr]
  (let [map-file-lines (get-map-file-lines rdr)
        rows (map line-to-keyword-seq (:map-lines map-file-lines))
        numrows (count rows)
        metadata (metadata-lines-to-map (:metadata-lines map-file-lines))]
    (let [board (into {}
                  (map-indexed
                    #(vec [(- numrows %1) (line-seq-to-hash %2)])
                    rows))
         water (metadata "Water")
         flooding (metadata "Flooding")
         waterproof (metadata "Waterproof")
         ]
      (struct-map icfp.core/game-state
        :board board
        :water water
        :flooding flooding
        :waterproof waterproof
        :score 0
        )
    )))


(defn read-game-state-from-file [path]
  (with-open
    [rdr (clojure.java.io/reader path)]
    (println (read-game-state rdr))))

;(read-map-from-file *in*)
(read-game-state-from-file "/home/cprice/work/puppet/icfp/icfp-2012/resources/maps/contest1.map")
(read-game-state-from-file "/home/cprice/work/puppet/icfp/icfp-2012/resources/maps/flood5.map")
