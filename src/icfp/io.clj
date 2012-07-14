(ns icfp.io
  (:use [icfp.core :only [game-state]]))

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

(defn find-objects [board sym]
  (into #{}
    (for [[x ys] board
          [y val] ys
          :when (= val sym)]
      [x y])))

(defn get-object-locations [board]
  {
    :lambdas (find-objects board :>)
    :rocks (find-objects board :*)
    :robot (first (find-objects board :R))
    :lift (first (find-objects board :L))
  }
  )

(defn build-board [rows]
  (let [numrows (count rows)
       row-hashes (map-indexed
                      #(vec [(- numrows %1) (line-seq-to-hash %2)])
                      rows)]
      (reduce
        (fn [result [x y val]] (assoc-in result [x y] val))
        {}
        (for [[y row] row-hashes
              [x val] row]
          [x y val]))))

(defn read-game-state [rdr]
  (let [map-file-lines (get-map-file-lines rdr)
        rows (map line-to-keyword-seq (:map-lines map-file-lines))
        metadata (metadata-lines-to-map (:metadata-lines map-file-lines))
        board (build-board rows)
        water (metadata "Water")
        flooding (metadata "Flooding")
        waterproof (metadata "Waterproof")
        ; TODO: would be more efficient to do a binding or something,
        ;  and gather information about the object locations while
        ;  we are parsing the lines.  For now, just iterating after
        ;  the fact.
        object-locations (get-object-locations board)
        ]
      (struct-map game-state
        :board board
        :status :alive
        :lambdas (:lambdas object-locations)
        :rocks (:rocks object-locations)
        :robot (:robot object-locations)
        :lift (:lift object-locations)
        :score 0
        :moves []
        :water water
        :flooding flooding
        :waterproof waterproof
        )))


(defn read-game-state-from-file [path]
  (with-open
    [rdr (clojure.java.io/reader path)]
    (let [game-state (read-game-state rdr)]
;      (println game-state)
      game-state)))

(defn convert-back-to-orig-game-char [c]
  (condp = c
    "_" " "
    ">" "\\"
    c))

(defn pretty-format-row [row]
  (clojure.string/join "" (map convert-back-to-orig-game-char (map name row))))

(defn sorted-row-numbers [board]
  (sort (keys board)))

(defn columns-in-row-order [board]
  (map #(get board %) (sorted-row-numbers board)))

(defn extract-row-in [board]
  (fn [row-number]
    (map #(get % row-number) (columns-in-row-order board))))

(defn rows-for [row-numbers board]
  (map (extract-row-in board) row-numbers))

(defn print-game-state [game-state]
  (let [
         board (:board game-state)
         row-numbers-in-display-order (reverse (sorted-row-numbers board))
         rows (rows-for row-numbers-in-display-order board)
         ]
    (println (clojure.string/join "\n" (map pretty-format-row rows)))))

