(ns icfp.io
  (:use [icfp.core :only [game-state game-over? step]]))

(def valid-map-chars #{"#" "\\" "." " " "R" "L" "*"})

(defn char-to-keyword [c]
  (keyword
    (cond
      ; convert a few of the input symbols to things that are easier to
      ;  represent as literals in clojure
      (= c " ") "_"
      (= c "\\") ">"
      (valid-map-chars c) c
      (= c "@") :*
      (re-matches #"[A-I1-9]" c) :#
      :else c)))

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
  (into {} (remove #(#{"Trampoline"} (first %))
                   (map #(vec (split-metadata-line %1)) lines))))

(defn find-objects [board sym]
  (into #{}
    (for [[x ys] board
          [y val] ys
          :when (= val sym)]
      [x y])))

(defn get-object-locations [board]
  {:lambdas (find-objects board :>)
   :rocks (find-objects board :*)
   :robot (first (find-objects board :R))
   :lift (first (find-objects board :L))
   :beards (find-objects board :W)})

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
        growth (metadata "Growth")
        razors (metadata "Razors")
        ; TODO: would be more efficient to do a binding or something,
        ;  and gather information about the object locations while
        ;  we are parsing the lines.  For now, just iterating after
        ;  the fact.
        object-locations (get-object-locations board)]
      (struct-map game-state
        :board board
        :status :alive
        :lambdas (:lambdas object-locations)
        :rocks (:rocks object-locations)
        :robot (:robot object-locations)
        :lift (:lift object-locations)
        :beards (:beards object-locations)
        :score 0
        :moves []
        :water (if water (Integer/parseInt water) 0)
        :flooding (if flooding (Integer/parseInt flooding) 0)
        :waterproof (if waterproof (Integer/parseInt waterproof) 10)
        :growth (if growth (Integer/parseInt growth) 25)
        :g (dec (if growth (Integer/parseInt growth) 25))
        :razors (if razors (Integer/parseInt razors) 0))))

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
  (apply str (map convert-back-to-orig-game-char (map name row))))

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

(defn drive-via-keyboard
  [game-ref]
  (let [keytrans {"w" :U "a" :L "d" :R "s" :D "z" :W "x" :A}]
    (loop []
      (if (game-over? @game-ref)
        @game-ref
        (let [cmd (-> (read-line)
                      (read-string)
                      (str)
                      (keytrans))]
          (when cmd
            (dosync
             (alter game-ref step cmd)))
          (recur))))))
