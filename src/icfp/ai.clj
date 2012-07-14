(ns icfp.ai
  (:use icfp.core
        clojure.data.priority-map
        clojure.math.combinatorics))

(defn distance
  [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                (Math/pow (- y2 y1) 2))))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn cost
  [current start end]
  (let [g (manhattan-distance start current)
        h (manhattan-distance current end)
        f (+ g h)]
    [f g h]))

(defn edges
  [{:keys [board robot] :as game-state} [x y]]
  (for [[tx ty] [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]
        :when (and (not= [x y] [tx ty])
                   (movable? game-state [tx ty]))]
    [tx ty]))

(def a*-priority-function
  (fn [x y]
    (if (= x y)
      0
      (let [[f1 _ h1] x
            [f2 _ h2] y]
        (if (= f1 f2)
          (if (< h1 h2) -1 1)
          (if (< f1 f2) -1 1))))))

(defn reconstruct-path
  [came-from current]
  (if-let [previous (came-from current)]
    (conj (reconstruct-path came-from previous) current)
    [current]))

(defn a*
  [{:keys [board] :as game-state} start end]
  (loop [closed #{}
        [current [f1 g1 h1]] [start [(manhattan-distance start end) 0 (manhattan-distance start end)]]
        g {start g1}
        f {start f1}
        open (priority-map-by a*-priority-function)
        came-from {}]
    (if current
      (if (= current end)
        (reconstruct-path came-from end)
        (let [new-closed (conj closed current)
              neighbors (remove new-closed (edges game-state current))
              ;;_ (prn current)
              ;;_ (prn neighbors)
              ;;_ (prn "neighbors" neighbors)
              improvements (filter #(or (not (open %))
                                        (< (inc g1) (g %)))
                                   neighbors)
              ;;_ (prn "improvements" (map vector improvements (map (constantly (inc g1)) improvements) (map g improvements)))
              came-from (reduce #(conj %1 [%2 current]) came-from improvements)
              g (reduce conj g (for [neighbor improvements]
                                 [neighbor (+ g1 (manhattan-distance current neighbor))]))
              f (reduce conj f (for [neighbor improvements]
                                 [neighbor (+ (g neighbor) (manhattan-distance neighbor end))]))
              new-open (reduce conj open (for [neighbor improvements]
                                           [neighbor [(f neighbor) (g neighbor) (manhattan-distance neighbor end)]]))]
          (recur new-closed (first new-open) g f (if (not (empty? new-open)) (pop new-open)) came-from))))))

(defn closest
  [pos candidates]
  {:pre  [candidates]
   :post [(some #{%} candidates)]}
  (let [dists    (into {} (for [c candidates]
                                   [(distance pos c) c]))
        min-dist (apply min (keys dists))]
    (dists min-dist)))

(defn directions-from
  [[x1 y1] [x2 y2]]
  (let [dirs {[:= :=] [:W]
              [:= :>] [:U]
              [:= :<] [:D]
              [:> :=] [:R]
              [:> :>] [:U :R]
              [:> :<] [:D :R]
              [:< :=] [:L]
              [:< :>] [:U :L]
              [:< :<] [:D :L]}
        cmp  (fn [a b] (cond
                         (= a b) :=
                         (> a b) :>
                         :else :<))]
    (dirs [(cmp x2 x1) (cmp y2 y1)])))

(defn stupid-1
  [{:keys [robot lambdas lift] :as game-state}]
  (let [dest           (if (empty? lambdas)
                         lift
                         (closest robot lambdas))
        ordered-moves  (concat (directions-from robot dest) [:U :D :L :R])
        possible-moves (filter #(move-allowed? game-state %) ordered-moves)
        dir            (or (first possible-moves) :A)
        ]
    (prn "Destination: " dest)
    (prn "Moving: " dir)
    dir))

(defn run-sequence
  [game-state moves]
  (if-let [move (first moves)]
    (let [new-state (step game-state move)]
      (if (game-over? new-state)
        new-state
        (recur new-state (rest moves))))
    game-state))

(defn route-to-lift
  [{:keys [robot lift] :as game-state}]
  (a* game-state robot lift))

(defn a*-lambdas
  [{:keys [robot lift] :as game-state} lambdas]
  (let [plan   (mapcat #(a* game-state %1 %2) (cons robot lambdas) lambdas)
        moves  (mapcat directions-from plan (rest plan))]
        (run-sequence game-state (remove #(= :W %) moves))))

(defn a*-ai
  [{:keys [robot lambdas lift] :as game-state}]
  (let [intermediates (pmap #(a*-lambdas game-state %) (permutations lambdas))
        finals (map (fn [game-state]
                      (if (game-over? game-state)
                        game-state
                        (let [plan (route-to-lift game-state)
                              moves (mapcat directions-from plan (rest plan))]
                          (run-sequence game-state (remove #(= :W %) moves))))) intermediates)]
    (last (into (sorted-map)
                (for [result finals]
                  [(compute-score result) result])))))

(defn run-ai-vector
  [moves game-ref]
  (doseq [move moves]
    (if-not (game-over? @game-ref)
      (dosync
        (prn "Trying to move" move)
        (alter game-ref step move)))))

(defn run-ai
  [f game-ref]
  (loop []
    (if-not (game-over? @game-ref)
      (do
        (dosync
         (alter game-ref step (f @game-ref)))
        (recur)))))
