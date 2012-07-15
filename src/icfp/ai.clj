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
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))))

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

(defn cost
  [current start end]
  (let [g (manhattan-distance start current)
        h (manhattan-distance current end)
        f (+ g h)]
    [f g h]))

(defn edges
  [game-state]
  (when-not (game-over? game-state)
    (for [command #{:L :R :U :D :A}
          :when (command-allowed? game-state command)]
      (step game-state command))))

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
  [edge-fn cost-fn goal? start goal]
  (loop [closed #{}
        [current [f1 g1 h1]] [start [(cost-fn start goal) 0 (cost-fn start goal)]]
        g {start g1}
        f {start f1}
        open (priority-map-by a*-priority-function)]
    (when current
      (if (goal? current goal)
        current
        (let [new-closed (conj closed current)
              neighbors (edge-fn current)
              improvements (remove #(or (new-closed %)
                                        (and (open %)
                                             (< (g %) (+ (g current) (cost-fn current %)))))
                                   neighbors)
              g (reduce conj g (for [neighbor improvements]
                                 [neighbor (+ g1 (cost-fn current neighbor))]))
              f (reduce conj f (for [neighbor improvements]
                                 [neighbor (+ (g neighbor) (cost-fn neighbor goal))]))
              new-open (reduce conj open (for [neighbor improvements]
                                           [neighbor [(f neighbor) (g neighbor) (cost-fn neighbor goal)]]))]
          (recur new-closed (first new-open) g f (if (not (empty? new-open)) (pop new-open))))))))

(defn closest
  [pos candidates]
  {:pre  [candidates]
   :post [(some #{%} candidates)]}
  (let [dists    (into {} (for [c candidates]
                                   [(manhattan-distance pos c) c]))
        min-dist (apply min (keys dists))]
    (dists min-dist)))


(defn valid-dest?
  [{:keys [lambdas lift] :as game-state} pos]
  (or (some #{pos} lambdas)
      (= lift pos)))

(defn stupid-1
  [{:keys [robot lambdas lift moves visited prev-dest] :as game-state :or {visited []}}]
  (let [dest            (if (valid-dest? game-state prev-dest)
                          prev-dest
                          (if (empty? lambdas)
                            lift
                            (closest robot lambdas)))
        seen            (if (= dest prev-dest)
                          (conj visited robot)
                          [robot])
        ordered-moves   (concat (directions-from robot dest) [:U :D :L :R])
        move-preferred? (fn [m]
                          (let [future (step game-state m)]
                            (and (not (some #{(:robot future)} seen))
                                 (not (lose? future))
                                 (move-allowed? game-state m))))
        move-ok?        (fn [m]
                          (let [future (step game-state m)]
                            (and (not (lose? future))
                                 (move-allowed? game-state m))))
        preferred-moves (filter move-preferred? ordered-moves)
        possible-moves  (filter move-ok? ordered-moves)
        dir             (or (first preferred-moves)
                            (first possible-moves)
                            :A)
        seen            (if (empty? preferred-moves)
                          [robot]
                          seen)
        ]
        (prn)
        (prn "Move " (count moves))
        (prn "Robot: " robot)
        (prn "Seen: " seen)
        (prn "Destination: " dest)
        (prn "Distance: " (manhattan-distance robot dest))
        (prn "Moves in order: " ordered-moves)
        (prn "Preferred moves: " preferred-moves)
        (prn "Possible moves: " possible-moves)
        (prn "Moving: " dir)
        (prn game-state)
        (-> game-state
            (step dir)
            (assoc :visited seen)
            (assoc :prev-dest dest))))

(defn run-sequence
  [game-state moves]
  (if-let [move (first moves)]
    (let [new-state (step game-state move)]
      (if (game-over? new-state)
        new-state
        (recur new-state (rest moves))))
    game-state))

(comment (defn a*-lambdas
  [{:keys [robot lift] :as game-state} lambdas]
  (let [edge-fn edges
        cost-fn #(manhattan-distance (:robot %1) (:robot %2))
        goal? #(= (:robot %1) (:robot %2))]
    (loop [path []
           start game-state
           [goal & next-goals] lambdas]
      (if goal
        (let [result (a* edge-fn cost-fn goal? start end)]
          (recur (concat path (:moves result)) result next-goals))
        path)))))

(defn a*-lambdas
  [game-state lambdas]
  (let [edge-fn edges
        cost-fn #(manhattan-distance (:robot %1) (:robot %2))
        goal? #(= (:robot %1) (:robot %2))]
    (reduce #(a* edge-fn cost-fn goal? %1 {:robot %2}) game-state lambdas)))

(defn a*-ai
  [{:keys [robot lambdas lift] :as game-state}]
  (let [results (take 1 (map #(a*-lambdas game-state %) (map #(concat % [lift]) (permutations lambdas))))]
    (last (into (sorted-map)
                (for [result results]
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
  (loop [n 0]
    ;;(Thread/sleep 800)
    (if-not (game-over? @game-ref)
      (do
        (dosync
         (ref-set game-ref (f @game-ref)))
        (if (< n 200)
          (recur (inc n))))
      @game-ref)))
