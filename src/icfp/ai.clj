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

(defn hamiltonian
  ([vertices]
   {:pre [(sequential? vertices)
          (every? vector? vertices)]}
   (let [edges (into (priority-map)
                     (for [a vertices
                           b vertices
                           :when (not= a b)]
                       [[a b] (manhattan-distance a b)]))]
     (hamiltonian vertices edges)))
  ([vertices edges]
   {:post [(= (count vertices) (count %))]}
   (loop [path []
          current (first vertices)
          remaining-edges edges]
     ;; Find the closest remaining vertex to us. Drop all the edges that aren't
     ;; from us and then take the first that's left, which should be the
     ;; shortest.
     (if current
       (let [[[_ next] cost] (first (drop-while #(not= current (ffirst %)) remaining-edges))]
         ;; Remove all the edges that are to or from the current vertex now.
         (recur (conj path current) next (remove #(or (= (first (key %)) current) (= (second (key %)) current)) remaining-edges)))
       path))))

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
  {:pre  [candidates]}
  (let [dists        (for [c candidates]
                       [(manhattan-distance pos c) c])
        dists        (sort dists)
        [min-dist _] (first dists)]
    (map second
         (filter #(= (first %) min-dist) dists))))

(defn stupid-2-cost
  [p {:keys [robot score rocks board moves] :as g} dest]
  (let [dist       (manhattan-distance robot dest)
        dead       (if (lose? g) 5000 0)
        win        (if (win? g) -5000 0)
        s          (- 0 score)
        backtrack  (let [[m1 m2] (take 2 (reverse moves))]
                     (if (= m1 (invert-move m2)) 10 0))
        below-rock (let [[rx ry] robot
                         above (get-in board [rx (inc ry)])]
                     (if (= :* above)
                       1000
                       0))
        rocks-moved (if (= (:rocks p) rocks)
                      0
                      1000)
        ]
    (+ dist dead win s backtrack below-rock)))

(defn stupid-2
  [{:keys [robot lambdas lift] :as g}]
  (let [dest  (first (closest robot lambdas))
        costs (for [m [:U :D :L :R]
                    :when (move-allowed? g m)]
                [(stupid-2-cost g (step g m) dest) m])
        costs (sort costs)
        dir   (second (first costs))
        ]
    (prn)
    (prn costs)
    (prn dir)
    (prn robot)
    (-> g
        (step dir))))

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

(defn a*-targets
  [game-state targets]
  (let [edge-fn edges
        cost-fn #(manhattan-distance (:robot %1) (:robot %2))
        goal? #(= (:robot %1) (:robot %2))]
    (reduce #(a* edge-fn cost-fn goal? %1 {:robot %2}) game-state targets)))

(defn a*-ai
  [{:keys [robot lambdas lift] :as game-state}]
  (let [edge-fn edges
        cost-fn #(manhattan-distance (:robot %1) (:robot %2))
        goal? #(= (:robot %1) (:robot %2))]
  (loop [current game-state
         remaining lambdas]
    (if (seq remaining)
      (let [results (into (sorted-map)
                          (for [state (map #(a* edge-fn cost-fn goal? current {:robot %}) remaining)]
                            [(compute-score state) state]))
            state (val (first results))]
        (recur state (:lambdas state)))
      (if (open-lift? current (:lift current))
        (let [state (a* edge-fn cost-fn goal? current {:robot (:lift current)})]
          [(compute-score state) state])
        [(compute-score current) current])))))

(comment (defn a*-ai
  [{:keys [robot lambdas lift] :as game-state}]
  (let [strategy (hamiltonian (cons robot lambdas))
        ;; Take the rest because the first one is the robot, and add the lift
        targets (concat (rest strategy) [lift])
        result (a*-targets game-state targets)]
    [(compute-score result) result])))

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
        (if (< n 1000)
          (recur (inc n))
          (prn "Terminating early")))
      @game-ref)))
