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
  [{:keys [robot] :as game-state} goal]
  (when-not (game-over? game-state)
    ;; Don't even bother if we can't get to the goal
    (let [giveup-if-blocked (fn [game-state goal commands]
                              (if (position-blocked? game-state (:robot goal))
                                (apply disj commands #{:L :R :U :D})
                                commands))
          flee-from-doom (fn [game-state goal commands]
                           (if (rock? game-state (up robot))
                             (disj commands :D)
                             commands))
          remove-illegal-commands (fn [game-state goal commands]
                                    (filter #(command-allowed? game-state %) commands))
          command-rules [giveup-if-blocked flee-from-doom remove-illegal-commands]
          commands (reduce #(%2 game-state goal %1) #{:L :R :U :D :A} command-rules)]
      (for [command commands]
        (step game-state command)))))

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
  (println (format "Navigating from %s to %s" (:robot start) (:robot goal)))
  (loop [closed #{}
        [current [f1 g1 h1]] [start [(cost-fn start goal) 0 (cost-fn start goal)]]
        g {start g1}
        f {start f1}
        open (priority-map-by a*-priority-function)]
    (if current
      ;;(prn (:robot current))
      (if (goal? current goal)
        current
        (let [new-closed (conj closed (:board current))
              neighbors (edge-fn current goal)
              improvements (remove #(or (new-closed (:board %))
                                        (and (open %)
                                             (< (g %) (+ (g current) (cost-fn current %)))))
                                   neighbors)
              g (reduce conj g (for [neighbor improvements
                                     :let [backtracking-penalty (if (opposite? (last (:moves current)) (last (:moves neighbor))) 0 0)]]
                                 [neighbor (+ g1 (cost-fn current neighbor) backtracking-penalty)]))
              f (reduce conj f (for [neighbor improvements]
                                 [neighbor (+ (g neighbor) (cost-fn neighbor goal))]))
              new-open (reduce conj open (for [neighbor improvements]
                                           [neighbor [(f neighbor) (g neighbor) (cost-fn neighbor goal)]]))]
          (recur new-closed (first new-open) g f (if (not (empty? new-open)) (pop new-open)))))
      (throw (RuntimeException. (format "Failed to find a route from %s to %s" (:robot start) (:robot goal)))))))

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

(defn a*-targets
  [game-state targets]
  (let [edge-fn edges
        cost-fn (fn [a b]
                  (cond
                    ;; We want to try not to block the goal. We'd rather do
                    ;; that than die or abort, though.
                    (position-blocked? (wait-n-turns a 10) (:robot b))
                    10000

                    ;; It's even worse to block the lift than our own target!
                    (position-blocked? (wait-n-turns a 10) (:lift a))
                    100000

                    ;; If we can't win, aborting is a good alternative
                    (aborted? a)
                    1000000

                    ;; If we can't win OR abort (we're already dead), then we
                    ;; need to count this as done as well
                    (lose? a)
                    10000000

                    :else
                    (manhattan-distance (:robot a) (:robot b))))
        goal? (fn [curr goal]
                (or (= (:robot curr) (:robot goal))
                    (aborted? curr)))]
    (reduce (fn [start goal]
              (let [result (a* edge-fn cost-fn goal? start {:robot goal})]
                (println (str "Path so far: " (clojure.string/join (map name (:moves result)))))
                result)) game-state targets)))

(comment (defn a*-ai
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
        [(compute-score current) current]))))))

(defn a*-ai
  [{:keys [robot lambdas lift] :as game-state}]
  (let [strategy (hamiltonian (cons robot lambdas))
        ;; Take the rest because the first one is the robot, and add the lift
        targets (concat (rest strategy) [lift])
        result (a*-targets game-state targets)]
    [(compute-score result) result]))

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
