(ns icfp.ai
  (:use icfp.core))

(defn distance
  [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                (Math/pow (- y2 y1) 2))))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))))

(defn closest
  [pos candidates]
  {:pre  [candidates]
   :post [(some #{%} candidates)]}
  (let [dists    (into {} (for [c candidates]
                                   [(manhattan-distance pos c) c]))
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
