(ns icfp.ai
  (:use icfp.core))

(defn distance
  [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                (Math/pow (- y2 y1) 2))))

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

(defn run-ai
  [f game-ref]
  (loop []
    (if-not (game-over? @game-ref)
      (do
        (dosync
         (alter game-ref step (f @game-ref)))
        (recur)))))