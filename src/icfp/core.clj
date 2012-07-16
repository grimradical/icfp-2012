(ns icfp.core)

;; lambdas - location of lambdas
;; rocks - location of rocks
;; robot - robot position
;; lift - location of lift
;; moves - history of previous moves
;; water - current water level
;; flooding - current flooding rate
;; waterproof - how many turns the robot can stay underwater until it dies
(defstruct game-state :board :status :lambdas :rocks :lift :robot :score :moves
    :water :flooding :waterproof :g :growth :razors :beards)

(defn replace-in-set
  [s old new]
  (set (replace {old new} s)))

(defn robot?
  [{:keys [board]} position]
  (= (get-in board position) :R))

(defn rock?
  [{:keys [board]} position]
  (= (get-in board position) :*))

(defn space?
  [{:keys [board]} position]
  (= (get-in board position) :_))

(defn wall?
  [{:keys [board]} position]
  (= (get-in board position) :#))

(defn earth?
  [{:keys [board]} position]
  (= (get-in board position) :.))

(defn lambda?
  [{:keys [board]} position]
  (= (get-in board position) :>))

(defn beard?
  [{:keys [board]} position]
  (= (get-in board position) :W))

(defn shaveable-beard?
  [{:keys [board razors] :as game-state} position]
  (and (beard? game-state position)
       (pos? razors)))

(defn razor?
  [{:keys [board]} position]
  (= (get-in board position) :!))

(defn open-lift?
  [{:keys [board] :as game-state} position]
  (= (get-in board position) :O))

(defn closed-lift?
  [{:keys [board] :as game-state} position]
  (= (get-in board position) :L))

(defn lift?
  [{:keys [board] :as game-state} position]
  (or (open-lift? game-state position)
      (closed-lift? game-state position)))

(defn assert-game-state
  [{:keys [board lambdas rocks lift robot beards razors g growth] :as game-state}]
  (assert (map? game-state))
  (assert (map? board))
  (assert (every? (partial lambda? game-state) lambdas))
  (assert (every? (partial rock? game-state) rocks))
  (assert (every? (partial beard? game-state) beards))
  (assert (>= razors 0))
  (assert (and (< g growth) (>= g 0)))
  (assert (or (lift? game-state lift)
              (and (robot? game-state lift)
                   (empty? lambdas))))
  (assert (robot? game-state robot))
  true)

(defn width
  [board]
  (count board))

(defn height
  [board]
  (count (board 1)))

(defn left
  [position]
  [(dec (first position)) (last position)])

(def lleft
  (comp left left))

(defn right
  [position]
  [(inc (first position)) (last position)])

(def rright
  (comp right right))

(defn up
  [position]
  [(first position) (inc (last position))])

(defn down
  [position]
  [(first position) (dec (last position))])

(defn adjacent-coords
  [pos]
  [(up pos)
   (down pos)
   (left pos)
   (right pos)])

(defn surrounding-coords
  [pos]
  (concat (adjacent-coords pos) [(left (up pos))
                                 (right (up pos))
                                 (left (down pos))
                                 (right (down pos))]))

(defn movable?
  [game-state position]
  (or (space? game-state position)
      (earth? game-state position)
      (open-lift? game-state position)
      (lambda? game-state position)
      (razor? game-state position)
      (shaveable-beard? game-state position)))

(defn move-allowed?
  [{:keys [board robot] :as game-state} move]
  (condp = move
    :U (movable? game-state (up robot))
    :D (movable? game-state (down robot))
    :L (or (movable? game-state (left robot))
           (and (rock? game-state (left robot))
                 (space? game-state (lleft robot))))
    :R (or (movable? game-state (right robot))
           (and (rock? game-state (right robot))
                 (space? game-state (rright robot))))))

(defn command-allowed?
  [{:keys [board robot razors] :as game-state} command]
  (condp = command
    :L (move-allowed? game-state :L)
    :R (move-allowed? game-state :R)
    :U (move-allowed? game-state :U)
    :D (move-allowed? game-state :D)
    :S (pos? razors)
    :W true
    :A true))

(defn move-to-position
  [{:keys [board] :as game-state} obj old-pos new-pos]
  {:pre [(not (nil? obj))
         (= (get-in board old-pos) obj)
         (not= old-pos new-pos)]
   :post [(= (get-in (:board %) new-pos) obj)
          (space? % old-pos)
          (not= board (:board %))]}
  (let [new-board (-> board
                    (assoc-in new-pos obj)
                    (assoc-in old-pos :_))]
    (assoc game-state :board new-board)))

(defn maybe-crush-robot
  [{:keys [status robot] :as game-state} rock]
  {:pre [(assert-game-state game-state)]
   :post [(assert-game-state %)
          (not (and (#{:aborted :victory} status)
                    (= :dead (:status %))))]}
  (if (and (= rock (up robot))
           (= status :alive))
    (assoc game-state :status :dead)
    game-state))

(defn move-rock
  [{:keys [rocks] :as game-state} old-rock new-rock]
  {:pre [(assert-game-state game-state)
         (rock? game-state old-rock)]
   :post [(assert-game-state %)
          (space? % old-rock)
          (rock? % new-rock)]}
  (-> game-state
    (update-in [:rocks] replace-in-set old-rock new-rock)
    (move-to-position :* old-rock new-rock)
    (maybe-crush-robot new-rock)))

(defn push-rock
  [{:keys [robot] :as game-state} direction]
  {:pre [(assert-game-state game-state)]
   :post [(assert-game-state %)]}
  (if (and (#{left right} direction)
           (rock? game-state (direction robot)))
    (let [old-rock (direction robot)
          new-rock (direction old-rock)]
      (move-rock game-state old-rock new-rock))
    game-state))

(defn shave-beard
  [{:keys [razors] :as game-state} position]
  {:pre [(assert-game-state game-state)
         (shaveable-beard? game-state position)]
   :post [(assert-game-state %)
          (space? % position)]}
  (-> game-state
    (update-in [:board] assoc-in position :_)
    (update-in [:beards] disj position)
    (update-in [:razors] dec)))

(defn shave-beards
  [{:keys [robot razors] :as game-state}]
  {:pre [(assert-game-state game-state)
         (pos? razors)]
   :post [(assert-game-state %)
          (= (:razors %) (dec razors))
          (every? (partial space? %)
                  (filter (partial shaveable-beard? game-state)
                          (surrounding-coords robot)))]}
  (reduce shave-beard game-state
          (filter (partial shaveable-beard? game-state)
                  (surrounding-coords robot))))

(defn move-robot
  [{:keys [robot] :as game-state} direction]
  {:pre [(assert-game-state game-state)]
   :post [(assert-game-state %)
          (not= (:robot %) robot)
          (= (:robot %) (direction robot))]}
  (let [new-robot (direction robot)]
    (-> game-state
      (assoc :robot new-robot)
      (move-to-position :R robot new-robot))))

(defn open-lift
  [{:keys [lambdas lift] :as game-state}]
  {:pre [(assert-game-state game-state)
         (closed-lift? game-state lift)]
   :post [(assert-game-state %)
          (if (empty? lambdas)
            (open-lift? % lift)
            (closed-lift? % lift))
          (= lift (:lift %))]}
  (if (empty? lambdas)
    (update-in game-state [:board] assoc-in lift :O)
    game-state))

(defn collect-lambda*
  [{:keys [lambdas score] :as game-state} position]
  {:pre [(lambda? game-state position)
         (lambdas position)]
   :post [(= (count (:lambdas %)) (dec (count lambdas)))
          (space? % position)
          (if (empty? (:lambdas %))
            (open-lift? % (:lift %))
            (closed-lift? % (:lift %)))
          (= (:score %) (+ score 25))]}
  (-> game-state
    (update-in [:board] assoc-in position :_)
    (update-in [:lambdas] disj position)
    (update-in [:score] + 25)
    (open-lift)))

(defn collect-lambda
  [game-state position]
  {:pre [(assert-game-state game-state)]
   :post [(assert-game-state %)]}
  (if (lambda? game-state position)
    (collect-lambda* game-state position)
    game-state))

(defn collect-razor*
  [{:keys [razors score] :as game-state} position]
  {:pre [(razor? game-state position)]
   :post [(= (:razors %) (inc razors))
          (space? % position)]}
  (-> game-state
    (update-in [:board] assoc-in position :_)
    (update-in [:razors] inc)))

(defn collect-razor
  [game-state position]
  {:pre [(assert-game-state game-state)]
   :post [(assert-game-state %)]}
  (if (razor? game-state position)
    (collect-razor* game-state position)
    game-state))

(defn enter-lift
  [{:keys [status lambdas lift robot] :as game-state}]
  {:pre [(not= (empty? lambdas)
               (closed-lift? game-state lift))
         (assert-game-state game-state)]
   :post [(assert-game-state %)
          (#{:alive :victory} (:status %))]}
  (if (= lift robot)
    (assoc game-state :status :victory)
    game-state))

(defn move
  [{:keys [board robot] :as game-state} command]
  {:pre [(assert-game-state game-state)]
   :post [(assert-game-state %)]}
  (let [direction (command {:L left :R right :U up :D down})]
    (-> game-state
      (push-rock direction)
      (collect-lambda (direction robot))
      (collect-razor (direction robot))
      (move-robot direction)
      (enter-lift))))

(defn execute-command
  [{:keys [moves] :as game-state} command]
  {:pre [(assert-game-state game-state)]
   :post [(assert-game-state %)]}
  (if (command-allowed? game-state command)
    (let [new-state (cond
                      (#{:L :R :U :D} command)
                      (move game-state command)

                      (= :W command)
                      game-state

                      (= :S command)
                      (shave-beards game-state)

                      (= :A command)
                      (assoc game-state :status :aborted))]
      (update-in new-state [:moves] conj command))
    (execute-command game-state :W)))

(defn fall-down
  [{:keys [board] :as game-state} position]
  (let [lower-pos (down position)]
    (if (space? game-state lower-pos)
      lower-pos)))

(defn fall-right
  [{:keys [board] :as game-state} position]
  (let [lower-pos (down position)
        right-pos (right position)
        lower-right-pos (down (right position))]
    (if (and (or (rock? game-state lower-pos)
                 (lambda? game-state lower-pos))
             (space? game-state right-pos)
             (space? game-state lower-right-pos))
      lower-right-pos)))

(defn fall-left
  [{:keys [board] :as game-state} position]
  (let [lower-pos (down position)
        left-pos (left position)
        lower-left-pos (down (left position))]
    (if (and (rock? game-state lower-pos)
             (space? game-state left-pos)
             (space? game-state lower-left-pos))
      lower-left-pos)))

(defn fall-rock
  [game-state position]
  {:pre [(rock? game-state position)]}
  (let [possible-falls (juxt fall-down fall-right fall-left)]
    (first (remove nil? (possible-falls game-state position)))))

(defn add-beard
  [game-state beard]
  (-> game-state
    (update-in [:beards] conj beard)
    (update-in [:board] assoc-in beard :W)))

(defn grow-beard
  [{:keys [board] :as game-state} beard]
  (let [new-beards (filter #(space? game-state %) (surrounding-coords beard))]
    (reduce add-beard game-state new-beards)))

(defn grow-beards
  [{:keys [beards g growth board] :as game-state}]
  (if (zero? g)
    (let [new-state  (reduce grow-beard game-state beards)]
      (assoc new-state :g (dec growth)))
    (update-in game-state [:g] dec)))

(defn update-board
  [{:keys [rocks robot] :as game-state}]
  {:pre [(assert-game-state game-state)]
   :post [(assert-game-state %)]}
  (let [rock-movements (for [rock rocks
                             :let [new-rock (fall-rock game-state rock)]
                             :when new-rock]
                         [rock new-rock])]
    (-> (reduce #(apply move-rock %1 %2) game-state rock-movements)
      (grow-beards))))

(defn step
  [{:keys [status] :as game-state} command]
  {:pre [(assert-game-state game-state)
         (= status :alive)]
   :post [(assert-game-state %)]}
  (-> game-state
    (execute-command command)
    (update-board)))

(defn compute-score
  ([{:keys [status] :as game-state}]
   (compute-score game-state status))
  ([{:keys [score moves] :as game-state} status]
   (condp = status
     :alive
     nil

     :victory
     ;; 50 additional points per lambda (triple).
     (- (* score 3) (count moves))

     :aborted
     ;; 25 additional points per lambda (double), and don't count the abort
     ;; move against us.
     (- (* score 2) (dec (count moves)))

     :dead
     (- score (count moves)))))

(defn win?
  [{:keys [status] :as game-state}]
  {:pre [(assert-game-state game-state)]}
  (= status :victory))

(defn lose?
  [{:keys [status] :as game-state}]
  {:pre [(assert-game-state game-state)]}
  (= status :dead))

(defn aborted?
  [{:keys [status] :as game-state}]
  {:pre [(assert-game-state game-state)]}
  (= status :aborted))

(defn game-over?
  [{:keys [status] :as game-state}]
  {:pre [(assert-game-state game-state)]}
  (not= status :alive))

(defn wait-n-turns
  [game-state n]
  (if (zero? n)
    game-state
    (let [next-state (if (game-over? game-state)
                        game-state
                        (step game-state :W))]
      (recur next-state (dec n)))))

(defn death-sentence?
  [game-state n]
  (= :dead (:status (wait-n-turns n))))

(defn opposite?
  [d1 d2]
  (let [opposites #{[:L :R] [:R :L] [:U :D] [:D :U]}]
    (opposites [d1 d2])))

(defn rock-movable?
  [{:keys [board] :as game-state} [x y :as pos]]
  {:pre [(rock? game-state pos)]}
  ;; A rock is movable if there an empty space next to it, of if just
  ;; waiting a turn causes it to change position on its own (sliding)
  (or (and (= :_ (get-in board [(dec x) y]))
          (= :_ (get-in board [(inc x) y])))
      (let [future (step game-state :W)]
        (not= :* (get-in (:board future) pos)))))

(defn immovable?
  [{:keys [board] :as game-state} [x y :as pos]]
  (condp = (get-in board pos)
    :L true
    :O true
    :# true
    :> true
    :* (not (rock-movable? game-state pos))
    false))

(defn rock-immovable?
  [game-state pos]
  (not (rock-movable? game-state pos)))

(defn position-blocked?
  [{:keys [board] :as game-state} pos]
  (let [liberty? #(or (#{:_ :. :> :R} (get-in board %))
                      (shaveable-beard? game-state pos)
                      (and (rock? game-state %)
                           (rock-movable? game-state %)))
        candidates (adjacent-coords pos)]
    (empty? (filter liberty? candidates))))

(defn lift-blocked?
  [{:keys [lift] :as game-state}]
  (position-blocked? game-state lift))

(defn any-lambda-blocked?
  [{:keys [lambdas] :as game-state}]
  (some identity (map #(position-blocked? game-state %) lambdas)))
