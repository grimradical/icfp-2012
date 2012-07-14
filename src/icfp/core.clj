(ns icfp.core)

(defstruct game-state :board :lambdas :rocks :robot :score :moves
    :water :flooding :waterproof)

(defn replace-in-set
  [s old new]
  (set (replace {old new} s)))

(defn assert-game-state
  [{:keys [board lambdas rocks robot score moves water flooding waterproof] :as game-state}]
  (assert board)
  (assert (every? (partial lambda? game-state) lambdas))
  (assert (every? (partial rock? game-state) rocks))
  (assert (robot? game-state robot))
  true)

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

(defn lift?
  [{:keys [board]} position]
  (= (get-in board position) :L))

(defn open-lift?
  [{:keys [board] :as game-state} position]
  (= (get-in board position) :O))

(defn closed-lift?
  [{:keys [board] :as game-state} position]
  (= (get-in board position) :L))

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

(defn movable?
  [game-state position]
  (or (space? game-state position)
      (earth? game-state position)
      (open-lift? game-state position)
      (lambda? game-state position)))

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
  [{:keys [board robot] :as game-state} command]
  (condp = command
    :L (move-allowed? game-state :L)
    :R (move-allowed? game-state :R)
    :U (move-allowed? game-state :U)
    :D (move-allowed? game-state :D)
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

(defn push-rock
  [{:keys [robot] :as game-state} direction]
  {:pre [(assert-game-state game-state)]
   :post [(assert-game-state %)]}
  (if (and (#{left right} direction)
           (rock? game-state (direction robot)))
    (let [old-rock (direction robot)
          new-rock (direction old-rock)]
      (-> game-state
        (update-in [:rocks] replace-in-set old-rock new-rock)
        (move-to-position :* old-rock new-rock)))
    game-state))

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
  {:pre [(lambda? game-state position)]
   :post [(= (count (:lambas %)) (dec (count lambdas)))
          (space? % position)
          (if (empty (:lambdas %))
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

(defn move
  [{:keys [board robot] :as game-state} command]
  {:pre [(assert-game-state game-state)]
   :post [(assert-game-state %)]}
  (let [direction (command {:L left :R right :U up :D down})]
    (-> game-state
      (push-rock direction)
      (collect-lambda (direction robot))
      (move-robot direction))))

(defn execute-command
  [{:keys [moves] :as game-state} command]
  {:pre [(assert-game-state game-state)]
   :post [(assert-game-state %)]}
  (if (command-allowed? game-state command)
    (let [new-state (cond
                      (#{:L :R :U :D} command)
                      (move game-state command)

                      (#{:W :A} command)
                      game-state)]
      (update-in new-state [:moves] conj command))
    (execute-command game-state :W)))

(defn fall-down
  [{:keys [board] :as game-state} position]
  (let [lower-pos (down position)]
    (if (apply (some-fn space? robot?) [game-state lower-pos])
      lower-pos)))

(defn fall-right
  [{:keys [board] :as game-state} position]
  (let [lower-pos (down position)
        right-pos (right position)
        lower-right-pos (down (right position))]
    (if (and (apply (some-fn rock? lambda?) [game-state lower-pos])
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

(def fall-rock
  (some-fn fall-down fall-right fall-left last))

(defn update-board
  [{:keys [board rocks] :as game-state}]
  {:pre [(assert-game-state game-state)]
   :post [(assert-game-state %)]}
  (let [new-rocks (set (map (partial fall-rock game-state) rocks))]
    (-> game-state
      ;; Insert spaces where the old rocks were
      (assoc :board (reduce #(assoc-in board % :_) board rocks))
      ;; And rocks where the new rocks are
      (assoc :board (reduce #(assoc-in board % :*) board new-rocks))
      (assoc game-state :rocks new-rocks))))
