(ns icfp.core)

(defstruct game-state :board :lambdas :rocks :robot :score :moves
    :water :flooding :waterproof)

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
  [{:keys [board lambdas] :as game-state} position]
  (and (empty? lambdas)
       (lift? board position)))

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

(def movable?
  (some-fn space? earth? open-lift? lambda?))

(defn move-allowed?
  [{:keys [board robot-position] :as game-state} move]
  (condp = move
    :U (movable? game-state (up robot-position))
    :D (movable? game-state (down robot-position))
    :L (or (movable? game-state (left robot-position))
           (and (rock? game-state (left robot-position))
                 (space? game-state (lleft robot-position))))
    :R (or (movable? game-state (right robot-position))
           (and (rock? game-state (right robot-position))
                 (space? game-state (rright robot-position))))))

(defn command-allowed?
  [{:keys [board robot-position] :as game-state} command]
  (condp = command
    :L (move-allowed? board :L)
    :R (move-allowed? board :R)
    :U (move-allowed? board :U)
    :D (move-allowed? board :D)
    :W true
    :A true))

(defn move-sideways
  [{:keys [board robot] :as game-state} direction]
  ;; We already know the move is legal, so it's either a rock or not. If it's a
  ;; rock, move the robot, remove the old rock, add the new rock. Otherwise
  ;; just move the robot.
  (if (rock? game-state (direction robot))
    (-> game-state
      (update-in [:robot] direction)
      (update-in [:rocks] disj (direction robot))
      (update-in [:rocks] conj (direction (direction robot))))
    (update-in [:robot] direction)))

(defn move
  [{:keys [board robot] :as game-state} command]
  (condp = command
    :L (move-sideways game-state left)
    :R (move-sideways game-state right)
    :U (update-in game-state [:robot] up)
    :D (update-in game-state [:robot] down)))

(defn execute-command
  [{:keys [moves] :as game-state} command]
  (if (command-allowed? game-state command)
    (cond
      ([:L :R :U :D] command)
      (move game-state command)

      ([:W :A] command)
      nil)
    (update-in game-state [:moves] conj command)
    (execute-command game-state :W)))

(defn fall-down
  [board position]
  (let [lower-pos (down position)]
    (if ((some-fn space? robot?) lower-pos)
      lower-pos)))

(defn fall-right
  [board position]
  (let [lower-pos (down position)
        right-pos (right position)
        lower-right-pos (down (right position))]
    (if (and ((some-fn rock? lambda?) lower-pos)
             (space? right-pos)
             (space? lower-right-pos)))))

(defn fall-left
  [board position]
  (let [lower-pos (down position)
        left-pos (left position)
        lower-left-pos (down (left position))]
    (if (and (rock? lower-pos)
             (space? left-pos)
             (space? lower-left-pos)))))

(def fall-rock
  (some-fn fall-down fall-right fall-left last))

(defn update-board
  [{:keys [board rocks] :as game-state}]
  (let [new-rocks (set (map (partial fall-rock board) rocks))]
    (assoc game-state :rocks new-rocks)))
