(ns icfp.core)

(defstruct game-state :board :lambdas :rocks :robot :score :moves)

(defn rock?
  [{:keys [board]} position]
  (= (get-in board position) :R))

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

(defn movable?
  [game-state position]
  ((some-fn space? earth? open-lift? lambda?) game-state position))

(comment (defn move-allowed?
  [{:keys [board robot-position] :as game-state}]
  (condp = move
    :U (movable? game-state (up robot-position))
    :D (movable? game-state (down robot-position))
    :L (or (movable? game-state (left robot-position))
            (and (rock? game-state (left robot-position))
                 (space? game-state (lleft robot-position))))
    :R (or (movable? game-state (right robot-position))
            (and (rock? game-state (right robot-position))
                 (space? game-state (rright robot-position)))))))

(defn command-allowed?
  [{:keys [board robot-position] :as game-state} command]
  (condp = command
    :L (move-allowed? board :L)
    :R (move-allowed? board :R)
    :U (move-allowed? board :U)
    :D (move-allowed? board :D)
    :W true
    :A true))
