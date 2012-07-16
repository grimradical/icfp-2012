(ns icfp.viz
  (:require [icfp.core :as core])
  (:use quil.core))

(def sprite-size 24)

(declare images)
(declare colors)

(defn draw-game
  [{:keys [board current-goal overlays] :as game :or {overlays {}}}]
  (let [trans-x #(* sprite-size %)
        trans-y #(* sprite-size (- (inc (core/height board)) %))]

    (doseq [[x ys] board
            [y obj] ys
            :let [xp (trans-x x)
                  yp (trans-y y)
                  outline (cond
                            (= [x y] current-goal)
                            :goal)]]

      (stroke-weight 0.5)
      (stroke 100 100 100)

      (when-let [i (images obj)]
        (image i (- xp sprite-size) (- yp sprite-size) sprite-size sprite-size))

      (when-let [c (colors obj)]
        (apply fill c)
        (rect (- xp sprite-size) (- yp sprite-size) sprite-size sprite-size)
        (no-fill))

      (when-let [c (colors outline)]
        (no-fill)
        (apply stroke c)
        (stroke-weight 2)
        (rect (- xp sprite-size) (- yp sprite-size) (- sprite-size 2) (- sprite-size 2))
        (no-stroke))

      (when-let [txt (overlays [x y])]
        (fill 255 255 255)
        (text-align :left :top)
        (text-size 8)
        (text (str txt) (- xp sprite-size) (- yp sprite-size))
        (no-fill))
      )))

(defn setup-with-game
  [{:keys [board] :as game}]
  (fn []
    (def images {
                 :R (load-image "resources/miner.bmp.gif")
                 :* (load-image "resources/rock.bmp.gif")
                 :L (load-image "resources/lift.bmp.gif")
                 :O (load-image "resources/openlift.bmp.gif")
                 :# (load-image "resources/bricks.bmp.gif")
                 :> (load-image "resources/lambda.bmp.gif")
                 })

    (def colors {:_ [0 0 0]
                 :. [139 69 19]
                 :W [0 255 0]
                 :goal [0 255 255]})
    (smooth)))

(defn game-to-sketch
  [game-ref]
  (let [{:keys [board] :as game} (deref game-ref)
        w (* sprite-size (core/width board))
        h (* sprite-size (core/height board))]
    (sketch
     :setup (setup-with-game game)
     :title "Digger!"
     :draw (fn []
             (draw-game (deref game-ref)))
     :size [(* 1.5 w) (* 1.5 h)]
     )))
