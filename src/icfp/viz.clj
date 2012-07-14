(ns icfp.viz
  (:require [icfp.core :as core])
  (:use quil.core))

(def sprite-size 16)

(declare images)
(declare colors)

(defn draw-game
  [{:keys [board water] :as game}]
  (let [trans-x #(* sprite-size %)
        trans-y #(* sprite-size (- (inc (core/height board)) %))]
    (fill 0 0 0)
    (doseq [[x ys] board
            [y obj] ys
            :let [xp (trans-x x)
                  yp (trans-y y)]]
      (if-let [i (images obj)]
        (image i (- xp sprite-size) (- yp sprite-size))
        (do
          (apply fill (colors obj))
          (rect (- xp sprite-size) (- yp sprite-size) 16 16))))))

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
                 :. [139 69 19]})
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
