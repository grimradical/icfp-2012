(ns icfp.viz
  (:require [icfp.core :as core])
  (:use quil.core)
  )

(def sprite-size 16)

(defn draw-game
  [{:keys [board] :as game}]
  (let [trans-x #(* sprite-size %)
        trans-y #(* sprite-size (- (inc (core/height board)) %))]
    (fill 0 0 0)
    (doseq [[x ys] board
            [y obj] ys
            :let [x (trans-x x)
                  y (trans-y y)]]
      (if-let [i (images obj)]
        (image i (- x sprite-size) (- y sprite-size))
        (do
          (apply fill (colors obj))
          (rect (- x sprite-size) (- y sprite-size) 16 16))))))

(defn setup-with-game
  [{:keys [board] :as game}]
  (fn []
    (def images {
                 :R (load-image "/Users/deepak/Downloads/icfp/miner.bmp.gif")
                 :* (load-image "/Users/deepak/Downloads/icfp/rock.bmp.gif")
                 :L (load-image "/Users/deepak/Downloads/icfp/lift.bmp.gif")
                 :O (load-image "/Users/deepak/Downloads/icfp/openlift.bmp.gif")
                 :# (load-image "/Users/deepak/Downloads/icfp/bricks.bmp.gif")
                 :> (load-image "/Users/deepak/Downloads/icfp/lambda.bmp.gif")
                 })

    (def colors {:_ [0 0 0]
                 :. [226 202 202]})
    (smooth)
    (draw-game game)))

(defn game-to-sketch
  [{:keys [board] :as game}]
  (let [_ "aawef"
        w (* sprite-size (core/width board))
        h (* sprite-size (core/height board))]
    (sketch
     :setup (setup-with-game game)
     :title "Digger!"
     :size [(* 1.5 w) (* 1.5 h)]
     )))
