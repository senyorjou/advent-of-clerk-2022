;; # 🎄 Advent of Clerk: Day 9
(ns advent-of-clerk.day-09
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))


(def raw-data (slurp "resources/day_09.txt"))
(def data (str/split-lines raw-data))

(def dir-stream
  ;; stores movement in a one-step list
  ;; R2U3 ==> RRUUU
  (reduce (fn [acc value]
            (let [[dir units] (str/split value #" ")]
              (concat acc (repeat (parse-long units) dir))))
          [] data))


(def head-path
  ;; creates all the coordinates for HEAD
  (reductions (fn [[x y] dir]
                (case dir
                  "R" [(inc x) y]
                  "L" [(dec x) y]
                  "U" [x (inc y)]
                  "D" [x (dec y)]))
              [0 0] dir-stream))


(defn onestep
  "Moves from tail to head but one step maximum"
  [head tail]
  (let [diff (- head tail)]
    (cond
      (< diff 0) -1
      (= diff 0) 0
      (> diff 0) 1)))

(defn follow-head
  "Creates an entry on tail path, which is the same if needs no movement"
  [[hx hy] [tx ty]]
  (if (and (<= (abs (- hx tx)) 1)
           (<= (abs (- hy ty)) 1))
    ;; No movement, H & T are touching
    [tx ty]
    ;; Else move towards head but only by 1 step maximum
    [(+ tx (onestep hx tx))
     (+ ty (onestep hy ty))]))

(defn make-tail-path
  "Creates a trail of points for the tail following head"
  [head-path]
  (reductions (fn [acc head-pos]
                (follow-head head-pos acc))
              head-path))


;; ### Puzzle 1. Find places where rope tail has been
(defn p1 []
  (-> head-path
      make-tail-path
      distinct
      count))

(p1)
(assert (or (= (p1) 6087)
            (= (p1) 13))) ;; for test data

;; ### Puzzle 2. Find places where rope nth 9 tail has been
;; #### tail-1 will be used as head for tail-2 and recursivelly...
(defn p2 []
  (-> (iterate make-tail-path head-path)
      (nth 9)
      distinct
      count))

(p2)

(assert (= (p2) 2493))


(clerk/plotly {:data [{:x (map first head-path)
                       :y (map second head-path)
                       :mode "markers"
                       :type "scatter"
                       :marker {:size 2}}]

               :layout {:showlegend false
                        :xaxis {:visible false
                                :showgrid false
                                :zeroline false
                                :showline false}
                        :yaxis {:visible false
                                :showgrid false
                                :zeroline false
                                :showline false}
                        :margin {:l 20 :r 0 :b 20 :t 20}}
               :config {:displayModeBar false
                        :displayLogo false
                        :showTips false
                        :staticPlot true}})
