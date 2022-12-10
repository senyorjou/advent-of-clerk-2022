;; # 🎄 Advent of Clerk: Day 10
(ns advent-of-clerk.day-10
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))


;; (def raw-data (slurp "resources/day_10_small.txt"))
(def raw-data (slurp "resources/day_10.txt"))
(def data (str/split-lines raw-data))

(defn parse-instructions [input]
  "Creates a list of values to add on each cycle, noop adds 0, each addx adds a noop in front"
  (reduce (fn [acc, line]
            (if (= "noop" line)
              (conj acc 0)
              (conj acc 0 (parse-long (second (str/split line #" "))))))
          [] input))

(defn accumulate-cycles [input]
  "Creates a list of accumulated values for each cycle"
  (reductions (fn [acc, qty]
                (+ acc qty))
              1 input))

;; ### Puzzle 1. Get values from cycles 20 60 100 140 180 220
(defn p1 [input]
  (let [instructions (parse-instructions input)
        cycles (accumulate-cycles instructions)]
    (apply + (map (fn [step]
                    (* (nth cycles (dec step)) step))
                  [20 60 100 140 180 220]))))

(p1 data)
(assert (or (= (p1 data) 17180)
            (= (p1 data) 13140))) ;; demo


(defn draw-pixel [[pos value]]
  "Draws # if pos is +- 1 the value"
  (if (<= -1 (- value pos) 1) "#" "."))

(defn map-values-to-screen [row]
  "Engine for drawing a pixel. If current position is on +- 1 of cycle value draws a #, else ."
  (let [key-values (map-indexed vector row)]
    (str/join (map draw-pixel key-values))))

;; ### Puzzle 2. Draw 40 chars lines with mapped values of the cycles
(defn p2 [input]
  (-> input
      parse-instructions
      accumulate-cycles
      (->> (partition 40)
           (map map-values-to-screen))))


(p2 data)


;; ### Puzzle 2': Visual reprtesentation
(defn get-row-coordinates [row line]
  (keep-indexed (fn [idx val]
               (if (= val \#)
                   [(* 5 idx) (* row 2)]))
             line))

(defn draw-message [input]
  (let [lines (reverse (p2 input))]
    (apply concat (map get-row-coordinates (range) lines))))

(clerk/plotly {:data [{:x (map first (draw-message data))
                       :y (map second (draw-message data))
                       :mode "markers"
                       :type "scatter"
                       :marker {:size 25}}]

               :layout {:showlegend false
                        :xaxis {:visible false
                                :showgrid false
                                :zeroline false
                                :showline false}
                        :yaxis {:visible false
                                :showgrid false
                                :zeroline false
                                :showline false
                                :range [-10  24]
                                :type "linear"
                                :autorange false}
                        :margin {:l 0 :r 0 :b 0 :t 0}}
               :config {:displayModeBar false
                        :displayLogo false
                        :showTips false
                        :staticPlot true}})
