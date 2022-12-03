;; # 🎄 Advent of Clerk: Day 3
(ns advent-of-clerk.day-03
  (:require [nextjournal.clerk :as clerk]
            [clojure.set :as set]
            [clojure.string :as str]))


(def raw-data (slurp "resources/day_03.txt"))

(defn char-range [from to]
  "Creates a range of chars from to"
  (map char (range (int from) (inc (int to)))))

;; Creates a map of chars \a 1 \b 2 ... \Z 52
(def char-map
  (merge
   (zipmap (char-range \a \z) (range 1 27))
   (zipmap (char-range \A \Z) (range 27 53))))

(defn find-repeated-char [s]
  "Splits a string in two, and finds repeated char"
  (let [len (count s)
        parts (split-at (/ len 2) s)]
    (first (apply set/intersection (map set parts)))))

(defn find-same-char [parts]
  "Finds the repeated char among strings a, b, c"
  (first (apply set/intersection (map set parts))))

(defn get-char-value [c]
  "Helper to avoid anon on thread"
  (get char-map c))


;; ### Puzzle 1. Find the sum of repeated items on left / right
^{::clerk/visibility {:result :hide}}
(defn p1 [input]
  (->> input
      str/split-lines
      (map find-repeated-char)
      (map get-char-value)
      (apply +)))

(p1 raw-data)

;; ### Puzzle 2. Find the sum of repeated items on a group of 3 ruckpacks
^{::clerk/visibility {:result :hide}}
(defn p2 [input]
  (->> input
       str/split-lines
       (partition 3)
       (map find-same-char)
       (map get-char-value)
       (apply +)))

(p2 raw-data)
