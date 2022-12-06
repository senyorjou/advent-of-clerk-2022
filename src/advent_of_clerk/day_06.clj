;; # 🎄 Advent of Clerk: Day 6
(ns advent-of-clerk.day-06
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def data (slurp "resources/day_06.txt"))

(defn some-repeated?
  "Checks if sample has repeated chars"
  [sample]
  (apply (complement distinct?) sample))


;; ### Puzzle 1. Find first sequence of unique chars of len `size`
(defn solve [size input]
  (->> input
       (partition size 1)
       (take-while some-repeated?)
       count
       (+ size)))

;; ### Puzzle 2. Find start of packet
(defn p1 [input]
  (solve 4 input))

(p1 data)


;; ### Puzzle 1. Find start of message
(defn p2 [input]
  (solve 14 input))

(p2 data)
