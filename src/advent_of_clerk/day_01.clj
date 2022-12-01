;; # 🎄 Advent of Clerk: Day 1
(ns advent-of-clerk.day-01
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(defn parse-and-aggregate
 "Gets chunks of elf data parses and sums"
  [lines]
  (apply + (map #(Integer/parseInt  %) (str/split-lines lines))))

;; Input data
;;
(def raw-data (slurp "resources/day_01.txt"))
(def data (map parse-and-aggregate  (str/split raw-data #"\n\n")))

;; Find the max value
(defn p1 [input]
  (apply max input))

;; Find the sum of 3 max values
(defn p2 [input]
  (apply + (take 3 (sort > input))))

;; ##### Results
(p1 data)
(p2 data)
