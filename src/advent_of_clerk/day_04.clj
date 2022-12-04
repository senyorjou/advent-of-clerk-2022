;; # 🎄 Advent of Clerk: Day 4
(ns advent-of-clerk.day-04
  (:require [nextjournal.clerk :as clerk]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (str/split-lines (slurp "resources/day_04.txt")))

(defn parse-line [line]
  (map parse-long (re-seq #"\d+" line)))

;; ##### `contained?` checks if following passes
;; - from-1 is <= than from-2 and...
;; - from-2 is <= than to-2 and...
;; - to-2 is <= than to-1
;; - ... or the inverse
(defn contained? [[f1 t1 f2 t2]]
  (or (<= f1 f2 t2 t1)
      (<= f2 f1 t1 t2)))

;; ##### `overlaps?` checks if following passes
;; - from-2 is between from-1 and to-1 or ...
;; - to-2 is between from-1 and to-1
;; - ... and the rest
(defn overlaps? [[f1 t1 f2 t2]]
  (or (<= f1 f2 t1)
      (<= f1 t2 t1)
      (<= f2 f1 t2)
      (<= f2 t1 t2)))



;; ### Puzzle 1. Find contained ranges
(defn p1 [input]
  (->> input
      (map parse-line)
      (filter contained?)
      count))

(p1 data)


;; ### Puzzle 2. Find overlapping ranges
(defn p2 [input]
  (->> input
      (map parse-line)
      (filter overlaps?)
      count))

(p2 data)
