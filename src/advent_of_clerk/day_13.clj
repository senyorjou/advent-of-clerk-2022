;; # 🎄 Advent of Clerk: Day 13
(ns advent-of-clerk.day-13
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))


(def raw-data (slurp "resources/day_13.txt"))
;; (def raw-data (slurp "resources/day_13_sample.txt"))
(def data (read-string (str "[" raw-data "]")))



(defn compare-lr
  ([[left right]]
  (compare-lr left right))

  ([left right]
  (cond
    (and (integer? left) (integer? right))
    (cond (< left right) :left
          (> left right) :right
          :else :equal)

    (integer? left)
    (compare-lr [left] right)

    (integer? right)
    (compare-lr left [right])

    (and (seqable? left) (seqable? right))
    (loop [i 0]
      (cond
        (and (= (count left) (count right))
             (every? #{:equal} (map compare-lr left right))) :equal
        (= i (count left)) :left
        (= i (count right)) :right
        :else
        (let [left-right (compare-lr (nth left i) (nth right i))]
          (if (= :equal left-right)
            (recur (inc i))
            left-right))))
    :else :equal)))


;; ### Puzzle 1. Filter valid packet data
(defn p1 [input]
  (->> input
       (partition 2)
       (map compare-lr)
       (map-indexed vector)                 ;; index results
       (filter (comp #(= :left %) second))  ;; get only lefts
       (map first)                          ;; take index
       (map inc)                            ;; 1-indexed
       (apply +)))

(p1 data)
(assert (= 5013 (p1 data)))


;; ### Puzzle 2. Find decoder key
(defn p2 [input]
  (let [sort-keys {:left -1 :right 1 :equal 0}
        sorter #((compare-lr %1 %2) sort-keys)
        sorted (->> (conj input [[2]] [[6]])
                    (sort sorter))]
    (* (inc (.indexOf sorted [[2]]))
       (inc (.indexOf sorted [[6]])))))

(p2 data)

(assert (= 25038 (p2 data)))
