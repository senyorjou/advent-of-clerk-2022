;; # 🎄 Advent of Clerk: Day 5
(ns advent-of-clerk.day-05
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def raw-data (slurp "resources/day_05.txt"))
(def only-columns (slurp "resources/day_05_columns.txt" ))


(defn parse-moves [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       (partition 3)))


(def data
  (let [[crates moves] (str/split raw-data #"\n\n")]
       {:demo-crates {1 ["N" "Z"] 2 ["D" "C" "M"] 3 ["P"]}
        :crates (sorted-map 1 ["V" "Q" "W" "M" "B" "N" "Z" "C"]
                            2 ["B" "C" "W" "R" "Z" "H"]
                            3 ["J" "R" "Q" "F"]
                            4 ["T" "M" "N" "F" "H" "W" "S" "Z"]
                            5 ["P" "Q" "N" "L" "W" "F" "G"]
                            6 ["W" "P" "L"]
                            7 ["J" "Q" "C" "G" "R" "D" "B" "V"]
                            8 ["W" "B" "N" "Q" "Z"]
                            9 ["J" "T" "G" "C" "F" "L" "H"])
        :moves (parse-moves moves)}))


(defn move [crates move f]
  (let [[qty from to] move
        moving-crates (take qty (get crates from))
        target-crate (concat (f moving-crates) (get crates to))
        origin-crate (drop qty (get crates from))]
    (assoc crates from origin-crate to target-crate)))


(defn p1 [input]
  (let [final-crates (reduce (fn [acc, movement]
                               (move acc movement reverse))
                             (:crates input)
                             (:moves input))]
    (->> final-crates
        vals
        (map first)
        (apply str))))

(p1 data)

(defn p2 [input]
  (let [final-crates (reduce (fn [acc, movement]
                               (move acc movement identity))
                             (:crates input)
                             (:moves input))]
    (->> final-crates
        vals
        (map first)
        (apply str))))

(p2 data)
