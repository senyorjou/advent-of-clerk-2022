;; # 🎄 Advent of Clerk: Day 5
(ns advent-of-clerk.day-05
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; (def raw-data (slurp "resources/day_05_sample.txt"))
(def raw-data (slurp "resources/day_05.txt"))


(defn parse-crates [lines]
  (zipmap (range 1 10)
          (map #(re-seq #".{1,4}" %) (butlast (str/split-lines lines)))))


(defn parse-moves [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       (partition 3)))


(def data
  (let [[crates moves] (str/split raw-data #"\n\n")]
       {:demo-crates {1 ["N" "Z"] 2 ["D" "C" "M"] 3 ["P"]}
        :crates {1 ["V" "Q" "W" "M" "B" "N" "Z" "C"]
                 2 ["B" "C" "W" "R" "Z" "H"]
                 3 ["J" "R" "Q" "F"]
                 4 ["T" "M" "N" "F" "H" "W" "S" "Z"]
                 5 ["P" "Q" "N" "L" "W" "F" "G"]
                 6 ["W" "P" "L"]
                 7 ["J" "Q" "C" "G" "R" "D" "B" "V"]
                 8 ["W" "B" "N" "Q" "Z"]
                 9 ["J" "T" "G" "C" "F" "L" "H"]}
        :moves (parse-moves moves)}))


(defn move [crates move]
  (let [[qty from to] move
        moving-crates (take qty (get crates from))
        target-crate (concat (reverse moving-crates) (get crates to))
        origin-crate (drop qty (get crates from))]
    (assoc crates from origin-crate to target-crate)))


(defn p1 [input]
  (apply str (map first (vals (reduce (fn [acc, movement]
                                        (move acc movement))
                                      (:crates input)
                                      (:moves input))))))

(p1 data)


(comment
  (re-seq #".{1,4}" (nth (str/split-lines raw-data) 1))
  (zipmap (range 1 3) (range 1 4))
  (take 2 [1 2 3])
  (concat [1 2] (reverse [3 4]))

  (get (move (move (:crates data) (first (:moves data))) (second (:moves data))) 6)

  (get (move (:crates data) (first (:moves data))) 1)

  (reduce (fn [acc, movement]
            (move acc movement))
          (:crates data)
          (:moves data))

  (map first (vals {1 [1 2 3] 2 [1 2 3]}))
  (take 10 (reverse (:moves data)))

  ;; JCTWQWJPW
  ;; JQWVHNWJZ
  ;;
  ;; JQWVHNWJZ

;;     [D]
;; [N] [C]
;; [Z] [M] [P]
;;  1   2   3

;; move 1 from 2 to 1
;; move 3 from 1 to 3
;; move 2 from 2 to 1
;; move 1 from 1 to 2

;; [V]         [T]         [J]
;; [Q]         [M] [P]     [Q]     [J]
;; [W] [B]     [N] [Q]     [C]     [T]
;; [M] [C]     [F] [N]     [G] [W] [G]
;; [B] [W] [J] [H] [L]     [R] [B] [C]
;; [N] [R] [R] [W] [W] [W] [D] [N] [F]
;; [Z] [Z] [Q] [S] [F] [P] [B] [Q] [L]
;; [C] [H] [F] [Z] [G] [L] [V] [Z] [H]
  )
