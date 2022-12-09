;; # 🎄 Advent of Clerk: Day 8
(ns advent-of-clerk.day-08
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

;; (def raw-data (slurp "resources/day_08_mini.txt"))
(def raw-data (slurp "resources/day_08.txt"))
(def data (str/split-lines raw-data))

(defn str->ints [s]
  (->> s
       (re-seq #"\d")
       (map parse-long)))


(defn visible-trees [rows row-pos pos]
  (let [row (nth rows row-pos)
        left (take pos row)
        right (drop (inc pos) row)
        pivot (nth row pos)]
    (when (or (empty? left)
              (empty? right)
              (> pivot (apply max left))
              (> pivot (apply max right)))
      [row-pos pos])))


;; ### Puzzle 1. Find visible trees
(defn p1 [input]
  (let [rows (map str->ints data)
        columns (apply map list rows)
        row-coords  (for [row (range (count rows))
                          tree (range (count (nth rows row)))
                          :when (visible-trees rows row tree)]
                      [row tree])
        col-coords (for [column (range (count columns))
                         tree (range (count (nth columns column)))
                         :when (visible-trees columns column tree)]
                     [tree column])]

    (count (distinct (concat row-coords col-coords)))))
(p1 data)

(assert (= (p1 data) 1870))


(defn count-trees [trees tree]
  (loop [tree-list trees
         stack []]
    (if-let [current (first tree-list)]
      (if (>= current tree)
        (conj stack current)
        (recur (rest tree-list) (conj stack current)))
      stack)))


(defn scenic-on-row [row pos]
  (let [pivot (nth row pos)
        left (count-trees (reverse (take pos row)) pivot)
        right (count-trees (drop (inc pos) row) pivot)]
    (* (count left) (count right))))


;; ### Puzzle 2. Count most scenic spot, the place with more trees at range
(defn p2 [input]
  (let [rows (map str->ints data)
        columns (apply map list rows)
        row-scenics (for [row (range (count rows))
                          tree (range (count (nth rows row)))
                          :let [scenic (scenic-on-row (nth rows row) tree)]]
                      [[row tree] scenic])
        col-scenics (for [col (range (count columns))
                          tree (range (count (nth columns col)))
                          :let [scenic (scenic-on-row (nth columns col) tree)]]
                      [[tree col] scenic])
        row-reduced (reduce (fn [acc [k v]] (assoc acc k v)) {} row-scenics)
        col-reduced (reduce (fn [acc [k v]] (assoc acc k v)) {} col-scenics)]

    (apply max (vals (merge-with * row-reduced col-reduced)))))

(p2 data)
(assert (= (p2 data) 517440))

