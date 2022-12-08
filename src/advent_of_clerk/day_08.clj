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


(def rows (map str->ints data))
(def columns (apply map list rows))

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

(defn visible-tree-on-row [row-pos pos]
  (let [row (nth rows row-pos)
        left (take pos row)
        right (drop (inc pos) row)
        pivot (nth row pos)]
    (if (or (empty? left)
            (empty? right)
            (> pivot (apply max left))
            (> pivot (apply max right)))
      [row-pos pos]
      false)))

(defn visible-tree-on-column [row-pos pos]
  (let [row (nth columns row-pos)
        left (take pos row)
        right (drop (inc pos) row)
        pivot (nth row pos)]
    (if (or (empty? left)
            (empty? right)
            (> pivot (apply max left))
            (> pivot (apply max right)))
      [row-pos pos]
      false)))


;; (def row-coords
;;   (for [row (range (count rows))
;;         tree (range (count (nth rows row)))
;;         :when (visible-tree-on-row row tree)]
;;   [row tree]))

;; (def col-coords
;;   (for [column (range (count columns))
;;         tree (range (count (nth columns column)))
;;         :when (visible-tree-on-column column tree)]
;;     [tree column]))


;; ### Puzzle 1. Find visible trees
(defn p1 [input]
  (let [row-coords  (for [row (range (count rows))
                          tree (range (count (nth rows row)))
                          :when (visible-tree-on-row row tree)]
                      [row tree])
        col-coords (for [column (range (count columns))
                         tree (range (count (nth columns column)))
                         :when (visible-tree-on-column column tree)]
                     [tree column])]

    (count (distinct (concat row-coords col-coords)))))

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

(def row-scenics
  (for [row (range (count rows))
        tree (range (count (nth rows row)))
        :let [scenic (scenic-on-row (nth rows row) tree)]]
  [[row tree] scenic]))

(def col-scenics
  (for [col (range (count columns))
        tree (range (count (nth columns col)))
        :let [scenic (scenic-on-row (nth columns col) tree)]]
  [[tree col] scenic]))

(def row-scenics-reduced (reduce (fn [acc [k v]]
                                   (assoc acc k v)) {} row-scenics))

(def col-scenics-reduced (reduce (fn [acc [k v]]
                                   (assoc acc k v)) {} col-scenics))

(apply max (vals (merge-with * row-scenics-reduced col-scenics-reduced)))


(comment
;; 30373
;; 25512
;; 65332
;; 33549
;; 35390
;; 3 2 6 3 3
;; 0 5 5 3 5
;; 3 5 3 5 3
;; 7 1 3 4 9
;; 3 2 2 9 0
;;
;;
;;RESPONSE 2152
)
