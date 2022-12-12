;; # 🎄 Advent of Clerk: Day 12
(ns advent-of-clerk.day-12
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))


(def raw-data (slurp "resources/day_12.txt"))
;; (def raw-data (slurp "resources/day_12_sample.txt"))
(def data (str/split raw-data  #"\n"))


(def letters "abcdefghijklmnopqrstuvwxyz")

(def deltas [[1 0] [-1 0] [0 1] [0 -1]])

(defn char->num [ch]
  (case ch
    \S {:start true :e 0}
    \E {:end true :e (+ 2 (- (int \z) (int \a)))}
    {:e (inc (- (int ch) (int \a)))}))


(def area-map (reduce (fn [acc, [k, v]]
                   (assoc acc k v))
                 {} (for [[y row] (map-indexed vector data)
                          [x val] (map-indexed vector row)]
                      [[x y] (char->num val)])))



(defn start-point? [[k v]]
  "Find start point"
  (when (:start v)
    k))


(defn next-mov [[x y]]
  (for [[dx dy] deltas
        :let [next-point [(+ x dx) (+ y dy)]]
        :when (and (area-map next-point))]
    next-point)
  )


(next-mov [0 0])

(filter start-point? area-map)
