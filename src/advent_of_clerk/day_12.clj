;; # 🎄 Advent of Clerk: Day 12
(ns advent-of-clerk.day-12
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))


;; (def raw-data (slurp "resources/day_12.txt"))
(def raw-data (slurp "resources/day_12_sample.txt"))
(def data (str/split raw-data  #"\n"))


(def letters "abcdefghijklmnopqrstuvwxyz")

(def deltas [[1 0] [-1 0] [0 1] [0 -1]])

(defn char->num [ch]
  (case ch
    \S {:start true :el 0}
    \E {:end true :el (+ 2 (- (int \z) (int \a)))}
    {:el (inc (- (int ch) (int \a)))}))


(def area-map (reduce (fn [acc, [k, v]]
                   (assoc acc k v))
                 {} (for [[y row] (map-indexed vector data)
                          [x val] (map-indexed vector row)]
                      [[x y] (char->num val)])))



(defn start-point? [[k v]]
  "Find start point"
  (when (:start v)
    k))

(defn valid-move? [from to]
   (let [el-from (get-in area-map [from :el])
         el-to (get-in area-map [to :el])]
     (>= el-from (dec el-to))))



(defn next-move [[x y]]
  (for [[dx dy] deltas
        :let [next-point [(+ x dx) (+ y dy)]]
        :when (and (area-map next-point)
                   (valid-move? [x y] [dx dy]))]
    next-point))


;; (loop [to-visit (into clojure.lang.PersistentQueue/EMPTY [(filter some? (map start-point? area-map))])
;; ;; (loop [to-visit [[0 0]]
;;        top 0]
;;   (println "POINTS " top)
;;   (if (> top 3)
;;     to-visit
;;     (let [points (into [] (next-move (peek to-visit)))]
;;       (recur (into (pop to-visit) points)
;;              (inc top)))))


;; (loop [top 0]
;;   (println "POINTS " top)
;;   (if (> top 3)
;;     top
;;     (recur (inc top))))




;; (into [] (next-move [0 0]))

;; (valid-move? [0 3] [1 3])
;; (area-map [0 3])
;; (area-map [1 3])


;; (next-move (ffirst (filter start-point? area-map)))

;; (peek [[0 0][1 0] [0 1]])
;; (into (pop [1 2 3]) [4])
;; (into [] (next-move [0 0]))

;; (filter some? (map start-point? area-map))
;; (peek (into clojure.lang.PersistentQueue/EMPTY  [(filter some? (map start-point? area-map))]))
;; (def a (into (pop (into clojure.lang.PersistentQueue/EMPTY  [(filter some? (map start-point? area-map))])) (next-move [0 0])))

;; (peek (pop (pop a)))(peek a)

;; (->> [[0 0]]
;;     (into clojure.lang.PersistentQueue/EMPTY)
;;     pop
;;     seq
;;     )
