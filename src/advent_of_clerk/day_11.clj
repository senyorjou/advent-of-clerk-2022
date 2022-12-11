;; # 🎄 Advent of Clerk: Day 11
(ns advent-of-clerk.day-11
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))


(def raw-data (slurp "resources/day_11.txt"))
(def data (map str/split-lines (str/split raw-data #"\n\n")))


(defn divisible-by?
  "Determine if num is divisible by div"
  [div num]
  (zero? (mod num div)))


(defn parse-operation [line]
  "Parses an operation in the form `old +/* value`"
  (let [[op qty] (str/split (second (str/split line #" old ")) #" ")]
    (if (= op "*")
      (if (= qty "old")
        #(* % %)
        #(* (parse-long qty) %))
      (if (= qty "old")
        #(+ % %)
        #(+ (parse-long qty) %)))))


(defn parse-monkey [monkey-data]
  (let [monkey-map (zipmap (range) monkey-data)]
    (-> {}
        (assoc :monkey (parse-long (re-find #"\d+" (get monkey-map 0))))
        (assoc :items (mapv bigint (re-seq #"\d+" (get monkey-map 1))))
        (assoc :inspected 0)
        (assoc :operation (parse-operation (get monkey-map 2)))
        (assoc :divisible-by (parse-long (re-find #"\d+" (get monkey-map 3))))
        (assoc :true (parse-long (re-find #"\d+" (get monkey-map 4))))
        (assoc :false (parse-long (re-find #"\d+" (get monkey-map 5)))))))


(def monkeys (zipmap (range) (map parse-monkey data)))
(into (sorted-map) (map parse-monkey data))


(defn play-item [monkeys monkey-data item]
  (let [initial-wl ((:operation monkey-data) item)
        final-wl (Math/floor (/ initial-wl 3))
        divisible (divisible-by? (:divisible-by monkey-data) final-wl)
        target-monkey (if divisible (:true monkey-data) (:false monkey-data))]
    (update-in monkeys [target-monkey :items] conj final-wl)))

(defn play-monkey [monkeys monkey]
  (let [monkey-data (get monkeys monkey)
        items (count (:items monkey-data))]
    (update-in (assoc-in (reduce (fn [acc item]
                                   (play-item acc monkey-data item))
                                 monkeys (:items monkey-data))
                         [monkey :items] [])
               [monkey :inspected] + items)))



;; ### Puzzle 1. Get values after 20 rounds with 1/3 worries
(defn p1 [input]
  (let [monkeys (zipmap (range) (map parse-monkey input))  ;; {0 monkey-0 1 monkey-1...}
        turns (count monkeys)
        rounds (* 20 turns)]
    (->> (take rounds (cycle (range turns)))  ;; just calculates number of iterations
         (reduce play-monkey monkeys)         ;; which is rounds * truns, and process each
         (map second)
         (map :inspected)
         (sort >)
         (transduce (take 2) *))))


(p1 data)
(assert (= (p1 data) 120056))


(defn play-item-no-worries [monkeys monkey-data item]
  (let [initial-wl (bigint ((:operation monkey-data) item))
        processed-wl (rem initial-wl (* 11 17 7 13 2 19 3 5)) ;; yes, hardcoded, sorry :/
        divisible (divisible-by? (:divisible-by monkey-data) processed-wl)
        target-monkey (if divisible (:true monkey-data) (:false monkey-data))]
    (update-in monkeys [target-monkey :items] conj processed-wl)))

(defn play-monkey-no-worries [monkeys monkey]
  (let [monkey-data (get monkeys monkey)
        items (count (:items monkey-data))]
    (update-in (assoc-in (reduce (fn [acc item]
                                   (play-item-no-worries acc monkey-data item))
                                 monkeys (:items monkey-data))
                         [monkey :items] [])
               [monkey :inspected] + items)))

;; ### Puzzle 2. Get values after 10.000 rounds with full worries
(defn p2 [input]
  (let [monkeys (zipmap (range) (map parse-monkey input))
        turns (count monkeys)
        rounds (* 10000 turns)]
    (->> (take rounds (cycle (range turns)))
         (reduce play-monkey-no-worries monkeys)
         (map second)
         (map :inspected)
         (sort >)
         (transduce (take 2) *))))
(p2 data)

(assert (= (p2 data) 21816744824))
