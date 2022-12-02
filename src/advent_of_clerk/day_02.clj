;; # 🎄 Advent of Clerk: Day 2
(ns advent-of-clerk.day-02
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; #### Data
;; (def raw-data (slurp "resources/day_02_sample.txt"))
(def raw-data (slurp "resources/day_02.txt"))
(def data (str/split-lines raw-data))
(def card-values {"X" 1 "Y" 2 "Z" 3})

;; #### Utility fns
(defn parse-turn [turn]
  (re-seq #"(\w) (\w)" turn))

(defn play-turn
  "Computes the outcome of a match"
  [player-1 player-2]
  (let [[loose draw win] [0 3 6]]
    (match [player-1 player-2]
       ["A" "X"] draw
       ["A" "Y"] win
       ["A" "Z"] loose
       ["B" "X"] loose
       ["B" "Y"] draw
       ["B" "Z"] win
       ["C" "X"] win
       ["C" "Y"] loose
       ["C" "Z"] draw)))

(defn choose-matching-card
  "Select a card that satisfies the result
  conditions X loose, Y draw & Z win"
  [player-1 condition]
  (match [player-1 condition]
     ["A" "X"] "Z"
     ["A" "Y"] "X"
     ["A" "Z"] "Y"
     ["B" "X"] "X"
     ["B" "Y"] "Y"
     ["B" "Z"] "Z"
     ["C" "X"] "Y"
     ["C" "Y"] "Z"
     ["C" "Z"] "X"))


(defn points-strategy-1
  "Calculates points for a turn. Match value + option value"
  [turn]
    (let [[[_ player-1 player-2]] (parse-turn turn)
          my-hand (get card-values player-2)
          turn-result (play-turn player-1 player-2)]
      (+ turn-result my-hand)))


(defn points-strategy-2
  "Calculates points for a turn.
  Match value + option value"
  [turn]
    (let [[[_ player-1 player-2]] (parse-turn turn)
          player-2-hand (choose-matching-card player-1 player-2)
          my-hand (get card-values player-2-hand)
          turn-result (play-turn player-1 player-2-hand)]
      (+ turn-result my-hand)))



;; ### Puzzle 1 find result of a play using first strategy
(defn p1 [input]
  (apply + (map points-strategy-1 input)))

(p1 data)

;; ### Puzzle 2 find result of a play using second strategy
(defn p2 [input]
  (apply + (map points-strategy-2 input)))

(p2 data)
