(ns user
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.index :as index]))

(clerk/serve! {:port 7878 :browse true})

(comment
  (clerk/build! {:paths (index/build-paths) :browse true :bundle true})

  (clerk/build! {:paths (index/build-paths) :bundle true})
  (clerk/show! "src/advent_of_clerk/day_05.clj")


)
