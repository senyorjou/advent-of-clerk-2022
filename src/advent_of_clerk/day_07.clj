;; # 🎄 Advent of Clerk: Day 7
(ns advent-of-clerk.day-07
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))


(def raw-data (slurp "resources/day_07.txt"))
(def data (str/split-lines raw-data))



;; ### Create tree
(defn create-tree [input]
  (loop [lines input
         tree {}          ;; actual tree, void initially
         dir-stack []]    ;; current dir-stack ["a" "b" "c"...]

    (let [line-entry (first lines)]
      (if (nil? line-entry)
        ;; Final tree parsed
        tree
        ;; if lines... continue parsing
        (cond
          ;; `ls` and `dir` have no effect, just recurr next line
          (or (= "$ ls" line-entry)
              (str/starts-with? line-entry "dir "))
          (recur (rest lines) tree dir-stack)

          ;; when cd we set current dir `dir-stack` vector
          (str/starts-with? line-entry "$ cd ")
          (let [new-dir (subs line-entry 5)]
            (case new-dir
              ;; on "/" we set to root
              "/" (recur (rest lines) tree ["/"])

              ;; on ".." we pop last entry
              ".." (recur (rest lines) tree (pop dir-stack))

              ;; if is a new dir we *add* the directory to dir-stack stack
              ;; next commands will occur in that dir space
              (recur (rest lines) tree (conj dir-stack new-dir))))

          ;; we hit a file, dir-stack is set
          ;; we update the tree using update-in to add the file to current dir
          :else
          (recur (rest lines)
                 (let [[file-size file-name] (str/split line-entry #" ")]
                   (update-in tree dir-stack assoc file-name (parse-long file-size)))
                 dir-stack))))))


(defn get-traversal
  "Builds a list of entries recursivelly"
  [tree path]
  (apply concat
         (when (seq path) [path]) ;; oops prevent initial
         (for [[new-path-name path-contents] tree
               :when (map? path-contents)]
           (get-traversal path-contents (conj path new-path-name)))))

;; `total-dir-size` uses tail recursion to compute the size of a directory
(def total-dir-size
  (memoize (fn [node]
             (let [entries (for [[_key node-or-value] node]
                             (if (map? node-or-value)
                               (total-dir-size node-or-value) ;; Here entry is a mini FS
                               node-or-value))]
               (apply + entries)))))

;; ### Puzzle 1. Find numbers of directories greater than 100000
(defn p1 [input]
  (let [tree (create-tree input)]
    (-> tree
        (get-traversal [])
        (->> (map #(total-dir-size (get-in tree %)))
             (filter #(>= 100000 %))
             (apply +)))))

(p1 data)

;; ### Puzzle 1. Find a directory that frees some space to reach at least 30000000
(defn p2 [input]
  (let [tree (create-tree input)
        space-free (- 70000000 (total-dir-size tree))
        needed-space (- 30000000 space-free)]

    (-> tree
        (get-traversal [])
        (->> (map #(total-dir-size (get-in tree %)))
             (filter #(>= % needed-space))
             sort
             first))))

(p2 data)
