(ns aoc2020.day23
  (:require [aoc2020.util :as util]
            [clojure.string :as s]))

(defn loop-inc
  [index max]
  (if (= index max) 0 (inc index)))

(defn remove-3 
  "Return a pair: the vector of three cups, and the vector of cups with 
   three removed. Index is the index of the first of the three cups"
  [cups index]
  (case index
    0 [(subvec cups 0 3) (subvec cups 3)]
    6 [(subvec cups 6) (subvec cups 0 index)]
    7 [(into [] (concat (subvec cups index) (subvec cups 0 1))) (subvec cups (- index 6) index)]
    8 [(into [] (concat (subvec cups index) (subvec cups 0 2))) (subvec cups (- index 6) index)]
    [(subvec cups index (+ index 3)) (into [] (concat (subvec cups 0 index) (subvec cups (+ index 3))))]))

(defn insert-3
  "Return a vector of cups with three inserted. Index is the index where the first of 
   the new elements should be inserted"
  [cups index new-cups]
  (case index 
    0 
    1
    2
    3
    4
    5 (into [] (concat cups new-cups))
    6
    7
    8 (into [] (concat new-cups cups))

    0 (into [] (concat new-cups cups))
    7 (into [] (concat (subvec new-cups 2) cups (subvec new-cups 0 2)))
    ;8 (into [] (concat (subvec new-cups 1) cups (subvec new-cups 0 1)))
    8 (into [] (concat cups new-cups))
    (into [] (concat (subvec cups 0 index) new-cups (subvec cups index)))))

(defn find-dest
  "Return the index in cups of the cup with a label of (dec current). If no such cup
   exists subtract 1 until found (wrapping around to 9 if 0 is reached)"
  [cups current-label]
  (let [dest-label (if (= 1 current-label) 
                     9 
                     (dec current-label))
        dest-index (.indexOf cups dest-label)]
    (if (= -1 dest-index) (find-dest cups dest-label) dest-index)))

(defn new-current
  [cups current]
  (let [current-index (.indexOf cups current)
        next-index (loop-inc current-index (count cups))]
    (println "next-index" next-index)
    (get cups next-index)))

(defn move
  [[cups current]]
  (let [current-index (.indexOf cups current)
        [three others] (remove-3 cups (loop-inc current-index (count cups)))
        dest-index (find-dest others current)
        new-cups (insert-3 others (loop-inc dest-index (count others)) three)
        new-curr (new-current new-cups current)]
    (println "new cups" new-cups new-curr)
    [new-cups new-curr]))

(defn main
  "Day 23 of Advent of Code 2020: Crab Cups
      lein run day23 filename"
  [[filename]]
  (let [example [3 8 9 1 2 5 4 6 7]
        cups [1 9 3 4 6 7 2 5 8]
        [new old] (remove-3 cups 3)]
    (println "insert at 0" (insert-3 old 0 new))
    (println "insert at 1" (insert-3 old 2 new))
    (println "insert at 2" (insert-3 old 3 new))
    (println "insert at 3" (insert-3 old 4 new))
    (println "insert at 4" (insert-3 old 5 new))
    (println "insert at 5" (insert-3 old 6 new))
  (println new old)))
;    (println "one move on example:" (move [example 3]))
;    (println "100 moves on example:" (last (take 100 (iterate move [example 3]))))
;));    (println "100 moves on example:" (last (take 101 (iterate move [example 3]))))))
