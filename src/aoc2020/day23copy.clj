(ns aoc2020.day23
  (:require [aoc2020.util :as util]
            [clojure.string :as s]))

(defn reorder
  "Reorder cups so that current is the last element"
  [cups current]
  (let [n-index (inc (.indexOf cups current))]
    (case n-index
      (dec (count cups)) cups
      (into [] (concat (subvec cups n-index) (subvec cups 0 n-index))))))

(defn find-dest
  "Return the index in cups of the cup with a label of (dec current). If no such cup
   exists subtract 1 until found (wrapping around to 9 if 0 is reached)"
  [cups current-label]
  (let [dest-label (if (= 1 current-label) 
                     9 
                     (dec current-label))
        dest-index (.indexOf cups dest-label)]
    (if (= -1 dest-index) 
      (find-dest cups dest-label)
      dest-label)))

(defn move
  [[cups current mcount]]
  (let [rcups (reorder cups current)
        triple (into [] (take 3 rcups))
        remaining (into [] (drop 3 rcups))
        dest-label (find-dest remaining current)
        new-cups (into [] (concat (reorder remaining dest-label) triple))
        new-current (nth new-cups (inc (.indexOf new-cups current)))]
;    (println "move " mcount)
;    (println "cups:" cups "(" current ")")
;    (println "triple" triple)
;    (println "remaining" remaining)
;    (println "dest-label" dest-label)
;    (println "new-cups" new-cups)
;    (println "new-current" new-current)
    [new-cups new-current (inc mcount)]))

;(last (take 101 (iterate move [[3 8 9 1 2 5 4 6 7] 3 1])))
;(last (take 101 (iterate move [[1 9 3 4 6 7 2 5 8] 1 1])))

(defn main
  "Day 23 of Advent of Code 2020: Crab Cups
      lein run day23 filename"
  [[filename]]
  (let [example [3 8 9 1 2 5 4 6 7]
        cups [1 9 3 4 6 7 2 5 8]]
    (println "example:" example)))
