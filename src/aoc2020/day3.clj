(ns aoc2020.day3
  (:require [aoc2020.util :as util] ))

(defn tree-line
  "Parse a line of '.' and '#' characters into a vector of booleans. '#' being true"
  [line]
  (mapv #(= % \#) line))

(defn make-map
  "Make a 2d vector of toboggan map"
  [lines]
  (mapv tree-line lines))

(defn make-coords
  "Create a sequence of the coordinates in the map that should be checked for trees"
  [tree-map drow dcol]
  (let [height (count tree-map)
        width (count (first tree-map)) 
        rows (range 0 height drow)
        cols (map #(mod % width) (take (count rows) (iterate (partial + dcol) 0) ))]
    (map vector rows cols)))

(defn count-trees
  "Count trees encountered in tree-map at angle described by dcol, drow"
  [tree-map drow dcol]
  (count (filter #(get-in tree-map %) (make-coords tree-map drow dcol))))

(defn main
  "Day 3 of Advent of Code 2020: 
      lein run day3 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (println (count-trees (make-map (util/read-lines filename)) 1 3)))
