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
  "Get coordinates in tree-map that should be checked for trees along deltas drow,dcol path"
  [tree-map [drow dcol]]
  (let [height (count tree-map)
        width (count (first tree-map)) 
        rows (range 0 height drow)
        cols (->> (iterate (partial + dcol) 0)
                  (take (count rows))
                  (map #(mod % width)))]
    (map vector rows cols)))

(defn count-trees
  "Count trees encountered in tree-map at angle described by deltas"
  [tree-map deltas]
  (->> (make-coords tree-map deltas)
       (filter #(get-in tree-map %))
       (count)))

(defn test-slopes
  "Return the product of the numbers of trees hit when taking the slopes"
  [tree-map slopes]
  (apply * (map #(count-trees tree-map %) slopes)))

(defn main
  "Day 3 of Advent of Code 2020: Toboggan Trajectory
      lein run day3 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [slopes [[1,1] [1,3] [1,5] [1,7] [2,1]]
        tree-map (make-map (util/read-lines filename))]
    (println (test-slopes tree-map slopes))))
