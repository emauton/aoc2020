(ns aoc2020.day11
  (:require [aoc2020.util :as util]
            [clojure.pprint :as pp]))

(defn make-grid
  "Make a 2D grid from line input.
   access: (get-in grid [y x])
   update: (assoc-in grid [y x] val)"
  [lines]
  (mapv vec lines))

(defn neighbours
  "Return neighbour coordinates for coord in grid"
  [grid coord]
  (let [dyx (for [i (range -1 2)
                  j (range -1 2)
                  :when (not= [0 0] [i j])]
              [i j])]
    (filter (fn [possible]
              (every? #(< -1 % (count grid)) possible))
            (map (fn [delta] (vec (map + coord delta))) dyx))))

(defn neighbour-values
  "Return neighbour values for coord in grid"
  [grid coord]
  (sort (map #(get-in grid %) (neighbours grid coord))))

(defn main
  "Day 11 of Advent of Code 2020: Seating System
      lein run day11 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [grid (make-grid (util/read-lines filename))
        size (count grid)]
    (pp/pprint grid)
    (println)
    (println "neighbours [0 0]:" (neighbours grid [0 0])) ; top left
    (println "neighbours [1 1]:" (neighbours grid [1 1])) ; middle of grid
    (println "neighbours [0 1]:" (neighbours grid [0 1])) ; top side
    (println "neighbours [1 0]:" (neighbours grid [1 0])) ; left side
    (println "neighbours [n n]:" (neighbours grid [(dec size) (dec size)])) ; bottom right
    (println)
    (println "neighbour vals [0 0]:" (neighbour-values grid [0 0]))
    (println "neighbour vals [1 1]:" (neighbour-values grid [1 1]))
    (println "neighbour vals [0 1]:" (neighbour-values grid [0 1]))
    (println)
    (pp/pprint (assoc-in grid [1 1] \#))))
