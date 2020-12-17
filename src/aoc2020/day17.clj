(ns aoc2020.day17
  (:require [aoc2020.util :as util] 
            [clojure.pprint :as pp]
            [clojure.string :as s]))

(defn neighbours
  "Return neighbour coordinates for coord (in infinite space)
   Index in slice, row, col order - z y x"
  [coord]
  (let [dzyx (for [i (range -1 2)
                   j (range -1 2)
                   k (range -1 2)
                   :when (not= [0 0 0] [i j k])]
              [i j k])]
    (map (fn [delta] (vec (map + coord delta))) dzyx)))

(defn map-cube
  "Return a map data structure initialised with input"
  [input]
  (let [z 0]
    ; y is row number
    ; x is char in y
    ))

(defn main
  "Day 17 of Advent of Code 2020: Conway Cubes
      lein run day17 filename"
  [[filename]]
  (let [mags-input ["######.#"
                    "#.###.#."
                    "###....."
                    "#.####.."
                    "##.#.###"
                    ".######."
                    "###.####"
                    "######.#"]]
    (println "Mags input:")
    (pp/pprint mags-input)))
