(ns aoc2020.day17
  (:require [aoc2020.util :as util] 
            [clojure.pprint :as pp]
            [clojure.string :as s]))

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
