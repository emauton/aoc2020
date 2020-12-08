(ns aoc2020.core
  (:require aoc2020.day1 
            aoc2020.day2
            aoc2020.day3
            aoc2020.day4
            aoc2020.day6
            aoc2020.day7
            aoc2020.day8
            clojure.string)
  (:gen-class))

(defn -main
  "Dispatch to the different day routines for Advent of Code 2020"
  [day & args]
  (let [day-ns (symbol (clojure.string/join "." ["aoc2020" day]))
        day-main (ns-resolve day-ns 'main)]
    (day-main args)))
