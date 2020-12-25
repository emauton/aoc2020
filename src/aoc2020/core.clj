(ns aoc2020.core
  (:require aoc2020.day1 
            aoc2020.day2
            aoc2020.day3
            aoc2020.day4
            aoc2020.day6
            aoc2020.day7
            aoc2020.day8
            aoc2020.day8c
            aoc2020.day9
            aoc2020.day10
            aoc2020.day11
            aoc2020.day12
            aoc2020.day13
            aoc2020.day14
            aoc2020.day14c
            aoc2020.day15
            aoc2020.day16
            aoc2020.day17
            aoc2020.day18
            aoc2020.day19
            aoc2020.day19c
            ;aoc2020.day20
            aoc2020.day21
            aoc2020.day22
            aoc2020.day23mags
            aoc2020.day23c
            aoc2020.day24
            aoc2020.day25
            clojure.string)
  (:gen-class))

(defn -main
  "Dispatch to the different day routines for Advent of Code 2020"
  [day & args]
  (let [day-ns (symbol (clojure.string/join "." ["aoc2020" day]))
        day-main (ns-resolve day-ns 'main)]
    (day-main args)))
