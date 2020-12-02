(ns aoc2020.day2
  (:require [aoc2020.util :as util] ))

(defn parse-line
  "Parse a line of the form d-d c: string"
  [line]
  (let [[_ min max ch pwd] (re-matches #"(\d+)-(\d+) ([a-z]): (\w+)" line)]
    [(Integer/parseInt min) (Integer/parseInt max) (char (first (seq ch))) pwd]))

(defn valid? 
  "Check if password matches policy"
  [line]
  (let [[min max ch pwd] (parse-line line)
        num (count (filter #(= % ch) (seq pwd)))]
    (<= min num max)))

(defn count-valid
  "Count the valid passwords in lines"
  [lines]
  (count (filter valid? lines)))

(defn main
  "Day 2 of Advent of Code 2020: Password Philosophy
      lein run day2 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (println (->> (util/read-lines filename)
                (count-valid))))