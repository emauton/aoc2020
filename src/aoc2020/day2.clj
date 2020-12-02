(ns aoc2020.day2
  (:require [aoc2020.util :as util] ))

(defn parse-line
  "Parse a line of the form d-d c: string"
  [line]
  (let [[_ a b ch pwd] (re-matches #"(\d+)-(\d+) ([a-z]): (\w+)" line)]
    [(Integer/parseInt a) (Integer/parseInt b) (char (first (seq ch))) pwd]))

(defn valid-sled-rental?
  "Check if password matches policy of the old sled rental place"
  [line]
  (let [[a b ch pwd] (parse-line line)
        n (count (filter #(= % ch) (seq pwd)))]
    (<= a n b)))

(defn valid-tobbogan-rental?
  "Check if password matches policy of the toboggan company"
  [line]
  (let [[a b ch pwd] (parse-line line)]
    (println "that ch is in either position a or b of pwd, but not both")))

(defn count-valid
  "Count the valid passwords in lines"
  [lines]
  (count (filter valid-sled-rental? lines)))

(defn main
  "Day 2 of Advent of Code 2020: Password Philosophy
      lein run day2 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (println (->> (util/read-lines filename)
                (count-valid))))
