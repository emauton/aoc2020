(ns aoc2020.day12
  (:require [aoc2020.util :as util]))

(defn parse-instrs
  "Parse the input into a vector of vector pairs"
  [input]
  (mapv (fn [ins] [(first ins) (Integer/parseInt (subs ins 1))]) input))

(defn forward
  [[north east] val dir]
  (case dir
    \N [(+ north val) east]
    \S [(- north val) east]
    \E [north (+ east val)]
    \W [north (- east val)]))

(defn left
  [val dir]
  (case val
    90 ))
  
(defn main
  "Day 12 of Advent of Code 2020: 
      lein run day11 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [instrs (parse-instrs (util/read-lines filename))]
    (println (first instrs))))
