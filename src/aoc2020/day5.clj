(ns aoc2020.day5
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]))

(defn get-row
  "Takes a string representation of a binary number in F (0) B (1) and returns an int"
  [row-str] 
  (Integer/parseInt (apply str (map {\F \0 \B \1} row-str)) 2))

(defn get-col
  "Takes a string representation of a binary number in F (0) B (1) and returns an int"
  [col-str] 
  (Integer/parseInt (apply str (map {\R \1 \L \0} col-str)) 2))

(defn parse-pass
  "Create a boarding pass [row column id] from the input line 
   F is front half, B is back half R is right, L is left 
   eg FBFBBFFRLR gives row 44, column 5"
  [line]
  (let [row (get-row (subs line 0 7))
        col (get-col (subs line 7 10))
        id (+ (* row 8) col)]
    [row col id]))
    
(defn highest-id
  "Return the highest ID from the list of boarding passes [row column ID]"
  [passes]
  (last (last (sort-by last passes))))

(defn main
  "Day 5 of Advent of Code 2020: Binary Boarding  
      lein run day5 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [passes (map parse-pass (util/read-lines filename))]
  (println "Sanity check, highest ID: " (highest-id passes))
  (println "Test parse-pass: " (parse-pass "FBFBBFFRLR"))
  (println "Test highest-id:" (highest-id [[1 2 2] [2 3 4] [5 3 2]]))))
