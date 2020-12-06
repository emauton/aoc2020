(ns aoc2020.day6
  (:require [aoc2020.util :as util]
            [clojure.string :as s]))

(defn get-groups 
  "return a nested collection [group1 group2 ...] where each group is a
   collection [person1 person2 ...] where each person is a
   string of answers. In input groups are separated by blank lines, and persons 
   are separated by newlines"
  [input]
  (map s/split-lines (s/split input #"\n\n")))

(defn main
  "Day 6 of Advent of Code 2020: Custom Customs
      lein run day6 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [groups (get-groups (util/slurp-resource filename))]
  (println "First group:" (first groups))))
