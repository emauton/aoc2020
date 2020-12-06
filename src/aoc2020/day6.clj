(ns aoc2020.day6
  (:require [aoc2020.util :as util]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn get-groups 
  "return a nested collection [group1 group2 ...] where each group is a
   collection [person1 person2 ...] where each person is a
   string of answers. In input groups are separated by blank lines, and persons 
   are separated by newlines"
  [input]
  (map s/split-lines (s/split input #"\n\n")))

(defn count-yes
  "count how many questions were answered 'yes' by group. That is the cardinality of the
   sets of questions answered 'yes' by the members of group. group is a collection of strings"
  [group]
  ;(count (reduce set/union (into #{} group))))
  ;(apply set/union (map (into #{}) group)))
  (count (apply set/union (map #(into #{} %) group))))

(defn main
  "Day 6 of Advent of Code 2020: Custom Customs
      lein run day6 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [groups (get-groups (util/slurp-resource filename))]
    (println "Sum of 'yes's over all groups: " (apply + (map count-yes groups)))))
