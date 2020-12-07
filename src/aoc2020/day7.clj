(ns aoc2020.day7
  (:require [aoc2020.util :as util]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn main
  "Day 7 of Advent of Code 2020: Handy Haversacks 
      lein run day7 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [groups (get-groups (util/slurp-resource filename))]
    (println "Sum of 'yes's over all groups (union): " (apply + (map count-yes-union groups)))
    (println "Sum of 'yes's over all groups (intersection): " (apply + (map count-yes-intersect groups)))))
