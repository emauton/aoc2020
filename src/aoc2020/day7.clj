(ns aoc2020.day7
  (:require [aoc2020.util :as util]
            [clojure.core.memoize :as m]
            [clojure.string :as s]
            [clojure.set :as set]))

(def example 
  {"shiny gold" ["bright white" "muted yellow"]
    "bright white" ["dark orange" "light red"]
    "muted yellow" ["dark orange" "light red"]
    "dark orange" []
    "light red" []})

(defn traverse
  [found query]
  (m/memo
   (let [result (get example query)]
     )))

(defn main
  "Day 7 of Advent of Code 2020: Handy Haversacks 
      lein run day7 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [rules (util/read-lines filename)]
    (println "First rule: " (first rules))
    (println "First example: " (first example))))
