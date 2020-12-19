(ns aoc2020.day19c
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]
            [instaparse.core :as insta]))

(defn main
  "Day 19 of Advent of Code 2020: Monster Messages 
      lein run day19 filename
   This is just an application of a parsing library. :sweat_smile:
   Only change is to move the 0 rule up to the top of the input."
  [[filename]]
  (let [[rules msgs] (s/split (util/slurp-resource filename) #"\n\n")
        parser (insta/parser rules)
        parses (map parser (s/split-lines msgs))]
    (println (count (filter (complement insta/failure?) parses)))))
