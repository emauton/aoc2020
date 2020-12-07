(ns aoc2020.day7
  (:require [aoc2020.util :as util]
            [clojure.core.memoize :as m]
            [clojure.string :as s]
            [clojure.pprint :as pp]))

(defn parse-rule
  "Parse rules of the form
    '1 bright white bag' or '2 muted yellow bags'
   into pairs
    [colour n]"
  [rule]
  (let [[_ n colour] (re-matches #".*(\d+) (.*) bag.*" rule)]
    (if (nil? colour)
      []
      [colour (Integer/parseInt n)])))

(defn parse-line
  [colours line]
  (let [[bag bags] (s/split line #" bags contain ")
        bags (s/split bags #",")
        bags (map parse-rule bags)]
    (reduce (fn [acc [colour _]]
              (if (nil? colour)
                colours
                (assoc acc colour
                       (conj (get acc colour #{}) bag))))
            colours
            bags)))

(defn colour-parse
  "Parse lines of the form
    'light red bags contain 1 bright white bag, 2 muted yellow bags.'
   into a datastructure
    {colour [colour0 colour1 ...]
     ...}
   where we map any particular colour to all colours that can contain it.
  "
  [lines]
  (reduce parse-line {} lines))

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
  (let [repr (colour-parse (util/read-lines filename))]
    (pp/pprint repr)))
