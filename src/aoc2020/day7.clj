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

(defn parents-parse
  "Parse lines of the form
    'light red bags contain 1 bright white bag, 2 muted yellow bags.'
   into a datastructure
    {colour [colour0 colour1 ...]
     ...}
   where we map any particular colour to all colours that can contain it."
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

(defn numbers-parse
  "Parse lines of the form
    'light red bags contain 1 bright white bag, 2 muted yellow bags.'
   into a datastructure
    {colour {colour0 number0 colour1 number1 ...} ...}
   where we map any particular colour to the number of each colour it must contain."
  [colours line]
  (let [[bag bags] (s/split line #" bags contain ")
        bags (s/split bags #",")
        bags (map parse-rule bags)]
    (reduce (fn [acc [colour n]]
              (if (nil? colour)
                colours
                (assoc acc bag
                       (assoc (get acc bag {}) colour n))))
            colours
            bags)))

(defn parse
  "Apply a line parser over input lines, reducing into a map"
  [parser lines]
  (reduce parser {} lines))

(defn parents-traverse
  [parents-map target acc]
  (let [parents (get parents-map target)]
    (if (empty? parents) acc
      (reduce (fn [internal-acc parent]
                (if (util/in? internal-acc parent)
                  internal-acc
                  (parents-traverse parents-map parent (conj internal-acc parent))))
              acc parents))))

(defn numbers-traverse
  [numbers-map target]
  (let [contained (get numbers-map target)]
    (if (nil? contained) 0
      (reduce-kv (fn [acc colour n]
                   (+ acc
                      (->> (numbers-traverse numbers-map colour)
                           (+ 1)
                           (* n))))
              0 contained))))

(defn main
  "Day 7 of Advent of Code 2020: Handy Haversacks
      lein run day7 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [parents-map (parse parents-parse (util/read-lines filename))
        numbers-map (parse numbers-parse (util/read-lines filename))]
    (println (count (parents-traverse parents-map "shiny gold" #{})))
    (println (numbers-traverse numbers-map "shiny gold"))))