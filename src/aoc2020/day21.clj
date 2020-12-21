(ns aoc2020.day21
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.set :as setlib]))

(defn parse-ingr
  [line]
  (let [[ingrs allergens] (s/split line #"\(contains ")
        ingrs (set (s/split ingrs #" "))
        allergens (set (s/split (s/replace allergens ")" "") #", "))]
  [ingrs allergens]))

(defn map-ingr-allergen
  [list-pairs]
  [(reduce setlib/union (map first list-pairs))
   (reduce setlib/union (map second list-pairs))])

(defn contains-list
  [all-allergens data]
  (map (fn [a] [a (reduce (fn [acc [ingrs allergens]] 
                            (if (some #(= % a) allergens)
                              (conj acc ingrs)
                              acc)) 
                          [] data)]) all-allergens))

(defn reduced-contains-list
  [list]
  (map (fn [[allergen ingr-list]] [allergen (reduce setlib/intersection ingr-list)]) list))

(defn unassigned-ingrs
  [allergen-set ingrs]
  (let [assigned (reduce setlib/union (map second allergen-set))]
    (setlib/difference ingrs assigned)))

(defn count-appearance
  [ingr data]
  (reduce (fn [acc line] (if (some #(= % ingr) (first line)) 
                           (inc acc) 
                           acc)) 
          0 data))

(defn main
  "Day 21 of Advent of Code 2020: Allergen Assessment
      lein run day21 filename
   This is just an application of a parsing library. :sweat_smile:
   Only change is to move the 0 rule up to the top of the input."
  [[filename]]
  (let [input (util/read-lines filename)
        data (map parse-ingr input)
        [all-ingrs all-allergens] (map-ingr-allergen data) 
        contains-sets (contains-list all-allergens data)
        assigned (reduced-contains-list contains-sets)
        unassigned (unassigned-ingrs assigned all-ingrs)]
    (println "First data:" (first data))
    ;(println "All ingredients:" all-ingrs)
    (println "All allergens: " all-allergens)
    (println "assigned:" assigned)
    (println "Unassigned:" unassigned)
    (println "Count:" (reduce + (map #(count-appearance % data) unassigned)))))