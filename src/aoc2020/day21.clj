(ns aoc2020.day21
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.set :as setlib]))

(defn parse-ingredients-line
  "Parse a single ingredients line into a pair of lists"
  [line]
  (let [[ingrs allergens] (s/split line #"\(contains ")
        ingrs (set (s/split ingrs #" "))
        allergens (set (s/split (s/replace allergens ")" "") #", "))]
  [ingrs allergens]))

(defn all-ingr-allergen
  "Return a pair of sets: all ingredients, all allergens"
  [list-pairs]
  [(reduce setlib/union (map first list-pairs))
   (reduce setlib/union (map second list-pairs))])

(defn get-allergen-lists
  "Return a list of lists where each is a pair: allergen, list of ingredients lists
   associated with that allergen"
  [all-allergens data]
  (map (fn [a] [a (reduce (fn [acc [ingrs allergens]] 
                            (if (some #(= % a) allergens)
                              (conj acc ingrs)
                              acc)) 
                          [] data)]) all-allergens))

(defn min-allergen-lists
  "Return a list of pairs: allergen, set of possible ingredients 
   (where possible ingredients are determined by the 
   intersection of lists from get-allergen-lists)"
  [list]
  (map (fn [[allergen ingr-list]] [allergen (reduce setlib/intersection ingr-list)]) list))

(defn unassigned-ingrs
  "Return the set of ingredients not included in allergen-set"
  [allergen-set ingrs]
  (let [assigned (reduce setlib/union (map second allergen-set))]
    (setlib/difference ingrs assigned)))

(defn count-appearance
  "Return the number of times an ingredient appears in the data set"
  [ingr data]
  (reduce (fn [acc line] (if (some #(= % ingr) (first line)) 
                           (inc acc) 
                           acc)) 
          0 data))

(defn all-empty?
  "If all of the lists of possible ingredients are empty return true. 
   This function is used by danger-list after remove-single"
  [possibility-list]
  (= 0 (count (filter #(not= 0 (count (second %))) possibility-list))))

(defn remove-single
  "Find the ingredients list with only one element, remove that ingredient
   from all the lists, and add the relationship (allergen: ingredient) to the
   list of singles"
  [possibility-list singles-map]
  (let [single (first (filter #(= 1 (count (second %))) possibility-list))
        single-ingr (first (second single))]
    [(map (fn [[allergen ingr-list]] 
            [allergen (setlib/difference ingr-list #{single-ingr})]) 
          possibility-list) (assoc singles-map (first single) (second single))]))

(defn danger-list
  "Return the list of pairs: allergen, ingredient. "
  [possibility-list singles-map]
  (if (all-empty? possibility-list)
   singles-map
   (let [[poss-list sing-map] (remove-single possibility-list singles-map)] 
     (danger-list poss-list sing-map))))

(defn main
  "Day 21 of Advent of Code 2020: Allergen Assessment
      lein run day21 filename
   This is just an application of a parsing library. :sweat_smile:
   Only change is to move the 0 rule up to the top of the input."
  [[filename]]
  (let [input (util/read-lines filename)
        data (map parse-ingredients-line input)
        [all-ingrs all-allergens] (all-ingr-allergen data) 
        allergen-lists (get-allergen-lists all-allergens data)
        assigned (min-allergen-lists allergen-lists)
        unassigned (unassigned-ingrs assigned all-ingrs)]
    (println "Count:" (reduce + (map #(count-appearance % data) unassigned)))
    (println "Danger list: " 
             (s/join "," (map #(str (first (second %))) (sort (danger-list assigned {})))))))