(ns aoc2020.day23c
  (:require [aoc2020.util :as util]
            [clojure.pprint :as pp]
            [clojure.string :as s]))

(defn parse
  "Parse a list of form '[1 6 7 2 4 8 3 5 9]' into a 'ring' map
   s.t. 1 -> 6, 6 -> 2 and so on, with 9 -> 1 looping back to form
   the ring. Also track our current place in the ring."
  [as-list]
  (let [
        [_ ring] (reduce (fn [[curr acc] n]
                           [n (assoc acc curr n)])
                         [(first as-list) (sorted-map)]
                         as-list)]
    {:curr (first as-list)
     :ring (assoc ring (last as-list) (first as-list))}))

(defn destination
  "Given the current game state and a seq of numbers to exclude,
   return a destination which is (dec curr) respecting the ring and
   the exclude list."
  [{:keys [curr ring] :as state} exclude]
  (let [dest (dec curr)]
    (cond
      (= 0 dest) (recur (assoc state :curr (inc (count ring))) exclude)
      (util/in? exclude dest) (recur (assoc state :curr dest) exclude)
      :else dest)))

(defn take-3
  "Get the 3 values 'clockwise' of curr in the game state"
  [{:keys [curr ring]}]
  (take 3 (drop 1 (iterate ring curr))))

(defn move
  [{:keys [curr ring] :as state}]
  (let [three (take-3 state)
        dest (destination state three)
        new-ring (merge ring {curr (ring (last three))
                              (last three) (ring dest)
                              dest (first three)})]
    {:curr (new-ring curr)
     :ring new-ring}))

(defn vec-state
  [ring start acc]
  (let [n (ring start)]
    (if (= 1 n)
      acc
      (recur ring n (conj acc n)))))

(defn vec-state-2
  [ring]
  [(ring 1) (ring (ring 1))])

(defn pad
  [as-list n]
  (vec (concat as-list (range (inc (count as-list)) (inc n)))))

(defn main
  "Day 23 of Advent of Code 2020: Crab Cups
      lein run day23c <input>
   where <input> is a number like 12345"
  [[input]]
   (let [as-list (mapv #(Integer/parseInt %) (s/split input #""))
         one-state (parse as-list)
         one-result (first (drop 100 (iterate move one-state)))
         one-vec (vec-state (:ring one-result) 1 [])
         two-state (parse (pad as-list 1000000))
         two-result (first (drop 10000000 (iterate move two-state)))
         two-vec (vec-state-2 (:ring two-result))]
     (println "Part 1:" (s/join "" one-vec))
     (println "Part 2:" (apply * two-vec))))
