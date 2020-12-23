(ns aoc2020.day23c
  (:require [aoc2020.util :as util]
            [clojure.pprint :as pp]
            [clojure.string :as s]))

(defn parse
  [input]
  {:index 0
   :ring (mapv #(Integer/parseInt %) (s/split input #""))})

;; Part 1

(defn next-index
  [cup ring]
  (let [new-cup (dec cup)]
    (cond
      (= 0 new-cup) (.indexOf ring (apply max ring))
      (util/in? ring new-cup) (.indexOf ring new-cup)
      :else (recur new-cup ring))))

(defn move
  [{:keys [index ring]}]
  (let [cup (nth ring index)
        cups (take 3 (drop (inc index) (cycle ring)))
        mid-ring (filter #(not (util/in? cups %)) ring)
        mid-index (next-index cup mid-ring)
        new-ring (vec (concat (take (inc mid-index) mid-ring)
                         cups
                         (drop (inc mid-index) mid-ring)))
        new-index (inc (.indexOf new-ring cup))]
    {:index (if (= (count new-ring) new-index)
              0
              new-index)
     :ring new-ring}))

(defn main
  "Day 23 of Advent of Code 2020: Crab Cups
      lein run day23c <input>
   where <input> is a number like 12345"
  [[input]]
  (let [state (parse input)]
    (println "Initial:" state)
    (println "Moves" state)
    (pp/pprint (take 101 (iterate move state)))))
