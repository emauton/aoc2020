(ns aoc2020.day1
  (:require [aoc2020.util :as util] 
            [clojure.math.combinatorics :as c]))

(defn indices-adding-to 
  "Find the indices of two elements of nums that sum to target"
  [nums target]
  (let [combs (c/combinations (range (count nums)) 2)
        test-sum (fn [[a b]] (= (+ (nth nums a) (nth nums b)) target))]
    (first (filter test-sum combs))))

(defn repair-expense-report
  "Return the product of two elements of nums that sum to 2020"
  [nums]
  (let [[a b] (indices-adding-to nums 2020)]
    (* (nth nums a) (nth nums b))))

(defn main
  "Day 1 of Advent of Code 2020: Report Repair"
  [args]
  (println (->> (util/read-numbers "mags/input-1-1")
                (repair-expense-report))) )

