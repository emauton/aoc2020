(ns aoc2020.day1
  (:require [aoc2020.util :as util] 
            [clojure.math.combinatorics :as c]))

(defn indices-adding-to 
  "Find the indices of n elements of nums that sum to target"
  [nums n target]
  (let [combs (c/combinations (range (count nums)) n)
        test-sum (fn [indices] (= (apply + (map #(nth nums %) indices)) target))]
    (first (filter test-sum combs))))

(defn repair-expense-report
  "Return the product of two elements of nums that sum to 2020"
  [n nums]
  (let [indices (indices-adding-to nums n 2020)]
    (apply * (map #(nth nums %) indices))))

(defn main
  "Day 1 of Advent of Code 2020: Report Repair
      lein run day1 <input>
   where <input> is a filename in project resources/"
  [[filename n]]
  (println (->> (util/read-numbers filename)
                (repair-expense-report (Integer/parseInt n)))))

