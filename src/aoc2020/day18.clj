(ns aoc2020.day18
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]))


(def test-str "4 * 9 * 3 + 2 + (8 + 5 * 8 * 5 * (4 * 8 * 8 * 8) + (5 + 6)) * 2")

(defn expr
  [symbols]
  (let [[cur op & remainder] symbols
        op-fn {\+ + \* *}]
    (cond
      (nil? nil) cur
      (= op \)) cur
      (= cur \() (recur (rest symbols))
      :else ((op-fn op) cur (recur remainder)))))

(defn main
  "Day 18 of Advent of Code 2020: Operation Order
      lein run day18 filename"
  [[filename]]
  (let []
    (println "Part 1, sum:")))