(ns aoc2020.day9
  (:require [aoc2020.util :as util]
            [clojure.string :as s]))

(defn valid?
  [val window target]
  (let [x (- target val)] 
    (if (= val x) 
      false 
      (contains? window x))))

(defn first-invalid
  [number-series pream-len]
  (let [window (apply sorted-set (take pream-len number-series))
        target (nth number-series pream-len)]
    (if (every? false? (map #(valid? % window target) window))
      target
      (recur (rest number-series) pream-len))))

(defn encryption-weakness
  [number-series target]
  (let [[total checked-vals] (reduce 
                              (fn [[acc values] n]
                                (let [total (+ acc n)]
                                  (if (>= total target)
                                    (reduced [total (conj values n)])
                                    [total (conj values n)])))
                              [0 []] 
                              number-series)]
    (if (= total target)
      (+ (apply min checked-vals) (apply max checked-vals))
      (recur (rest number-series) target))))
   
(defn main
  "Day 9 of Advent of Code 2020: Encoding Error
      lein run day9 <input> <preamble length>
  where <input> is a filename in project resources/"
  [[filename pream-len]]
  (let [number-series (map #(Long/parseLong %) (util/read-lines filename))
        invalid (first-invalid number-series (Integer/parseInt pream-len))]
    (println "first invalid:" invalid)
    (println "encryption weakness:" (encryption-weakness number-series invalid))))
