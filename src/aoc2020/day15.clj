(ns aoc2020.day15
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]))

(defn update-turns
  [[old] n]
  (if (nil? old)
    [n]
    [n old]))

(defn init
  [starting]
  (reduce (fn [[turn last-n turns] n]
            [(inc turn)
             n
             (update turns n update-turns turn)])
          [0 0 {}]
          starting))

(defn take-turns
  [[turn last-n turns] t total]
  (let [instances (get turns last-n)
        new-n (apply - (take 2 instances))]
    (cond
      (= t total) last-n
      (= 1 (count instances)) (recur [t 0 (update turns 0 update-turns t)] (inc t) total)
      :else (recur [t new-n (update turns new-n update-turns t)] (inc t) total))))

(defn all-turns
  [acc start-turn total-turns]
  (take-turns acc start-turn total-turns))

(defn main
  "Day 15 of Advent of Code 2020: Rambunctious Recitation
      lein run day15"
  [_]
  (let [example-input [0 3 6]
        cian-input [15 5 1 4 7 0]
        mags-input [12 20 0 6 1 17 7]]
    (println "Example part 1:" (all-turns (init example-input) (count example-input) 2020))
;    (println "Cian part 1:"    (all-turns (init cian-input)    (count cian-input)    2020))
;    (println "Cian part 2:"    (all-turns (init cian-input)    (count cian-input)    30000000))
    (println "Mags part 1:"    (all-turns (init mags-input)    (count mags-input)    2020))
    (println "Mags part 2:"    (all-turns (init mags-input)    (count mags-input)    30000000))
))
