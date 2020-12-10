(ns aoc2020.day10
  (:require [aoc2020.util :as util])

(defn get-diffs
  "return a pair [ones threes] of the count of differences of 1 and the 
   count of differences of 3 between the sorted list of adapter joltages"
  [joltages]
  (reduce (fn [[ones twos threes prev] j] 
            (case (- j prev)
              1 [(inc ones) twos threes j]
              2 [ones (inc twos) threes j]
              3 [ones twos (inc threes) j]
              [ones twos threes j]))
          [0 0 1 0] joltages))

(defn get-jumps
  [joltages]
  (first (reduce (fn [[diffs prev] j] 
                   [(conj diffs (- j prev)) j])
                 [[] 0]
                 joltages)))

(defn count-runs
  [joltages]
  (map count (filter #(= 1 (first %)) 
                     (partition-by identity (get-jumps joltages)))))

(defn part-2
  "janky answer to part 2"
  [counts] 
  (reduce (fn [acc count] 
            (case count
              2 (* acc 2)
              3 (* acc 4)
              4 (* acc 7)
              acc))
          1 counts))
   
(defn main
  "Day 10 of Advent of Code 2020: 
      lein run day10 <input> <preamble length>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [joltages (sort (map #(Integer/parseInt %) (util/read-lines filename)))
        [ones twos threes] (get-diffs joltages)
        counts (count-runs joltages)]
    (println "Part 1, 1s x 3s:" (* ones threes))
    (println "runs:" counts)
    (println "answer:" (part-2 counts))))
