(ns aoc2020.day10
  (:require [aoc2020.util :as util]))

(defn get-diffs
  "Return the count of differences between sorted adapter joltages"
  [joltages]
  (reduce (fn [[ones twos threes prev] j]
            (case (- j prev)
              1 [(inc ones) twos threes j]
              2 [ones (inc twos) threes j]
              3 [ones twos (inc threes) j]
              [ones twos threes j]))
          [0 0 1 0] joltages))

(defn get-jumps
  "Return a sequence of differences between sorted adapter joltages"
  [joltages]
  (first (reduce (fn [[diffs prev] j]
                   [(conj diffs (- j prev)) j])
                 [[] 0]
                 joltages)))

(defn count-1-runs
  "Count runs of 1s in the differences between sorted adapter joltages"
  [joltages]
  (map count (filter #(= 1 (first %))
                     (partition-by identity (get-jumps joltages)))))

(defn get-multiplier
  "Takes an argument n which is the number of items in a group.
   Returns the number of possible combinations of items in groups of no more than 2.
   That is 2^n minus the 'barred' combinations with groups that are too long"
  [n]
  (- (int (Math/pow 2 n)) (apply + (range 1 (- n 1)))))

(defn part-2
  "How many arrangments of adapters are possible?"
  [joltages]
  (reduce (fn [acc count] (* acc (get-multiplier (- count 1)))) 1 (count-1-runs joltages)))

(defn main
  "Day 10 of Advent of Code 2020:
      lein run day10 <input> <preamble length>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [joltages (sort (map #(Integer/parseInt %) (util/read-lines filename)))
        [ones _ threes] (get-diffs joltages)]
    (println "Part 1, 1s x 3s:" (* ones threes))
    (println "Part 2, possible arrangements:" (part-2 joltages))))
