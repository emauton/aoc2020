(ns aoc2020.day8
  (:require [aoc2020.util :as util]
            [clojure.string :as s]))

(defn parse
  [instrs]
  (map #(s/split % #" ") instrs))

(defn execute-inst
  "return (new-acc new-i)"
  [[instr mod] acc i]
  (case instr
    ("nop") [acc (inc i)] 
    ("acc") [(+ acc (Integer/parseInt mod)) (inc i)]
    ("jmp") [acc (+ i (Integer/parseInt mod))]))

(defn execute
  [instrs]
  (loop [v []
         acc 0
         i 0]
         (if (some #(= i %) v) 
           acc
           (let [[new-acc new-i] (execute-inst (nth instrs i) acc i)]
             (recur (conj v i) new-acc new-i)))))

(defn main
  "Day 8 of Advent of Code 2020: Handheld Halting
      lein run day8 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [instrs (util/read-lines filename)]
    (println "acc:" (execute (parse instrs)))))
