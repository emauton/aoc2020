(ns aoc2020.day14
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]))

(defn parse-mem
  [line]
  (if (not= \m (first line))
    line
    (s/split (subs line 4) #"] = ")))

(parse-mem "mem[1292] = 29")

(defn parse-instr
  "Parse input into a vector of [mask instrs &]"
  [input]
  (let [instr-set (map s/split-lines (s/split input #"mask = "))]
    (mapv (fn [i-s] (mapv parse-mem i-s)) instr-set)))
;  (map (fn [inst-set] (conj (map parse-mem (drop inst-set)) (first inst-set))) 

(parse-instr "mask = 000001001011XX1XX100X0001011X0001101
mem[54977] = 194579
mem[29691] = 71157948
mem[27205] = 122030256
mem[43160] = 91267
mem[45028] = 254793847
mem[27137] = 1696
mask = 1000011X10X10X1X1100101X00X011010001
mem[20727] = 25071621
mem[37927] = 626522009
mem[4815] = 119068316")

(defn main
  "Day 14 of Advent of Code 2020: Docking Data
      lein run day14 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [input (parse-instr (util/slurp-resource filename))]
    (println (nth input 3))))
