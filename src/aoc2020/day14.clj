(ns aoc2020.day14
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]))

(defn parse-mask
  [mask]
  ; reverse because it's most significant figure first
  (let [indices (reverse (range (count mask)))]
    (into [] (filter #(not= \X (second %)) (map vector indices (into [] mask))))))

(defn parse-mem
  [line]
  (if (s/includes? line "mem") 
    (let [[mem val] (s/split (subs line 4) #"] = ")]
      [mem (Integer/parseInt val)])
    (parse-mask line)))

(defn parse-instr
  "Parse input into a vector of [mask instrs &]"
  [input]
  (let [instr-set (map s/split-lines (s/split input #"mask = "))]
    ; rest because s/split creates an empty vector for the nothing that comes before the
    ; first mask
    (rest (mapv (fn [i-s] (mapv parse-mem i-s)) instr-set))))

(defn apply-mask
  "Apply the mask (first element of instrs) to each of the values in the pairs that
   make up the rest of the list"
  [instrs]
  (let [mask (first instrs)
        mem-list (rest instrs)]
    (map (fn [[mem val]] 
           [mem (reduce (fn [acc [index bit]] 
                     (if (= bit \1) 
                       (bit-set acc index)
                       (bit-clear acc index))) 
                   val mask)]) mem-list)))

(defn execute-instrs
  [input]
  (let [masked (map apply-mask input)]
    (reduce 
     (fn [acc mem-list] 
       (reduce 
        (fn [inner-acc [key val]] (assoc inner-acc key val)) 
        acc mem-list)) 
     {} masked)))

(defn sum-all
  [value-map]
  (reduce-kv (fn [acc _ val] (+ acc val)) 0 value-map))

(defn main
  "Day 14 of Advent of Code 2020: Docking Data
      lein run day14 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [input (parse-instr (util/slurp-resource filename))]
    (println "sum:" (sum-all (execute-instrs input)))))