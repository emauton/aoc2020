(ns aoc2020.day14
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]))

(defn parse-mask
  [mask]
  ; reverse because it's most significant figure first
  (let [indices (reverse (range (count mask)))]
    ;(into [] (filter #(not= \X (second %)) (map vector indices (into [] mask))))))
    (map vector indices (into [] mask))))

(defn parse-mem
  [line]
  (if (s/includes? line "mem") 
    (let [[mem val] (s/split (subs line 4) #"] = ")]
      [(Integer/parseInt mem) (Integer/parseInt val)])
    (parse-mask line)))

(defn parse-instr
  "Parse input into a vector of [mask instrs &]"
  [input]
  (let [instr-set (map s/split-lines (s/split input #"mask = "))]
    ; rest because s/split creates an empty vector for the nothing that comes before the
    ; first mask
    (rest (mapv (fn [i-s] (mapv parse-mem i-s)) instr-set))))

(defn apply-mask-to-val
  "Apply the mask (first element of instrs) to each of the values in the pairs that
   make up the rest of the list"
  [instrs]
  (let [mask (first instrs)
        mem-list (rest instrs)]
    (map (fn [[mem val]] 
           [mem (reduce (fn [acc [index bit]] 
                     (case bit
                       \1 (bit-set acc index)
                       \0 (bit-clear acc index)
                       \X acc)) 
                   val mask)]) mem-list)))

(defn execute-instrs-val
  [input]
  (let [masked (map apply-mask-to-val input)]
    (reduce 
     (fn [acc mem-list] 
       (reduce 
        (fn [inner-acc [key val]] (assoc inner-acc key val)) 
        acc mem-list)) 
     {} masked)))

(defn sum-all
  [value-map]
  (reduce-kv (fn [acc _ val] (+ acc val)) 0 value-map))

; Part 2

; mask looks like
;[[35 \0] [34 \0] [33 \X] [32 \0] [31 \0] [30 \1] [29 \0] [28 \0] [27 \1] [26 \1] [25 \0] 
(defn mask-gen
  [mask]
  (reduce (fn [acc [index bit]] ) [] mask))
 
(defn apply-mask-to-mem
  "Apply the mask (first element of instrs) to each of the mems in the pairs that
   make up the rest of the list. '"
  [instrs mem-map]
  (let [mask (first instrs)
        mem-list (rest instrs)
        all-masks (mask-gen mask)]
    (map (fn [[mem val]] 
           [mem (reduce (fn [acc [index bit]] 
                     (case bit
                       \1 (bit-set acc index)
                       \0 (bit-clear acc index)
                       \X acc)) 
                   val mask)]) mem-list)))

(defn execute-instrs-mem
  [input]
  (let [masked (map apply-mask-to-mem input)]
    (reduce 
     (fn [acc mem-list] 
       (reduce 
        (fn [inner-acc [key val]] (assoc inner-acc key val)) 
        acc mem-list)) 
     {} masked)))

(defn main
  "Day 14 of Advent of Code 2020: Docking Data
      lein run day14 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [input (parse-instr (util/slurp-resource filename))]
    (println "Part 1:" (sum-all (execute-instrs-val input)))
    (println "Part 2:" (sum-all (execute-instrs-mem input)))))