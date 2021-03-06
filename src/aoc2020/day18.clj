(ns aoc2020.day18
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]))

(defn parse
  [input]
  (filter util/not-nil?
          (map (fn [c]
                 (case c
                   \  nil
                   \* :star
                   \+ :plus
                   \( :open
                   \) :close
                   (Integer/parseInt (str c))))
               input)))

(defn compute-prefix
  [syms]
  (reduce (fn [[acc op] n]
            (case n
              :star [acc *]
              :plus [acc +]
              [(op acc n) +]))
          [0 +]
          syms))

(defn compute-prefix-sum
  [syms]
  (if (not (some #(= :plus %) syms))
    syms
    (let [parts (partition-by #(= :star %) syms)]
      (reduce (fn [acc p]
                (concat acc 
                        (if (some #(= :plus %) p)
                          [(apply + (filter #(not= :plus %) p))]
                          p))) 
              () parts))))

(defn compute-prefix-sum-first
  [syms]
  (let [sum-computed (compute-prefix-sum syms)]
    [(apply * (filter #(not= :star %) sum-computed)) *]))

(defn compute-paren
  [compute-fn syms]
  (let [pre (take-while #(not= :close %) syms)
        mid (reverse (take-while #(not= :open %) (reverse pre)))
        pre (drop-last (inc (count mid)) pre)
        post (rest (drop-while #(not= :close %) syms))] 
    (concat pre (cons (first (compute-fn mid)) post))))

(defn all-paren
  [compute-fn syms]
  (let [compute-paren-partial (partial compute-paren compute-fn)]
  (first (drop-while (fn [iter] (some #(= :open %) iter)) 
              (iterate compute-paren-partial syms)))))

(defn do-maths
  [input part]
  (let [compute-fn (if (= part 1)
                     compute-prefix
                     compute-prefix-sum-first)]
    (apply + (flatten (map (fn [expr]
       (compute-paren compute-fn (all-paren compute-fn expr))) input )))))

(defn main
  "Day 18 of Advent of Code 2020: Operation Order
      lein run day18 filename"
  [[filename]]
  (let [input (map parse (util/read-lines filename))] 
    (println "Part 1:" (do-maths input 1))
    (println "Part 2:" (do-maths input 2))))
