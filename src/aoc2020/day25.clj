(ns aoc2020.day25
  (:require [aoc2020.util :as util]))

(defn gen-loop
  [subject]
  (iterate (fn [[value i]]
             (let [product (* value subject)]
               [(rem product 20201227) (inc i)]))
           [1 0]))

(defn get-loop-size
  [public-key]
  (let [loop-seq (gen-loop 7)
        [_ size] (first (drop-while #(not= public-key (first %)) loop-seq))]
    size))

(defn get-encryption-key
  [public-key loop-size]
  (let [loop-seq (gen-loop public-key)
        [encryption-key _] (nth loop-seq loop-size)]
    encryption-key))

(defn main
  "Day 25 of Advent of Code 2020: Combo Breaker
      lein run day25 <filename>"
  [[filename]]
   (let [[card-key door-key] (map #(Integer/parseInt %) (util/read-lines filename))
         card-loop (get-loop-size card-key)
         encryption-key (get-encryption-key door-key card-loop)]
     (println "Card loop" card-loop)
     (println "Encryption key" encryption-key)))
