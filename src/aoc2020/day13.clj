(ns aoc2020.day13
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]
            [clojure.math.numeric-tower :as math]))

(defn parse-buses
  [text]
  (map #(Integer/parseInt %) (filter #(not= "x" %) (s/split text #","))))

(defn parse-schedule-gcd
  [text]
  (let [buses (s/split text #",")
        indices (range (count buses))
        pairs (map 
               (fn [[bus index]] [(Integer/parseInt bus) index]) 
               (filter #(not= "x" (first %)) (map vector buses indices)))
        new-pairs (map (fn [[b i]] [b (cond 
                        (= i 0) i
                        (< i b) (- b i)
                        :else (- b (mod i b)))]) pairs)]
    [(map #(first %) new-pairs) (map #(second %) new-pairs)]))

(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. 
   https://rosettacode.org/wiki/Chinese_remainder_theorem#Clojure"
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))
 
(defn chinese_remainder
  " Main routine to return the chinese remainder 
   https://rosettacode.org/wiki/Chinese_remainder_theorem#Clojure"
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)           ; p = prod / n_i
                        egcd (extended-gcd p n_i)   ; Extended gcd
                        inv_p (second egcd)]        ; Second item is the inverse
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))] ; Replaces the Python for loop to sum
                                                      ; (map vector n a) is same as
        ;                                             ; Python's version Zip (n, a)
    (mod sum-prod prod)))                             ; Result line
 
(defn get-bus
  [t buses]
  (filter (fn [b] (not= nil b)) (map #(if (= 0 (rem t %)) [t %] nil) buses)))

(defn next-bus
  [earliest buses]
  (let [times (drop earliest (range))]
    (get-bus (first (drop-while (fn [t] (empty? (get-bus t buses))) times)) buses)))

(defn main
  "Day 13 of Advent of Code 2020: Shuttle Search
      lein run day13 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [input (util/read-lines filename)
        earliest (Integer/parseInt (first input))
        buses (parse-buses (second input))
        [t b] (first (next-bus earliest buses))
        p (parse-schedule-gcd (second input)) ]
    (println "earliest time:" earliest)
    (println "buses:" buses)
    (println "next bus:" (next-bus earliest buses))
    (println "product:" (* b (- t earliest)))
    (println "chinese remainer:" (chinese_remainder (first p) (second p)))))
