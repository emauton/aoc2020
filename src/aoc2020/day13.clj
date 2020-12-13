(ns aoc2020.day13
  (:require [aoc2020.util :as util]
            [clojure.string :as s]
            [clojure.math.numeric-tower :as math]))

;; Part 1

(defn parse-buses
  [text]
  (map #(Integer/parseInt %)
       (filter #(not= "x" %) (s/split text #","))))

(defn get-bus
  [t buses]
  (filter (fn [b] (not= nil b))
          (map #(if (= 0 (rem t %)) [t %] nil) buses)))

(defn next-bus
  [earliest buses]
  (let [times (drop earliest (range))]
    (get-bus (first (drop-while (fn [t]
                                  (empty? (get-bus t buses)))
                                times))
             buses)))

;; Part 2

;; CRT impl from https://rosettacode.org/wiki/Chinese_remainder_theorem#Clojure

(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs."
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

(defn chinese-remainder
  "Main routine to return the chinese remainder"
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

(defn parse-schedule-crt
  "Represent the bus schedule as the two pairs expected by CRT impl above:
   a list of primes (buses) + the timestamp we're looking for mod that prime.
   i.e. a system of congruences per
   https://brilliant.org/wiki/chinese-remainder-theorem/"
  [text]
  (let [buses (s/split text #",")
        indices (range (count buses))
        pairs (map
               (fn [[bus index]] [(Integer/parseInt bus) index])
               (filter #(not= "x" (first %)) (map vector buses indices)))
        congruences (map (fn [[b i]] [b (mod (- i) b)]) pairs)]
    (apply map vector congruences))) ; "unzip" into a pair of lists
                                     ; rather than a list of pairs

(defn main
  "Day 13 of Advent of Code 2020: Shuttle Search
      lein run day13 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [input (util/read-lines filename)
        earliest (Integer/parseInt (first input))
        buses (parse-buses (second input))
        [timestamp bus] (first (next-bus earliest buses))
        [primes congruences] (parse-schedule-crt (second input))]
    (println "bus number * schedule difference:"
             (* bus (- timestamp earliest)))
    (println "first timestamp for ordered bus departures:"
             (chinese-remainder primes congruences))))
