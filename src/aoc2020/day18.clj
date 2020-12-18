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
            ;  :open (reduced [acc op])
            ;  :close (reduced [acc op])
              [(op acc n) +]))
          [0 +]
          syms))

(defn compute-paren
  [syms]
  (let [pre (take-while #(not= :close %) syms)
        mid (reverse (take-while #(not= :open %) (reverse pre)))
        pre (drop-last (inc (count mid)) pre)
        post (rest (drop-while #(not= :close %) syms))] 
       ; (println mid)
       ; (println (cons (first (compute-prefix mid)) post))
    (concat pre (cons (first (compute-prefix mid)) post))))
;             (first (compute-prefix (take-while #(not= :open %) 
;                    (reverse (take-while #(not= :close %) syms))))) post))))

(defn all-paren
  [syms]
  (first (drop-while (fn [iter] (some #(= :open %) iter)) 
              (iterate compute-paren syms))))


(defn main
  "Day 18 of Advent of Code 2020: Operation Order
      lein run day18 filename"
  [[filename]]
  (let [input (map parse (util/read-lines filename))] 
;    (println (reduce (fn [acc val] (+ acc (first val))) (map (fn [expr] (compute-paren (all-paren expr))) input)))))
    (println (apply + (flatten (map (fn [expr] (compute-paren (all-paren expr))) input))))))

