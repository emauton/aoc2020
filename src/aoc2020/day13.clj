(ns aoc2020.day13
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]))

(defn parse-buses
  [text]
  (map #(Integer/parseInt %) (filter #(not= "x" %) (s/split text #","))))

(defn parse-schedule
  [text]
  (map #(Integer/parseInt %) (filter #(not= "x" %) (s/split text #","))))
  )

(defn get-bus
  [t buses]
  (filter (fn [b] (not= nil b)) (map #(if (= 0 (rem t %)) [t %] nil) buses)))

(defn next-bus
  [earliest buses]
  (let [times (drop earliest (range))]
    (get-bus (first (drop-while (fn [t] (empty? (get-bus t buses))) times)) buses)))

(next-bus 939 (parse-buses "7,13,x,x,59,x,31,19"))
(defn main
  "Day 13 of Advent of Code 2020: Shuttle Search
      lein run day13 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [input (util/read-lines filename)
        earliest (Integer/parseInt (first input))
        buses (parse-buses (second input))
        [t b] (first (next-bus earliest buses))]
    (println "earliest time:" earliest)
    (println "buses:" buses)
    (println "next bus:" (next-bus earliest buses))
    (println "product:" (* b (- t earliest)))))
