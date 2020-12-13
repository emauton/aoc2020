(ns aoc2020.day13
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]))

(defn parse-buses
  [text]
  (map #(Integer/parseInt %) (filter #(not= "x" %) (s/split text #","))))

(defn parse-schedule
  [text]
  (let [buses (s/split text #",")
        indices (range (count buses))
        pairs (sort (comp - compare) 
                    (map (fn [[bus index]] [(Integer/parseInt bus) index]) 
                         (filter #(not= "x" (first %)) (map vector buses indices))))
        diff (last (first pairs))]
    (map (fn [[bus modifier]] [bus (- modifier diff)]) pairs)))
           
(defn schedule-match?
  [time schedule]
  (every? (fn [[b i]] (= 0 (rem (+ time i) b))) schedule))

(defn first-multiple
  [big small]
  (let [additive (range)]
    (+ big (first (drop-while #(not= 0 (rem (+ big %) small)) additive)))))

(defn get-timestamp
  [schedule]
  (let [earliest 100000
  ;(let [earliest 100000000000000
        big-bus (first (first schedule))
        start (first-multiple earliest big-bus)
        times (range start (java.lang.Long/MAX_VALUE) big-bus)]
    (first (drop-while #(not (schedule-match? % schedule)) times))))

;(first-multiple 100000000000000 19)
(get-timestamp (parse-schedule "7,13,x,x,59,x,31,19"))
;(parse-schedule "19,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,523,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,13,x,x,x,x,x,x,x,x,x,x,29,x,853,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,23")



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
    (println "product:" (* b (- t earliest)))
    (println "Schedule match:" (get-timestamp (parse-schedule (second input))))))
