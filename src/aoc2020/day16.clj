(ns aoc2020.day16
  (:require [aoc2020.util :as util] 
            [clojure.pprint :as pp]
            [clojure.string :as s]))

(defn parse-ticket
  [ticket]
  (mapv #(Integer/parseInt %) (s/split ticket #",")))

(defn parse-rule
  "departure location: 34-724 or 735-974
   -> 
   {:field \"departure location\"
    :rules [[34 724] [735 974]]}"
  [rule]
  (let [[_ field & args] (re-matches #"(.*): (\d+)-(\d+) or (\d+)-(\d+)" rule)
        [r0 r1 r2 r3] (map #(Integer/parseInt %) args) ]
    {:field field
     :ranges [[r0 r1] [r2 r3]]}))

(defn parse
  [input]
  (let [[rules ticket tickets] (s/split input #"\n(your ticket:|nearby tickets:)\n")
        rules (map parse-rule (s/split-lines rules))
        ticket (parse-ticket (s/trim ticket))
        tickets (map parse-ticket (s/split-lines tickets))]
    [rules ticket tickets]))

(defn valid?
  [field {ranges :ranges}]
  (let [[a b] (first ranges)
        [c d] (second ranges)]
    (or (<= a field b) (<= c field d))))

(defn field-if-invalid
  [field rules]
  (when (not-any? #(valid? field %) rules) field))

(defn invalid-fields
  [ticket rules]
  (filter #(field-if-invalid % rules) ticket))

(defn field-candidates
  "Return candidate names for a given field index"
  [field tickets rules]
  (let [all-vals (map #(nth % field) tickets)]
    (->> rules
         (filter (fn [rule]
                   (every? #(valid? % rule) all-vals)))
         (map :field))))

(defn all-candidates
  "Return candidate names for all field indices"
  [tickets rules]
  (let [fields (count (first tickets))]
    (mapv #(field-candidates % tickets rules) (range 0 fields))))

(defn find-one
  [candidates]
  (->> candidates
       (map-indexed (fn [i cs]
                      (when (= 1 (count cs)) [i (first cs)])))
       (filter util/not-nil?)
       (first)))

(defn remove-one
  [c candidates]
  (map (fn [cs] (filter #(not= c %) cs)) candidates))

(defn solve-candidates
  "The puzzle appears to be laid out in such a way that there is always a unique
   next field to solve for, so we can simply find that and proceed."
  [candidates acc]
  (let [[index c] (find-one candidates)
        removed (remove-one c candidates)]
    (if (nil? c)
      acc
      (recur removed (assoc acc c index)))))

(defn departure-indices
  [fields]
  (reduce-kv (fn [acc k v]
               (if (s/starts-with? k "departure")
                 (conj acc v)
                 acc))
             []
             fields))

(defn main
  "Day 16 of Advent of Code 2020: Ticket Translation
      lein run day16"
  [[filename]]
  (let [[rules ticket tickets] (parse (util/slurp-resource filename))
        invalid (apply concat (map #(invalid-fields % rules) tickets))
        error-rate (apply + invalid)
        valid (conj (filter #(empty? (invalid-fields % rules)) tickets) ticket)
        solved (solve-candidates (all-candidates valid rules) {})
        departures (departure-indices solved)
        departure-vals (map #(nth ticket %) departures)]
        
    (println "Ticket scanning error rate:" error-rate)
    (println "Valid:" (count valid) "/" (count tickets))
    (println "Departure field product:" (apply * departure-vals))))
