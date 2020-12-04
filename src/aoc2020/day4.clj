(ns aoc2020.day4
  (:require [aoc2020.util :as util] 
            [clojure.string :as s] ))

(defn parse-passport
  "Return a map of the key:value pairs in the input string"
  [input]
  (apply hash-map (s/split input #"[ :\n]+")))

(defn split-passports
  "Take output of slurp-resource and split into a sequence of passports"
  [filename]
  (let [input (util/slurp-resource filename)
        passports (s/split input #"\n\n")]
    (map parse-passport passports)))

(defn valid?
  "Return true if passport is 'valid'
    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID) (skipped for nefarious purposes)"
  [passport]
  (every? #(contains? passport %) ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]))

(defn validyear?
  "Return true if year is a number between min and max (inclusive)"
  [year min max]
  (and (not= nil (re-matches #"(\d+)" year)) (<= min (Integer/parseInt year) max) ))

(defn validhgt?
  "Return true is hgt is a number followed by cm or in and 
      if cm   150 <= hgt <= 193
      if in   59 <= hgt <= 76 "
  [hgt]
  (let [])
  (or (s/ends-with? hgt "cm") 
      (s/ends-with? hgt "in")
      )
       ())

(defn main
  "Day 4 of Advent of Code 2020: 
      lein run day4 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (println "Number of valid passports:" (count (filter valid? (split-passports filename)))))