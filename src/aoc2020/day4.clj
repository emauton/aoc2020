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

(defn all-keys-present?
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

(def checks-map {"byr" (fn [value] (constantly true))
                 "iyr" (fn [value] (constantly true))
                 "eyr" (fn [value] (constantly true))
                 "hgt" (fn [value] (constantly true))
                 "hcl" (fn [value] (constantly true))
                 "ecl" (fn [value] (constantly true))
                 "pid" (fn [value] (constantly true))
                 "cid" (fn [value] (constantly true))})

(defn key-valid?
  [passport k]
  (and (contains? passport k) ((get checks-map k) (get passport k))))

(defn all-keys-valid?
  "Return true if passport is 'valid' including extra checks on each value"
  [passport]
  (every? #(key-valid? passport %) ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]))

(defn validyear?
  "Return true if year is a number between min and max (inclusive)"
  [year min max]
  (and (not= nil (re-matches #"(\d+)" year)) (<= min (Integer/parseInt year) max) ))

(defn main
  "Day 4 of Advent of Code 2020: 
      lein run day4 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (println "All keys present:" (count (filter all-keys-present? (split-passports filename))))
  (println "All keys valid:" (count (filter all-keys-valid? (split-passports filename)))))
