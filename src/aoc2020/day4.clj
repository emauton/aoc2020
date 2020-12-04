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

(defn validyear?
  "Return true if year is a number between min and max (inclusive)"
  [year min max]
  (and (not= nil (re-matches #"(\d+)" year)) (<= min (Integer/parseInt year) max) ))

(defn validhgt?
  "Return true is hgt is a number followed by cm or in and 
      if cm   150 <= hgt <= 193
      if in   59 <= hgt <= 76 "
  [hgt]
  (let [[_ n unit] (re-matches #"(\d+)(\w+)" hgt)]
    (cond
      (or (nil? n) (nil? unit)) false
      (= unit "cm") (<= 150 (Integer/parseInt n) 193)
      (= unit "in") (<= 59 (Integer/parseInt n) 76)
      :else false
    )))

(defn validhcl?
  "Return true if hcl is a '#' followed by 6 hex digits"
  [hcl]
  (not= nil (re-matches #"#[0-9a-f]{6}" hcl)))

(defn validecl?
  "Return true if ecl is our list of eye colours"
  [ecl]
  (some #(= ecl %) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))

(defn validpid?
  "Return true if pid is a nine-digit number"
  [pid]
  (not= nil (re-matches #"[0-9]{9}" pid)))

(def checks-map {"byr" #(validyear? % 1920 2002)
                 "iyr" #(validyear? % 2010 2020)
                 "eyr" #(validyear? % 2020 2030)
                 "hgt" #(validhgt? %)
                 "hcl" #(validhcl? %)
                 "ecl" #(validecl? %)
                 "pid" #(validpid? %)
                 "cid" (fn [value] (constantly true))})

(defn key-valid?
  [passport k]
  (and (contains? passport k) ((get checks-map k) (get passport k))))

(defn all-keys-valid?
  "Return true if passport is 'valid' including extra checks on each value"
  [passport]
  (every? #(key-valid? passport %) ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]))

(defn main
  "Day 4 of Advent of Code 2020: 
      lein run day4 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (println "All keys present:" (count (filter all-keys-present? (split-passports filename))))
  (println "All keys valid:" (count (filter all-keys-valid? (split-passports filename)))))
