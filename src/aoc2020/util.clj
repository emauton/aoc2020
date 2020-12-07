(ns aoc2020.util
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn slurp-resource
  "Slurp all data from a file in project root's resources/"
  [filename]
  (->  filename io/resource slurp))

(defn read-lines
  "Read line-by-line from a file in project root's resources/"
  [filename]
  (-> filename io/resource io/reader line-seq))

(defn read-numbers [filename]
  (map #(Integer/parseInt %) (read-lines filename)))

(defn break-commas
  "Break up a line on commas"
  [line]
  (string/split (string/trim line) #","))

(defn in? 
  "Test if a is in coll"
  [coll a] 
  (some #(= a %) coll))