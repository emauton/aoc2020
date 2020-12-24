(ns aoc2020.day24
  (:require [aoc2020.util :as util]
            [instaparse.core :as insta]
            [clojure.set]))

;; Parsing

(def direction-deltas
  {"w"  [-1  1  0]
   "e"  [ 1 -1  0]
   "nw" [ 0  1 -1]
   "ne" [ 1  0 -1]
   "sw" [-1  0  1]
   "se" [ 0 -1  1]})

(def directions
  (insta/parser
    "INSTRS = DIR*
     DIR = 'e' | 'w' | 'nw' | 'ne' | 'sw' | 'se'"))

(defn parse
  "Parse a list of directions into 3d deltas"
  [line]
  (let [dirs (map second (rest (directions line)))]
    (map direction-deltas dirs)))

;; Part 1

(defn sum-deltas
  [ds]
  (reduce (fn [acc d]
            (vec (map + acc d))) ds))

(defn flip
  [floor c]
  (assoc floor c (inc (get floor c 0))))

(defn flip-tiles
  "Given a list of instructions, assemble a floor as
   a map of coord -> number of times flipped"
  [instrs]
  (let [coords (map sum-deltas instrs)]
    (reduce (fn [floor c]
              (flip floor c))
            {} coords)))

(defn black?
  [floor c]
  (odd? (get floor c 0)))

(defn count-black
  [floor]
  (count (filter (fn [[_ cnt]] (odd? cnt)) floor)))

;; Part 2

(defn neighbours
  "Return hex neighbour coordinates for coord (in infinite space)"
  [coord]
    (map #(vec (map + coord %)) (vals direction-deltas)))

(defn flip-coord?
  "Implement annoying flip rules for a particular coordinate"
  [floor coord]
  (let [black (black? floor coord)
        black-neighbours (count (filter #(black? floor %) (neighbours coord)))]
    (cond
      (and black
           (or (= black-neighbours 0)
               (> black-neighbours 2))) true
      (and (not black)
           (= black-neighbours 2)) true
      :else false)))

(defn iteration
  "One day's worth of tile-flipping"
  [floor]
  (let [in-floor (set (keys floor))
        all-neighbours (map #(set (neighbours %)) in-floor)
        coords (apply clojure.set/union (conj all-neighbours in-floor))
        to-flip (filter #(flip-coord? floor %) coords)]
    (reduce flip floor to-flip)))

(defn main
  "Day 24 of Advent of Code 2020: Lobby Layout
      lein run day24 <filename>"
  [[filename]]
   (let [instrs (map parse (util/read-lines filename))
         floor (flip-tiles instrs)
         after-100 (nth (iterate iteration floor) 100)]
     (println "Part 1 black tiles:" (count-black floor))
     (println "Part 2 black tiles:" (count-black after-100))))
