(ns aoc2020.day11
  (:require [aoc2020.util :as util]))

;; Shared

(defn all-coords
  "Return 2D vector of coords in grid"
  [grid]
  (for [y (range (count grid))]
    (for [x (range (count (first grid)))]
      [y x])))

(defn next-round
  "Flip seats according to the rules represented by new-seat-fn"
  [grid new-seat-fn]
  (mapv #(mapv (fn [c] (new-seat-fn grid c)) %)
        (all-coords grid)))

(defn neighbour-values
  "Return list of neighbour values (that is '# L or .' at coord,
   by rules specified in neighbours-fn"
  [neighbours-fn grid coord]
  (sort (map #(get-in grid %) (neighbours-fn grid coord))))

(defn count-occupied
  [grid]
  (reduce (fn [acc row]
            (+ acc
               (count (filter #(= \# %) row))))
          0 grid))

;; Part 1

(defn neighbours-direct
  "Return direct neighbour coordinates for coord"
  [grid coord]
  (let [rows (count grid)
        cols (count (first grid))
        dyx (for [i (range -1 2)
                  j (range -1 2)
                  :when (not= [0 0] [i j])]
              [i j])]
    (filter (fn [[r c]]
              (and (< -1 r rows)
                   (< -1 c cols)))
            (map (fn [delta]
                   (vec (map + coord delta)))
                 dyx))))

(defn new-seat-direct
  "Return new seat val at coord given direct rules"
  [grid coord]
  (let [val (get-in grid coord)
        n (neighbour-values neighbours-direct grid coord)]
    (case val
      \L (if (empty? (filter #(= \# %) n)) \# \L)
      \# (if (>= (count (filter #(= \# %) n)) 4) \L \#)
      \. \.)))

(defn all-rounds-direct
  [grid]
  (let [next-grid (next-round grid new-seat-direct)]
    (if (= 0 (compare grid next-grid))
      next-grid
      (recur next-grid))))

;; Part 2

(defn in-bounds?
  [grid [y x]]
  (let [rows (count grid)
        cols (count (first grid))]
    (cond
      (or (neg? y) (neg? x)) false
      (or (>= y rows) (>= x cols)) false
      :else true)))

(defn nearest-seat
  [grid [y x] [dy dx]]
  (let [candidates (take-while #(in-bounds? grid %)
                               (for [i (range)
                                     :when (pos? i)]
                                 [(+ y ( * i dy)) (+ x (* i dx))]))]
    (first (drop-while #(= \. (get-in grid %)) candidates))))

(defn neighbours-los
  "Return line-of-sight neighbour coordinates for coord"
  [grid coord]
  (let [dyx (for [i (range -1 2)
                  j (range -1 2)
                  :when (not= [0 0] [i j])]
              [i j])]
    (filter util/not-nil? (map #(nearest-seat grid coord %) dyx))))

(defn new-seat-los
  "Return new seat val at coord given LoS rules"
  [grid coord]
  (let [val (get-in grid coord)
        n (neighbour-values neighbours-los grid coord)]
    (case val
      \L (if (empty? (filter #(= \# %) n)) \# \L)
      \# (if (>= (count (filter #(= \# %) n)) 5) \L \#)
      \. \.)))

(defn all-rounds-los
  [grid]
  (let [next-grid (next-round grid new-seat-los)]
    (if (= 0 (compare grid next-grid))
      next-grid
      (recur next-grid))))

(defn main
  "Day 11 of Advent of Code 2020: Seating System
      lein run day11 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [grid (mapv vec (util/read-lines filename))
        direct (all-rounds-direct grid)
        los (all-rounds-los grid)]
    (println "Occupied by direct neighbour:" (count-occupied direct))
    (println "Occupied by LoS neighbour:" (count-occupied los))))
