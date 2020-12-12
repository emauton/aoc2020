(ns aoc2020.day11
  (:require [aoc2020.util :as util]
            [clojure.pprint :as pp]))

(defn make-grid
  "Make a 2D grid from line input.
   access: (get-in grid [row col])
   update: (assoc-in grid [row col] val)"
  [lines]
  (mapv vec lines))

(defn neighbours
  "Return neighbour coordinates for coord in grid"
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
            (map (fn [delta] (vec (map + coord delta))) dyx))))

;(defn nearest-ul
;  "nearest neighbour seat to the upper left"
;  [grid row col]
;  (let [ul (for [i (range)] [(- row i) (- col i)])]
;    (reduce (fn [seat-coords]
;              (case (get-in grid seat-coords) 
;                nil (reduced nil)
;                \. nil
;                \L (reduced seat-coords)
;                \# (reduced seat-coords))) ul)))
;
;(defn distant-neighbours
;  "Return neighbour coordinates for coord in grid where neighbour
;   is the first seat in a given direction, rather than adjacent"
;  [grid coord]
;  (let [rows (count grid)
;        cols (count (first grid))
;        dyx (for [i (range -1 2)
;                  j (range -1 2)
;                  :when (not= [0 0] [i j])]
;              [i j])]
;    (filter (fn [[r c]]
;              (and (< -1 r rows)
;                   (< -1 c cols)))
;            (map (fn [delta] (vec (map + coord delta))) dyx))))

(defn neighbour-values
  "Return neighbour values for coord in grid"
  [grid coord]
  (sort (map #(get-in grid %) (neighbours grid coord))))

(defn all-coords
  "return a vector of all the coordinates [row col] given max(row) and max(cols)"
  [rows cols]
  (for [row (range rows)]
   (for [col (range cols)] [row col])))

(defn new-seat
  "Return the new value for the seat at coord"
  [grid coord]
  (let [val (get-in grid coord)
        n (neighbour-values grid coord)]
    (case val 
      \L (if (empty? (filter #(= \# %) n)) \# \L) 
      \# (if (>= (count (filter #(= \# %) n)) 4) \L \#) 
      \. \.)))

(defn next-round
  "Flip seats according to the rules"
  [grid rows cols]
  (let [coords (all-coords rows cols)]
    (mapv #(mapv (fn [c] (new-seat grid c)) %) coords)))

(defn all-rounds
  [grid rows cols]
  (let [next-r (next-round grid rows cols)]
    (if (= 0 (compare grid next-r))
      next-r
      (all-rounds next-r rows cols))))

(defn count-occupied
  [grid] 
  (reduce 
   (fn [acc row] (+ acc (count (filter #(= \# %) row)))) 0 grid))

(defn main
  "Day 11 of Advent of Code 2020: Seating System
      lein run day11 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [grid (make-grid (util/read-lines filename))
        rows (count grid)
        cols (count (first grid))
        final (all-rounds grid rows cols)]
    ;(pp/pprint grid)
    (println)
;    (println "rows cols" (count grid) (count (first grid)))
;    (println "neighbours [0 0]:" (neighbours grid [0 0])) ; top left
;    (println "neighbours [1 1]:" (neighbours grid [1 1])) ; middle of grid
;    (println "neighbours [0 1]:" (neighbours grid [0 1])) ; top side
;    (println "neighbours [1 0]:" (neighbours grid [1 0])) ; left side
;    (println "neighbours [n n]:" (neighbours grid [(dec rows) (dec cols)])) ; bottom right
;    (println)
;    (println "neighbour vals [0 0]:" (neighbour-values grid [0 0]))
;    (println "neighbour vals [1 1]:" (neighbour-values grid [1 1]))
;    (println "neighbour vals [0 1]:" (neighbour-values grid [0 1]))
    (println)
    (pp/pprint final)
    (println "Occupied seats: " (count-occupied final))))
