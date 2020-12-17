(ns aoc2020.day17
  (:require [aoc2020.util :as util] 
            [clojure.pprint :as pp]
            [clojure.string :as s]))

(defn neighbours-3d
  "Return neighbour coordinates for coord (in infinite space)
   Index in slice, row, col order - z y x"
  [coord]
  (let [dzyx (for [i (range -1 2)
                   j (range -1 2)
                   k (range -1 2)
                   :when (not= [0 0 0] [i j k])]
              [i j k])]
    (map (fn [delta] (vec (map + coord delta))) dzyx)))

(defn neighbours-4d
  "Return neighbour coordinates for coord (in infinite space)
   Index in cube, slice, row, col order - w z y x"
  [coord]
  (let [dwzyx (for [i (range -1 2)
                   j (range -1 2)
                   k (range -1 2)
                   l (range -1 2)
                   :when (not= [0 0 0 0] [i j k l])]
              [i j k l])]
    (map (fn [delta] (vec (map + coord delta))) dwzyx)))

(defn map-cube-3d
  "Return a map data structure initialised with input"
  [input]
  (let [z 0
        cube {}
        indexed-input (map-indexed (fn [i row] (map-indexed (fn [j col] [i j col]) row)) input)]
    (reduce 
     (fn [acc row] (reduce (fn [inneracc [y x item]] (assoc inneracc [z y x] item)) acc row)) 
     cube indexed-input)))

(defn map-cube-4d
  "Return a map data structure initialised with input"
  [input]
  (let [w 0 
        z 0
        cube {}
        indexed-input (map-indexed (fn [i row] (map-indexed (fn [j col] [i j col]) row)) input)]
    (reduce 
     (fn [acc row] (reduce (fn [inneracc [y x item]] (assoc inneracc [w z y x] item)) acc row)) 
     cube indexed-input)))

(defn neighbour-values-3d
  "Return list of neighbour values (that is '#' or '.') at coord,
   by rules specified in neighbours"
  [cube coord]
  (sort (map #(get cube %) (neighbours-3d coord))))

(defn neighbour-values-4d
  "Return list of neighbour values (that is '#' or '.') at coord,
   by rules specified in neighbours"
  [cube coord]
  (sort (map #(get cube %) (neighbours-4d coord))))

(defn new-seat-3d
  "Return new seat val at coord given direct rules"
  [cube coord]
  (let [val (get cube coord)
        n (neighbour-values-3d cube coord)
        active-count (count (filter #(= \# %) n))]
    (case val
      \# (if (or (= active-count 2) (= active-count 3)) 
           \# 
           \.)
      (nil \.) (if (= active-count 3)
           \#
           \.))))

(defn new-seat-4d
  "Return new seat val at coord given direct rules"
  [cube coord]
  (let [val (get cube coord)
        n (neighbour-values-4d cube coord)
        active-count (count (filter #(= \# %) n))]
    (case val
      \# (if (or (= active-count 2) (= active-count 3)) 
           \# 
           \.)
      (nil \.) (if (= active-count 3)
           \#
           \.))))

(defn next-round-3d
  [cube]
  (let [n-list (distinct (apply concat (mapv #(neighbours-3d (first %)) cube)))]
    (reduce (fn [new-cube n] (assoc new-cube n (new-seat-3d cube n))) {} n-list)))

(defn next-round-4d
  [cube]
  (let [n-list (distinct (apply concat (mapv #(neighbours-4d (first %)) cube)))]
    (reduce (fn [new-cube n] (assoc new-cube n (new-seat-4d cube n))) {} n-list)))

(defn count-active
  [cube]
  (count (filter #(= \# (second %)) cube)))

(defn main
  "Day 17 of Advent of Code 2020: Conway Cubes
      lein run day17 filename"
  [[filename]]
  (let [mags-input ["######.#"
                    "#.###.#."
                    "###....."
                    "#.####.."
                    "##.#.###"
                    ".######."
                    "###.####"
                    "######.#"]
        mags-cube-3d (map-cube-3d mags-input)
        sixth-3d-mags (nth (iterate next-round-3d mags-cube-3d) 6)
        mags-cube-4d (map-cube-4d mags-input)
        sixth-4d-mags (nth (iterate next-round-4d mags-cube-4d) 6)]
;        sixth-4d (next-round-4d (next-round-4d (next-round-4d (next-round-4d (next-round-4d (next-round-4d mags-cube-4d))))))]
    (println "New count" (count-active sixth-3d-mags))
    (println "New count" (count-active sixth-4d-mags))))
    
    ;(pp/pprint mags-input)))
