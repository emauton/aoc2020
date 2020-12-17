(ns aoc2020.day17
  (:require [aoc2020.util :as util]))

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

(defn neighbour-values
  "Return list of neighbour values (that is '#' or '.') at coord,
   by rules specified in neighbours-fn"
  [neighbours-fn cube coord]
  (sort (map #(get cube %) (neighbours-fn coord))))

(defn new-seat
  "Return new seat val at coord given direct rules"
  [neighbours-fn cube coord]
  (let [val (get cube coord)
        n (neighbour-values neighbours-fn cube coord)
        active-count (count (filter #(= \# %) n))]
    (case val
      \# (if (or (= active-count 2) (= active-count 3)) 
           \# 
           \.)
      (nil \.) (if (= active-count 3)
           \#
           \.))))

(defn next-round
  [neighbours-fn cube]
  (let [n-list (distinct (apply concat (mapv #(neighbours-fn (first %)) cube)))]
    (reduce (fn [new-cube n] (assoc new-cube n (new-seat neighbours-fn cube n))) {} n-list)))

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
        cian-input [".##...#."
                    ".#.###.."
                    "..##.#.#"
                    "##...#.#"
                    "#..#...#"
                    "#..###.."
                    ".##.####"
                    "..#####."]
        next-round-3d (partial next-round neighbours-3d)
        next-round-4d (partial next-round neighbours-4d)
        mags-cube-3d (map-cube-3d mags-input)
        cian-cube-3d (map-cube-3d cian-input)
        sixth-3d-mags (nth (iterate next-round-3d mags-cube-3d) 6)
        sixth-3d-cian (nth (iterate next-round-3d cian-cube-3d) 6)
        mags-cube-4d (map-cube-4d mags-input)
        cian-cube-4d (map-cube-4d cian-input)
        sixth-4d-mags (nth (iterate next-round-4d mags-cube-4d) 6)
        sixth-4d-cian (nth (iterate next-round-4d cian-cube-4d) 6)]
    (println "3d mags" (count-active sixth-3d-mags))
    (println "4d mags" (count-active sixth-4d-mags))
    (println "3d cian" (count-active sixth-3d-cian))
    (println "4d cian" (count-active sixth-4d-cian))))