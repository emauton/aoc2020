(ns aoc2020.day20
  (:require [aoc2020.util :as util] 
            [clojure.pprint :as pp]
            [clojure.string :as s]))

;Tile 2473:
;.#.##.#.##
;.......#..
;.####.##..
;#...###..#
;...#.....#
;#........#
;.##.##...#
;........##
;#.##...#.#
;.#####.#.#

(def example-tile
"Tile 2473:
.#.##.#.##
.......#..
.####.##..
#...###..#
...#.....#
#........#
.##.##...#
........##
#.##...#.#
.#####.#.#")

(defn to-binary
  [tile-string]
  (s/replace (s/replace tile-string "#" "1") "." "0"))

(to-binary ".#.##.#.##")
;(Integer/parseInt (to-binary ".#####.#.#") 2)

(defn get-id
  [tile-string]
  (Integer/parseInt (s/replace (s/replace tile-string "Tile " "") ":" "")))

(defn get-top
  "Return the top border of tile"
  [tile]
  (first tile))

(defn get-bottom
  "Return the bottom border of tile"
  [tile]
  (last tile))

(defn get-left
  "Return the left border of tile"
  [tile]
  (apply str (map #(first %) tile)))

(defn get-right
  "Return the left border of tile bottom to top"
  [tile]
  (apply str (map #(last %) tile)))

(defn get-borders
  "Return a map of the borders of tile"
  [tile]
  (let [borders [(get-top tile) (get-left tile) (get-right tile) (get-bottom tile)]]
    (map (fn [b] (let [bin-b (to-binary b)] 
                   [(Integer/parseInt bin-b 2) 
                    (Integer/parseInt (s/reverse bin-b) 2)])) 
         borders)))

(defn get-tile
  [tile-strings]
  [(get-id (first tile-strings)) (get-borders (rest tile-strings))])

(defn parse-tiles
  "Returns a map of id: tile-info where tile-info is a list of
   pairs of borders and their reverse (interpreted as binary and converted to int)
   Does not include full tile info"
  [tiles]
  (map #(let [pt (get-tile %)] {:id (first pt) :edges (second pt)}) 
       (map #(s/split % #"\n") tiles)))

(defn match?
  [[a1 a2] [b1]]
  (or (= a1 b1) (= a2 b1)))

(defn count-matches
  [tile tile-list]
  (let [matches (reduce 
                 (fn [acc t] (let [candidates (for [a-edge (:edges tile)
                                                    b-edge (:edges t)
                                                    :when (not= (:id tile) (:id t))]
                                                (match? a-edge b-edge))]
                               (if (some true? candidates)
                                 (conj acc t)
                                 acc)))
                 [] tile-list)]
    (count matches)))

(defn corner?
  [tile tile-list]
  (= 2 (count-matches tile tile-list)))

(defn side?
  [tile tile-list]
  (= 3 (count-matches tile tile-list)))

(defn find-corners
  [tiles]
  (reduce (fn [acc t] (if (corner? t tiles) (conj acc t) acc)) [] tiles))

(defn categorise-tiles
  [tiles]
  (reduce
   (fn [[corners sides others] t] (case (count-matches t tiles)
                                    2 [(conj corners t) sides others]
                                    3 [corners (conj sides t) others]
                                    [corners sides (conj others t)]))
   [[][][]] tiles))

;(defn add-sides
;  [centre sides]
;  )
;
;(defn add-corners 
;  [centre corners]
;  )
;
;(defn find-match
;  [t tiles]
;  )
;
;(defn find-double-match
;  [t1 t2 tiles]
;  )

(defn arrange-four
  [[t tiles] depth]
  (let [centre {[depth depth] t}
        top-right ]
    (assoc centre [depth (+ 1 depth)] (find-match t tiles))
    (assoc centre [(+ 1 depth) depth] (find-match t tiles))
    ))

(defn arrange-tiles
  [tiles depth]
  (if (> (count tiles) 4)
    (let [[corners sides others] (categorise-tiles tiles)
          centre (arrange-tiles others (inc depth))]
      (add-corners (add-sides centre sides) corners))
    (arrange-four tiles depth)))

(defn main
  "Day 20 of Advent of Code 2020: Jurassic Jigsaw
      lein run day20 filename
   This is just an application of a parsing library. :sweat_smile:
   Only change is to move the 0 rule up to the top of the input."
  [[filename]]
  (let [tiles (parse-tiles (s/split (util/slurp-resource filename) #"\n\n"))
        corners (find-corners tiles)]
    (println "Tiles count:" (count tiles))
    (pp/pprint (categorise-tiles tiles))
    (println "Corner ID product:" (apply * (map :id corners)))))
