(ns aoc2020.day20m
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

; data structure for tile
; :id int
; :borders {
; :top [[value reverse] neighbourID]
; :right [[value reverse] neighbourID]
; :bottom [[value reverse] neighbourID]
; :left [[value reverse] neighbourID]
; }
; :core [[]...]

; ops
; (get-neighbour tile border-name) returns neighbour id for that border
; (get-value tile border-name) returns [value reverse] for that border
; (get-matching-border border neighbour-tile) returns the border name of 
;                 the matching border and true (if they match) or false
;                 (if they're opposite)
; 
; (make-row first-tile tiles) starting with first-tile, add tiles on the right until
; the most recently added tile has no right neighbour
; 

(defn get-id
  [id-string]
  (Integer/parseInt (s/replace (s/replace id-string "Tile " "") ":" "")))

(defn binary-pair
  [tile-string]
  (let [binary (s/replace (s/replace tile-string "#" "1") "." "0")]
    [(Integer/parseInt binary 2) 
     (Integer/parseInt (s/reverse binary) 2)])) 

(defn get-borders
 [image-strings]
  {:top (binary-pair (first image-strings)) 
   :right (binary-pair (apply str (map #(last %) image-strings))) 
   :bottom (binary-pair (s/reverse (last image-strings))) 
   :left (binary-pair (s/reverse (apply str (map #(first %) image-strings))))}) 

(defn get-core
  [image-strings]
  (mapv #(vec (subs % 1 (dec (count %)))) (subvec (vec image-strings) 1 (dec (count image-strings)))))

(defn parse-tile
  [tile]
  (let [tile-strings (s/split tile #"\n")]
    {:id (get-id (first tile-strings)) 
     :borders (get-borders (rest tile-strings)) 
     :core (get-core (rest tile-strings))}))

(defn match?
  [[a1 a2] [b1]]
  (or (= a1 b1) (= a2 b1)))

(defn get-match
  [id1 border1 tile-list]
  (:id (first (filter (fn [tile2] (some true? (for [border2 (vals (:borders tile2))
                                                    :when (not= id1 (:id tile2))]
                                                (match? border1 border2))))
                      tile-list))))

(defn matching-sides
  [tile tile-list]
  (into {} (for [[name border] (:borders tile)]
             [name [border (get-match (:id tile) border tile-list)]])))

(defn add-neighbours
  [tile-list]
  (map (fn [tile] (assoc tile :borders (matching-sides tile tile-list))) tile-list))

;(get-id (first (s/split example-tile #"\n")))
;(get-borders (rest (s/split example-tile #"\n")))
;(get-core (rest (s/split example-tile #"\n")))
;(parse-tile example-tile)

; :borders {
; :top [[value reverse] neighbourID]
; :right [[value reverse] neighbourID]
; :bottom [[value reverse] neighbourID]
; :left [[value reverse] neighbourID]
; }

(defn count-matches
  [tile]
  (count (filter #(not= nil %) (map (fn [[k v]] (last v)) (:borders tile)))))

(defn categorise-tiles
  [tiles]
  (reduce (fn [[corners sides others] t] (case (count-matches t) 
                                           2 [(conj corners t) sides others]
                                           3 [corners (conj sides t) others]
                                           [corners sides (conj others t)]))
          [[][][]] tiles))

(defn transform-core
  [core ttype direction]
  (case ttype
    :rotate (case direction
              :left (apply map vector (map reverse core))
              :right (mapv reverse (apply map vector core)))
    :flip (case direction
            :horizontal (mapv reverse core)
            :vertical (vec (reverse core)))))

(pp/pprint (transform-core [[\# \. \. \# \. \. \. \.] 
                            [\. \. \. \# \# \. \. \#] 
                            [\# \# \# \. \# \. \. \.] 
                            [\# \. \# \# \. \# \# \#] 
                            [\# \. \. \. \# \. \# \#] 
                            [\# \. \# \. \# \. \. \#] 
                            [\. \# \. \. \. \. \# \.] 
                            [\# \# \. \. \. \# \. \#]] :flip :vertical))

(defn transform-borders
  [borders ttype direction]
  (case ttype
    :rotate (case direction
              :left {:top (:right borders)
                     :right (:bottom borders)
                     :bottom (:left borders)
                     :left (:top borders)}
              :right {:top (:left borders)
                      :right (:top borders)
                      :bottom (:right borders)
                      :left (:bottom borders)})
    :flip (case direction
            :horizontal {:top [(vec (reverse (first (:top borders)))) (second (:top borders))] 
                         :right (:left borders) 
                         :bottom [(vec (reverse (first (:bottom borders)))) (second (:bottom borders))]
                         :left (:right borders)}
            :vertical {:top (:bottom borders)
                       :right [(vec (reverse (first (:right borders)))) (second (:right borders))]
                       :bottom (:top borders)
                       :left [(vec (reverse (first (:left borders)))) (second (:left borders))]})))

(pp/pprint (transform-borders {:top [[210 300] 1427]
                               :right [[89 616] 3079] 
                               :bottom [[924 231] nil] 
                               :left [[318 498] 1951]} :flip :vertical))


(defn transform-once
  [tile ttype direction]
  {:id (:id tile)
   :borders (transform-borders (:borders tile) ttype direction) 
   :core (transform-core (:core tile) ttype direction)})

(defn transform-tile
  [tile transforms]
  (reduce (fn [acc [ttype direction]]
            (transform-once acc ttype direction))
          tile transforms))

(defn arrange-tiles
  [tiles]
    (let [[corners sides others] (categorise-tiles tiles)]

    (println (map :id corners))
    (println (map :id sides))
    (println (map :id others))
    ))

(defn top-left-corner
  [corners]
  (first (filter #(and (nil? (last (:top (:borders %)))) (nil? (last (:left (:borders %))))) corners)))

(defn get-neighbour
  [side tile1 tiles]
  (let [id2 (last (side (:borders tile1)))
        tile2 (first (filter #(= (:id %) id2) tiles))]
    tile2))

(defn get-side-by-id
  [borders id]
  (first (for [[side deets] borders :when (= (last deets) id)] [side deets])))

(defn get-transforms-right
  "Returns a list of the transforms that should be performed on tile2 so that it
   lines up with the right side of tile1."
  [tile1 tile2]
  (let [id1 (:id tile1)
        border-vals1 (first (:right (:borders tile1)))
        [side [border-vals2] _] (get-side-by-id (:borders tile2) id1)]
    (case side
      ; note: the borders are read in a clockwise direction, so eg for a left
      ; border to match a right border they need to not be equal
      :left   (if (= border-vals1 border-vals2) 
                [[:flip :vertical]]
                [])
      :right  (if (= border-vals1 border-vals2) 
                [[:flip :horizontal]] 
                [[:flip :horizontal] [:flip :vertical]] )
      :top    (if (= border-vals1 border-vals2) 
                [[:rotate :left] [:flip :vertical]] 
                [[:rotate :left]])
      :bottom (if (= border-vals1 border-vals2) 
                [[:rotate :right] [:flip :vertical]] 
                [[:rotate :right]]))))

(defn get-transforms-bottom
  "Returns a list of the transforms that should be performed on tile2 so that it
   lines up with the bottom side of tile1."
  [tile1 tile2]
  (let [id1 (:id tile1)
        border-vals1 (first (:bottom (:borders tile1)))
        [side [border-vals2] _] (get-side-by-id (:borders tile2) id1)]
    (case side
      ; note: the borders are read in a clockwise direction, so eg for a left
      ; border to match a right border they need to not be equal
      :left   (if (= border-vals1 border-vals2) 
                [[:rotate :right] [:flip :horizontal]]
                [[:rotate :right]])
      :right  (if (= border-vals1 border-vals2) 
                [[:rotate :left] [:flip :horizontal]] 
                [[:rotate :left]])
      :top    (if (= border-vals1 border-vals2) 
                [[:flip :horizontal]] 
                [])
      :bottom (if (= border-vals1 border-vals2) 
                [[:flip :vertical]] 
                [[:flip :vertical] [:flip :horizontal]]))))


(defn create-row
  [seed tiles]
  (loop [current seed
         acc [current]
         neighbour (get-neighbour :right current tiles)]
    (if (nil? neighbour)
      acc
      (let [transforms (get-transforms-right current neighbour)
            transformed (transform-tile neighbour transforms)]
        (recur transformed
               (conj acc transformed)
               (get-neighbour :right transformed tiles))))))

(defn create-grid
  [tiles]
  (let [[corners _ _] (categorise-tiles tiles)
        seed (top-left-corner corners)]
    (println "corners" corners)
    (println "seed" seed)
    (loop [current seed
           acc [(create-row current tiles)]
           neighbour (get-neighbour :bottom current tiles)]
      (if (nil? neighbour)
        acc
        (let [transforms (get-transforms-bottom current neighbour)
              transformed (transform-tile neighbour transforms)]
          (recur transformed
                 (conj acc (create-row transformed tiles))
                 (get-neighbour :bottom transformed tiles)))))))

(defn main
  "Day 20 of Advent of Code 2020: Jurassic Jigsaw
      lein run day20 filename
   This is just an application of a parsing library. :sweat_smile:
   Only change is to move the 0 rule up to the top of the input."
  [[filename]]
  (let [tiles (map parse-tile (s/split (util/slurp-resource filename) #"\n\n"))
        ntiles (add-neighbours tiles)
        [corners sides others] (categorise-tiles tiles)
        first-corner (top-left-corner corners)
        tile1 (nth ntiles 5)
        tile2 (get-neighbour :bottom tile1 ntiles)
        transforms (get-transforms-bottom tile1 tile2)
        tile2-post (transform-tile tile2 transforms)]
    (println "matching side:" (matching-sides (second tiles) tiles))
;    (println "first tile" (first ntiles))
;    (println "right neighbour first tile" (get-right-neighbour (first ntiles) ntiles))
    (println "borders first ntiles:" (:borders (first ntiles)))
    (println "Tile 1")
    (pp/pprint tile1)
    (println "Tile 2")
    (pp/pprint tile2)
    (println "Transforms:" transforms)
    (println "Tile 2 post")
    (pp/pprint tile2-post)
    (println "Row:")
    (pp/pprint (create-grid ntiles))))
