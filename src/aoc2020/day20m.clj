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

(defn count-matches
  [tile]
  (count (filter #(not= nil %) (map (fn [[k v]] (last v)) (:borders tile)))))

(defn get-corners
  [tiles]
  (reduce (fn [corners t] (if (= (count-matches t) 2)
                            (conj corners t)
                            corners))
          [] tiles))

;(defn categorise-tiles
;  [tiles]
;  (reduce (fn [[corners sides others] t] (case (count-matches t) 
;                                           2 [(conj corners t) sides others]
;                                           3 [corners (conj sides t) others]
;                                           [corners sides (conj others t)]))
;          [[][][]] tiles))

(defn transform-core
  [core ttype direction]
  (case ttype
    :rotate (case direction
              :left (apply mapv vector (mapv reverse core))
              :right (mapv reverse (apply mapv vector core)))
    :flip (case direction
            :horizontal (mapv reverse core)
            :vertical (vec (reverse core)))))

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
        (println "borders neighbour pre" (:borders neighbour))
        (println "(right) current id, transform list" (:id current) transforms)
        (println "borders neighbour post" (:borders transformed))
        (recur transformed
               (conj acc transformed)
               (get-neighbour :right transformed tiles))))))

(defn borders-match?
  [b1 b2]
  (= (first b1) (reverse (first b2))))

(defn rows-match?
  [row1 row2]
;  (when (= 1741 (:id (first row2)))
;  (pp/pprint row1)
;  (pp/pprint row2))
  (every? #(= true %) (for [i (range (count row2))]
                        (do 
                          (println (:id (nth row1 i))(:bottom (:borders (nth row1 i))) (:id (nth row2 i)) (:top (:borders (nth row2 i))))
                          (borders-match? (:bottom (:borders (nth row1 i))) (:top (:borders (nth row2 i))))))))

(defn create-grid
  [tiles]
  (let [corners (get-corners tiles)
        seed (top-left-corner corners)]
    (loop [current seed
           acc [(create-row current tiles)]
           neighbour (get-neighbour :bottom current tiles)]
      (if (nil? neighbour)
        acc
        (let [transforms (get-transforms-bottom current neighbour)
              transformed (transform-tile neighbour transforms)
              transformed (if (= 1741 (:id transformed))
                            (transform-tile transformed [[:flip :horizontal]])
                            transformed)
              new-row (create-row transformed tiles)]
          (println "current id, transform list" (:id current) transforms)
          (println "new-row match" (rows-match? (last acc) new-row))
          (recur transformed
                 (conj acc new-row)
                 (get-neighbour :bottom transformed tiles)))))))

(def test-image (mapv vec (s/split ".#.#..#.##...#.##..#####
###....#.#....#..#......
##.##.###.#.#..######...
###.#####...#.#####.#..#
##.#....#.##.####...#.##
...########.#....#####.#
....#..#...##..#.#.###..
.####...#..#.....#......
#..#.##..#..###.#.##....
#.####..#.####.#.#.###..
###.#.#...#.######.#..##
#.####....##..########.#
##..##.#...#...#.#.#.#..
...#..#..#.#.##..###.###
.#.#....#.##.#...###.##.
###.#...#..#.##.######..
.#.#.###.##.##.#..#.##..
.####.###.#...###.#..#.#
..#.#..#..#.#.#.####.###
#..####...#.#.#.###.###.
#####..#####...###....##
#.##..#..#...#..####...#
.#.###..##..##..####.##.
...###...##...#...#..###" #"\n")))

;(println test-image)

(defn get-image-2
  [grid]
  (let [cores (map (fn [row] (map :core row)) grid)]
    (apply concat (mapv #(reduce (fn [acc tile]
                                   (vec (map-indexed (fn [index core-row]
                                                       (vec (concat core-row (nth tile index))))
                                                     acc)))
                                 %)
                        cores))))

(defn get-image
  [grid]
  (let [cores (mapv (fn [row] (mapv (fn [tile] (:core tile)) row)) grid)]
    (println "cores row cols" (count cores) (count (first cores)))
    (into [] (apply concat (mapv (fn [c] (for [i (range (count c))]
                                           (into [] (apply concat (mapv (fn [t] (nth t i)) c))))) cores)))))

(def monster-string [(vec "                  # ") 
                     (vec "#    ##    ##    ###")
                     (vec " #  #  #  #  #  #   ")])

(def monster-string2 [(vec (s/reverse "                  # ")) 
                     (vec (s/reverse "#    ##    ##    ###"))
                     (vec (s/reverse " #  #  #  #  #  #   "))])

(def monster-offsets
  (for [i (range (count monster-string)) 
        j (range (count (first monster-string)))
        :when (= \# (get-in monster-string [i j]))]
    [i j]))

(def monster-offsets2
  (for [i (range (count monster-string2)) 
        j (range (count (first monster-string2)))
        :when (= \# (get-in monster-string2 [i j]))]
    [i j]))

(defn match-monster?
  [index image]
    (or (every? (fn [o] (= \# (get-in image (mapv + index o)))) monster-offsets)
        (every? (fn [o] (= \# (get-in image (mapv + index o)))) monster-offsets)))

(defn generate-offsets
  [row-count row-length]
  (for [i (range (inc (- row-count (count monster-string))))
        j (range (inc (- row-length (count (first monster-string)))))]
    [i j]))

(defn transform-image
  [image list]
  (if (not= 0 (count list))
       (reduce (fn [acc [ttype direction]] 
                 (transform-core acc ttype direction))
               image list)
       image))

(transform-image test-image [[:rotate :left]])

(defn count-monsters
  [image offsets]
  (count (filter #(match-monster? % image) offsets)))
  
(defn count-non-monster-hashes
  [image]
  (let [offsets (generate-offsets (count image) (count (first image)))
        transform-list [[]
                        [[:rotate :left]]
                        [[:rotate :left] [:rotate :left]]
                        [[:rotate :right]]
                        [[:flip :vertical]]
                        [[:flip :vertical] [:rotate :left]]
                        [[:flip :vertical] [:rotate :left] [:rotate :left]]
                        [[:flip :vertical] [:rotate :right]]]
        m (first (filter #(not= 0 %) (map #(count-monsters (transform-image image %) offsets) transform-list)))
        ;m (first (filter (fn [tl] (not= 0 (count-monsters (transform-image image tl) offsets))) transform-list))
        hash-count (reduce (fn [acc row] (+ acc (count (filter #(= \# %) row)))) 0 image)]
;    (println "offsets" offsets)
;    (println "image" (count image) (count (first image)))
    (- hash-count (* m 15))))

(defn main
  "Day 20 of Advent of Code 2020: Jurassic Jigsaw
      lein run day20 filename"
  [[filename]]
  (let [tiles (map parse-tile (s/split (util/slurp-resource filename) #"\n\n"))
        ntiles (add-neighbours tiles)
        grid (create-grid ntiles)
        image (get-image-2 grid)
        transformed (transform-image image [[:rotate :right]])
        x (mapv reverse (apply mapv vector image))]
    
   ; (println "image rowcol" (count image) (count (first image)))
   ; (println "transformed rowcol" (count transformed) (count (first transformed)))
   ; (println "x rowcol" (count x) (count (first x)))
   ; (println (map count image)2371)
   ; (println (map count grid))
;    (println "untransformed first row")
;    (pp/pprint (first image))
;    (println "transformed first row")
;    (pp/pprint (first x))
    (println (count-non-monster-hashes image))))
;    (println (count-non-monster-hashes transformed))))
;(pp/pprint grid)
;  ))
 


;Tile 1741:
;#.#.#...#.
;#.....#...
;..........
;#....##.##
;#.....#.#.
;#...#.....
;.....#..##
;.###...##.
;#.....#..#
;#.#.#..###

;Tile 3677:
;##..###.##
;#.....###.
;.###......
;#..#..#..#
;.......##.
;.....#...#
;.##.....##
;.#......##
;....#...##
;###..#....