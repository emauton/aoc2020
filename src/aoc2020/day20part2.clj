(ns aoc2020.day20part2
  (:require [aoc2020.util :as util] 
            [clojure.pprint :as pp]
            [clojure.string :as s]))

(defn parse-id
  [id-string]
  (Integer/parseInt (s/replace (s/replace id-string "Tile " "") ":" "")))

(defn binary-pair
  [tile-string]
  (let [binary (s/replace (s/replace tile-string "#" "1") "." "0")]
    [(Integer/parseInt binary 2) 
     (Integer/parseInt (s/reverse binary) 2)])) 

(defn parse-borders
 [image-strings]
  {:top (binary-pair (first image-strings)) 
   :right (binary-pair (apply str (map #(last %) image-strings))) 
   :bottom (binary-pair (s/reverse (last image-strings))) 
   :left (binary-pair (s/reverse (apply str (map #(first %) image-strings))))}) 

(defn parse-core
  [image-strings]
  (mapv #(vec (subs % 1 (dec (count %)))) (subvec (vec image-strings) 1 (dec (count image-strings)))))

(defn parse-tile
  [input]
  (let [tile-strings (s/split input #"\n")]
    {:id (parse-id (first tile-strings)) 
     :borders (parse-borders (rest tile-strings)) 
     :core (parse-core (rest tile-strings))}))

(defn get-match
  [id1 border1 tile-list]
  (:id (first (filter (fn [tile2] 
                        (some true? (for [border2 (vals (:borders tile2))
                                          :when (not= id1 (:id tile2))]
                                      (or (= border1 border2) (= border1 (reverse border2))))))
                      tile-list))))

(defn matching-sides
  [tile tile-list]
  (into {} (for [[side border] (:borders tile)]
             [side [border (get-match (:id tile) border tile-list)]])))

(defn add-neighbours
  [tile-list]
  (map (fn [tile] (assoc tile :borders (matching-sides tile tile-list))) tile-list))
  
(defn get-top-left-corner
  [tiles]
  (reduce #(if (and (nil? (last (:top (:borders %2)))) (nil? (last (:left (:borders %2)))))
             (reduced %2)
             nil) nil tiles))

(defn get-neighbour-id
  [side tile]
  (last (side (:borders tile))))

(defn get-tile
  [tiles id]
  (first (filter #(= (:id %) id) tiles)))

(defn get-side-border
  [tile id]
  (first (second (first (filter (fn [[k v]] (= id (last v))) (:borders tile))))))

(defn get-side-name
  [tile id]
  (first (first (filter (fn [[k v]] (= id (last v))) (:borders tile)))))

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

(defn transform-to-match
  "Transform t2 to match the specified side of t1"
  [t1side t1 t2]
  (let [id1 (:id t1)
        t2side (get-side-name t2 id1)
        t1border (first (t1side (:borders t1)))
        t2border (get-side-border t2 id1)
        transform-list (case t1side
                         :right (case t2side
                                  :left []
                                  :top [[:rotate :left]]
                                  :right [[:rotate :left] [:rotate :left]]
                                  :bottom [[:rotate :right]])
                         :bottom (case t2side
                                   :left [[:rotate :right]]
                                   :top []
                                   :right [[:rotate :left]]
                                   :bottom [[:rotate :right] [:rotate :right]]
                                   (println "oop")))]
    (if (not= t1border t2border)
;    (do (println "t1b t2b" t1border t2border)
      transform-list
      (conj transform-list [:flip (if (= t1side :right) :vertical :horizontal)]))))

(defn print-tile
  [tile]
  (println "id:" (:id tile))
  (println "borders:")
  (pp/pprint (:borders tile))
 ; (println "core:" (:core tile))
  )

(defn print-row
  [tile-vec]
  ;(println "count tile-vec" (count tile-vec))
  ;(print-tile (nth tile-vec 3))
  (println (map :id tile-vec))
  (println (map #(get-neighbour-id :bottom %) tile-vec)))

;  (map #(print-tile %) tile-vec))
;  (pp/pprint tile-vec))

(defn create-row
  [t1 tiles]
  (let [row [t1]
        t2id (get-neighbour-id :right t1)]
    (if (nil? t2id)
      row
      (let [t2 (get-tile tiles t2id)]
        (concat row (create-row (transform-tile t2 (transform-to-match :right t1 t2)) tiles))))))

(defn add-row
  [[row-list tiles]]
  (let [start-tile-id (get-neighbour-id :bottom (first (last row-list)))]
    (if (nil? start-tile-id)
      nil
      (conj row-list (create-row (get-tile tiles start-tile-id) tiles)))))

(defn arrange-tiles
  [tiles]
  (let [corner (get-top-left-corner tiles)
        rows [(create-row corner tiles)]]
    (loop [start-tile-id (get-neighbour-id :bottom (first (last rows)))
           rows rows]
      (if (nil? start-tile-id)
        rows
        (let [prev-start (first (last rows))
              start-tile (get-tile tiles start-tile-id)
              transform-list (transform-to-match :bottom prev-start start-tile)
              transformed-start (transform-tile start-tile transform-list)
              new-row (create-row transformed-start tiles)
              rows (conj rows new-row)]
              (println "transform list" transform-list)
          (recur (get-neighbour-id :bottom transformed-start) rows))))))

(defn check-rows
  [r1 r2]
  (filter false? (map (fn [tr1 tr2] 
                        (println "check rows:"(get-neighbour-id :bottom tr1) (:id tr2))
                        (println (get-neighbour-id :top tr2) (:id tr1))
                        (and
                         (= (get-neighbour-id :bottom tr1) (:id tr2))
                         (= (get-neighbour-id :top tr2) (:id tr1)))) r1 r2)))

(defn run-tests
  [tiles]
  (let [corner (get-top-left-corner tiles)
        row (create-row corner tiles)
        start2 (get-tile tiles (get-neighbour-id :bottom corner))
        start3 (get-tile tiles (get-neighbour-id :bottom start2))]
    (print-row row)
    (print-row (create-row start2 tiles))
    (print-row (create-row start3 tiles))))

(defn main
  "Day 20 of Advent of Code 2020: Jurassic Jigsaw
      lein run day20 filename"
  [[filename]]
  (let [tiles (map parse-tile (s/split (util/slurp-resource filename) #"\n\n"))
        tiles-with-neighbours (add-neighbours tiles)
        zoom (transform-tile (get-tile tiles-with-neighbours 2551) [[:rotate :left][:flip :horizontal]])
        bing (get-tile tiles-with-neighbours 2843)
;        corner (get-top-left-corner tiles-with-neighbours)
;        row1 (create-row corner tiles-with-neighbours)
;        r2t1 (get-tile tiles-with-neighbours (get-neighbour-id :bottom (first row1)))
;        r2t1pt (transform-tile r2t1 (transform-to-match :bottom (first row1) r2t1))
;        row2 (create-row r2t1pt tiles-with-neighbours)
   ] ;grid (arrange-tiles tiles-with-neighbours)]
;        (print-tile zoom)
;        (print-tile bing)
;        (print-tile (transform-tile bing [[:rotate :left]]))
;        (print-tile (transform-tile bing [[:rotate :left][:rotate :left][:flip :horizontal]]))
;        (println (transform-to-match :bottom zoom bing))
    ;(pp/pprint grid)
   ; (print-tile (second (nth grid 1)))
   ; (print-tile bing)
   ; (print-tile (second (nth grid 2)))
;    (println (check-rows (nth grid 1) (nth grid 2)))
;    (println (check-rows (nth grid 5) (nth grid 6)))
       (run-tests tiles-with-neighbours)
    ));(pp/pprint grid)))
