(ns aoc2020.day20c
  (:require [aoc2020.util :as util] 
            [clojure.pprint :as pp]
            [clojure.string :as s]))

(defn binary-matcher
  [edge]
  (let [bin (s/join "" (map {\# \1 \. \0} edge))]
    [(Integer/parseInt bin 2)
     (Integer/parseInt (s/reverse bin) 2)]))

(defn match?
  [[a1 a2] [b1]]
  (or (= a1 b1) (= a2 b1)))

(defn parse
  [acc tile-string]
  (let [[title text] (s/split tile-string #":\n")
        [_ id] (s/split title #" ")
        id (Integer/parseInt id)
        rows (s/split-lines text)
        image (mapv (fn [row] (subs row 1 (dec (count row))))
                    (subvec rows 1 (dec (count rows))))
        edges [(first rows)                  ; top
               (apply str (map last rows))   ; right
               (last rows)                   ; bottom
               (apply str (map first rows))] ; left
        matchers (map binary-matcher edges)]
    {:id id
     :image core
     :edges matchers}))

(defn neighbours
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

(defn main
  "Day 20 of Advent of Code 2020: Jurassic Jigsaw
      lein run day20 <filename>"
  [[filename]]
  (let [tiles (reduce parse {} (s/split (util/slurp-resource filename) #"\n\n"))]
    (pp/pprint tiles)))
