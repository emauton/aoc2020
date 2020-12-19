(ns aoc2020.day19
  (:require [aoc2020.util :as util] 
            [clojure.pprint :as pp]
            [clojure.string :as s]))

(defn parse-rules
  "Parse a vector of rules into a map where rule name (as string) is key
   and value is either a character or a sequence of other rules
   the top layer of nesting represents OR, the deeper nesting represents rules
   that must be applied in sequence."  
  [rules]
  (reduce 
   (fn [acc r] 
     (let [[name rule] (s/split r #"[\:]")
           name (s/trim name)
           rule (s/trim rule)] 
       (case rule
         "\"a\"" (assoc acc name \a)
         "\"b\"" (assoc acc name \b)
         (assoc acc name (map #(s/split % #" ") (map s/trim (s/split rule #"[\|]")))))))
   {} rules))

(defn build-regex
  [rules-map name]
  (let [rules (get-in rules-map [name])]
    (if (char? rules)
      rules
      (let [ors (map (fn [r] (s/join (map (fn [n] (build-regex rules-map n)) r))) rules)
            conjd (s/join "|" ors)]
        (if (> (count ors) 1)
          (s/join ["(" conjd ")"])
          conjd)))))

(defn exact-match?
  [msg query]
  (= msg (first (re-find query msg))))

(defn main
  "Day 19 of Advent of Code 2020: Monster Messages 
      lein run day19 filename"
  [[filename]]
  (let [[rules _ msgs] (partition-by #(= "" %) (util/read-lines filename))
        parsed-rules (parse-rules rules)
        query (re-pattern (build-regex parsed-rules "0"))]
    (println "Match count:" (reduce (fn [acc m] (if (exact-match? m query) (inc acc) acc)) 0 msgs))))
