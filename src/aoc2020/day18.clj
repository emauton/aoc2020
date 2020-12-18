(ns aoc2020.day18
  (:require [aoc2020.util :as util] 
            [clojure.string :as s]))

(defn parse
  [input]
  (filter util/not-nil?
          (map (fn [c]
                 (case c
                   \  nil
                   \* :star
                   \+ :plus
                   \( :open
                   \) :close
                   (Integer/parseInt (str c))))
               input)))

(defn compute-prefix
  [syms]
  (reduce (fn [[acc op] n]
            (case n
              :star [acc *]
              :plus [acc +]
              :open (reduced [acc op])
              :close (reduced [acc op])
              [(op acc n) +]))
          [0 +]
          syms))

(defn expr
  [syms acc]
  (let [[prefix op] (compute-prefix syms)
        remain (drop-while #(not= :open %) syms)]
    (if (empty? remain)
      prefix
      (recur (rest remain) prefix))

    ))

;(defn expr
;  [[n op & remainder]]
;  (let [op-fn {\+ + \* *}]
;    (cond
;      (nil? nil) n
;      (= op \)) n 
;      (= n \() (recur (rest symbols))
;      :else ((op-fn op) n (recur remainder)))))

(defn main
  "Day 18 of Advent of Code 2020: Operation Order
      lein run day18 filename"
  [[filename]]
  (let [test-str "2 * 3 + 8 * (4 * 5)"
        parsed (parse test-str)]
    (println "parse" test-str)
    (println "compute" (compute-prefix parsed))))
