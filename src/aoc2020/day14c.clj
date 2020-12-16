(ns aoc2020.day14c
  (:require [aoc2020.util :as util]
            [clojure.string :as s]))

(defn pad36
  [v]
  (vec (concat v (take (- 36 (count v)) (repeat 0)))))

(defn str->36b
  [s]
  (->> (reverse s)
       (mapv #(case %
                \1 1
                \0 0
                nil))
       (pad36)))

(defn mask->bits
  [mask]
  (str->36b mask))

(defn number->bits
  [n]
  (str->36b (Integer/toBinaryString n)))

(defn bits->number
  [bits]
  (let [bitstr (s/join "" (reverse bits))]
    (Long/parseLong bitstr 2)))

(defn apply-mask
  [mask n]
  (mapv (fn [[m v]] (case m
                      1 1
                      0 0
                      v))
        (map vector mask n)))

(defn apply-mask-floating
  [mask addr]
  (reduce-kv (fn [acc index v]
                (case v
                  0 acc
                  1 (map #(assoc % index 1) acc)
                  (apply concat (map (fn [bits]
                                       [(assoc bits index 0)
                                        (assoc bits index 1)])
                                     acc))))
              [addr] mask))

(defn execute1-step
  [line {:keys [mask mem]}]
  (if-let [[_ m] (re-matches #"mask = (.*)" line)]
    {:mask (mask->bits m) :mem mem}
    (let [[_ index value] (re-matches #"mem\[(.*)\] = (.*)" line)
          index (Integer/parseInt index)
          value (Long/parseLong value)]
      {:mask mask
       :mem (->> value
                 (number->bits)
                 (apply-mask mask)
                 (bits->number)
                 (assoc mem index))})))

(defn execute2-step
  [line {:keys [mask mem]}]
  (if-let [[_ m] (re-matches #"mask = (.*)" line)]
    {:mask (mask->bits m) :mem mem}
    (let [[_ index value] (re-matches #"mem\[(.*)\] = (.*)" line)
          index (Integer/parseInt index)
          value (Long/parseLong value)]
      {:mask mask
       :mem (->> index
                 (number->bits)
                 (apply-mask-floating mask)
                 (reduce (fn [acc addr]
                           (assoc acc addr value))
                         mem))})))

(defn execute
  [input state step-fn]
  (if (empty? input)
    (:mem state)
    (recur (rest input)
           (step-fn (first input) state)
           step-fn)))

(defn main
  "Day 14 of Advent of Code 2020: Docking Data
      lein run day14 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [input (util/read-lines filename)
        mem1 (execute input {:mask nil :mem (sorted-map)} execute1-step)
        mem2 (execute input {:mask nil :mem (sorted-map)} execute2-step)]
    (println "Part 1:" (apply + (vals mem1)))
    (println "Part 2:" (apply + (vals mem2)))))
