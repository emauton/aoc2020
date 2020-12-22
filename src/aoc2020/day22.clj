(ns aoc2020.day22
  (:require [aoc2020.util :as util]
            [clojure.string :as s]
            [clojure.core.memoize :as m]))

(defn parse
  [input]
  (let [chunks (s/split input #"\n\n")
        [a b] (map s/split-lines chunks)]
    {:one (mapv #(Integer/parseInt %) (rest a))
     :two (mapv #(Integer/parseInt %) (rest b))
     :history []}))

;; Part 1

(defn round
  [{:keys [one two history]}]
  (let [c1 (first one)
        c2 (first two)
        history (conj history [one two])]
    (if (> c1 c2)
      {:one (conj (vec (rest one)) c1 c2)
       :two (vec (rest two))
       :history history}
      {:one (vec (rest one))
       :two (conj (vec (rest two)) c2 c1)
       :history history})))

(defn game
  [{:keys [one two history] :as decks}]
  (cond
    (some #(= [one two] %) history) one
    (empty? one) two
    (empty? two) one
    :else (recur (round decks))))

(defn score
  [deck]
  (reduce-kv (fn [acc index card]
               (+ acc (* (inc index) card)))
             0
             (vec (reverse deck))))

;; Part 2

(declare recursive-game)

(defn recursive-round
  [{:keys [one two history] :as decks}]
  (let [c1 (first one)
        c2 (first two)
        history (conj history [one two])]
    (if (and (<= c1 (count (rest one)))
             (<= c2 (count (rest two))))
      (let [result (recursive-game {:one (vec (take c1 (rest one)))
                                    :two (vec (take c2 (rest two)))
                                    :history []})]
        (if (= :one (:winner result))
          {:one (conj (vec (rest one)) c1 c2)
           :two (vec (rest two))
           :history history}
          {:one (vec (rest one))
           :two (conj (vec (rest two)) c2 c1)
           :history history}))
      (if (> c1 c2)
        {:one (conj (vec (rest one)) c1 c2)
         :two (vec (rest two))
         :history history}
        {:one (vec (rest one))
         :two (conj (vec (rest two)) c2 c1)
         :history history}))))

(def recursive-game
  (m/memo
    (fn [{:keys [one two history] :as decks}]
      (cond
        (some #(= [one two] %) history) {:winner :one :deck one}
        (empty? one) {:winner :two :deck two}
        (empty? two) {:winner :one :deck one}
        :else (recur (recursive-round decks))))))

(defn main
  "Day 22 of Advent of Code 2020: Crab Combat
      lein run day22 filename"
  [[filename]]
  (let [decks (parse (util/slurp-resource filename))
        result (game decks)
        direct-score (score result)
        recursive-result (recursive-game decks)
        recursive-score (score (:deck recursive-result))]
    (println "Score:" direct-score)
    (println "Recursive score:" recursive-score)))
