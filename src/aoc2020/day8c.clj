(ns aoc2020.day8c
  (:require [aoc2020.util :as util]
            [clojure.pprint :as pp]
            [clojure.string :as s]))

(defn new-program
  "Keep accumulator and program counter history in execution state"
  [text]
  {:pc 0 :text text :acc [0] :history []})

(defn execute-step
  [{:keys [pc text acc history]}]
  (let [[op n] (nth text pc)
        current (last acc)
        [new-pc new-acc] (cond
                           (= op "nop") [(inc pc) acc]
                           (= op "acc") [(inc pc) (conj acc (+ current n))]
                           (= op "jmp") [(+ pc n) acc])]
    {:pc new-pc :text text :acc new-acc :history (conj history pc)}))

(defn looped?
  [{:keys [history]}]
  (> (count history) (count (set history))))

(defn halted?
  [{:keys [pc text]}]
  (> pc (dec (count text))))

(defn execute-program
  [state]
  (let [next-state (execute-step state)]
    (cond
      (looped? next-state) next-state
      (halted? next-state) next-state
      :else (recur next-state))))

(defn loop-result
  [{:keys [acc]}]
  (first (take-last 2 acc)))

(defn halt-result
  [{:keys [acc]}]
  (last acc))

(defn parse-op
  "Parse an instruction of the form '<op> <+ or -><num>' e.g. 'nop +0'
   into [op number]"
  [line]
  (let [[op number] (s/split line #" ")]
    [op (Integer/parseInt number)]))

(defn fix-attempts
  "Generate all possible 'fixes' where we exchange a nop for a jmp or vice versa"
  [instrs]
  (let [enum-instrs (map-indexed vector instrs)]
    (reduce (fn [acc [i instr]]
              (cond
                (= "nop" (first instr)) (conj acc (assoc instrs i ["jmp" (last instr)]))
                (= "jmp" (first instr)) (conj acc (assoc instrs i ["nop" (last instr)]))
                :else acc))
            [] enum-instrs)))

(defn try-halt
  [instrs]
  (let [program (new-program instrs)
        terminal (execute-program program)]
    (when (halted? terminal) (halt-result terminal))))

(defn search-halt
  [instrs]
  (let [tries (fix-attempts instrs)]
    (first (drop-while nil? (map try-halt tries)))))

(defn main
  "Day 8 of Advent of Code 2020: Handheld Halting
      lein run day7 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [instrs (mapv parse-op (util/read-lines filename))
        computer (new-program instrs)
        terminal (execute-program computer)]
    (println "Loop result:" (loop-result terminal))
    (println "Halt result:" (search-halt instrs))))
