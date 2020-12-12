(ns aoc2020.day12
  (:require [aoc2020.util :as util]))

(defn new-ship
  "Usable either as a 'path' or 'waypoint' ship below"
  []
  {:facing :e
   :waypoint {:y 1 :x 10}
   :y 0
   :x 0})

;; Part 1

(defn path-move
  [{:keys [y x] :as ship} direction distance]
  (case direction
    :n (assoc ship :y (+ y distance))
    :s (assoc ship :y (- y distance))
    :e (assoc ship :x (+ x distance))
    :w (assoc ship :x (- x distance))))

(defn path-rotate
  [{:keys [facing] :as ship} direction degrees]
  (let [cyc (direction {:r (cycle [:n :e :s :w])
                        :l (cycle [:n :w :s :e])})
        steps {90 1 180 2 270 3}]
    (assoc ship :facing (nth
                          (drop-while #(not= facing %) cyc)
                          (steps degrees)))))

(defn path-apply
  [{:keys [facing] :as ship} [instr n]]
  (case instr
    "N" (path-move ship :n n)
    "S" (path-move ship :s n)
    "E" (path-move ship :e n)
    "W" (path-move ship :w n)
    "F" (path-move ship facing n)
    "L" (path-rotate ship :l n)
    "R" (path-rotate ship :r n)))

;; Part 2

(defn waypoint-move
  [{:keys [x y waypoint] :as ship} distance]
  (merge ship {:y (+ y (* distance (:y waypoint)))
               :x (+ x (* distance (:x waypoint)))}))

(defn waypoint-set
  [ship direction distance]
  (let [{:keys [y x] :as waypoint} (:waypoint ship)]
    (assoc ship :waypoint
          (case direction
            :n (assoc waypoint :y (+ y distance))
            :s (assoc waypoint :y (- y distance))
            :e (assoc waypoint :x (+ x distance))
            :w (assoc waypoint :x (- x distance))))))

(defn waypoint-rotate
  [ship direction degrees]
  (let [{:keys [y x] :as waypoint} (:waypoint ship)
        [new-y new-x] (case [direction degrees]
                        [:l 90]  [x (* -1 y)]
                        [:r 270] [x (* -1 y)]

                        [:l 180] [(* -1 y) (* -1 x)]
                        [:r 180] [(* -1 y) (* -1 x)]

                        [:l 270] [(* -1 x) y]
                        [:r 90]  [(* -1 x) y])]
    (assoc ship :waypoint {:y new-y :x new-x})))

(defn waypoint-apply
  [ship [instr n]]
  (case instr
    "N" (waypoint-set ship :n n)
    "S" (waypoint-set ship :s n)
    "E" (waypoint-set ship :e n)
    "W" (waypoint-set ship :w n)
    "F" (waypoint-move ship n)
    "L" (waypoint-rotate ship :l n)
    "R" (waypoint-rotate ship :r n)))

;; Shared

(defn manhattan
  [{:keys [y x]}]
  (int (+ (Math/abs y) (Math/abs x))))

(defn parse
  [line]
  (let [[_ instr n] (re-matches #"([NSEWLRF])(\d+)" line)]
    [instr (Integer/parseInt n)]))

(defn main
  "Day 12 of Advent of Code 2020: Rain Risk
      lein run day12 <input>
  where <input> is a filename in project resources/"
  [[filename]]
  (let [instrs (map parse (util/read-lines filename))
        path-ship (reduce path-apply (new-ship) instrs)
        waypoint-ship (reduce waypoint-apply (new-ship) instrs)]
    (println "Manhattan distance of path ship:" (manhattan path-ship))
    (println "Manhattan distance of waypoint ship:" (manhattan waypoint-ship))))
