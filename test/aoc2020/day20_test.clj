(ns aoc2020.day20-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [aoc2020.day20part2 :refer :all]
            [aoc2020.util :as util]))

(def test-tile
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

(def parsed (parse-tile test-tile))

(deftest parsing
  (testing "ID is correct"
    (is (= 2473 (:id parsed))))
  (testing "Borders are correct"
    (is (= (:borders parsed)
           {:top [363 858]
            :right [639 1017]
            :bottom [702 501]
            :left [296 82]})))
  (testing "Core is correct"
    (is (= (:core parsed))
        [[\. \. \. \. \. \. \# \.]
         [\# \# \# \# \. \# \# \.]
         [\. \. \. \# \# \# \. \.]
         [\. \. \# \. \. \. \. \.]
         [\. \. \. \. \. \. \. \.]
         [\# \# \. \# \# \. \. \.]
         [\. \. \. \. \. \. \. \#]
         [\. \# \# \. \. \. \# \.]])))

(def all-parsed
  (map parse-tile
       (s/split (util/slurp-resource "mags/input-20") #"\n\n")))

(def all-neighbours
  (add-neighbours all-parsed))

(def parsed-with-neighbours (get-tile all-neighbours 2473))

(deftest borders
  (testing "We find the correct match for each border of 2473"
    (is (= (matching-sides parsed all-parsed)
           {:top    [[363 858]  2851]
            :right  [[639 1017] 2333]
            :bottom [[702 501]  3041]
            :left   [[296 82]   1171]})))
  (testing "Neighbours of 2473 are picked up correctly"
    (is (= (:borders (get-tile all-neighbours 2473))
           {:top    [[363 858]  2851]
            :right  [[639 1017] 2333]
            :bottom [[702 501]  3041]
            :left   [[296 82]   1171]}))))

(deftest transforms
  (testing "core rotate right"
    (is (= (transform-core (:core parsed) :rotate :right)
           [[\. \. \# \. \. \. \# \.]
            [\# \. \# \. \. \. \# \.]
            [\# \. \. \. \# \. \# \.]
            [\. \. \# \. \. \# \# \.]
            [\. \. \# \. \. \# \. \.]
            [\. \. \. \. \. \# \# \.]
            [\# \. \. \. \. \. \# \#]
            [\. \# \. \. \. \. \. \.]])))
  (testing "core rotate left"
    (is (= (transform-core (:core parsed) :rotate :left)
           [[\. \. \. \. \. \. \# \.]
            [\# \# \. \. \. \. \. \#]
            [\. \# \# \. \. \. \. \.]
            [\. \. \# \. \. \# \. \.]
            [\. \# \# \. \. \# \. \.]
            [\. \# \. \# \. \. \. \#]
            [\. \# \. \. \. \# \. \#]
            [\. \# \. \. \. \# \. \.]])))
  (testing "core flip horizontal"
    (is (= (transform-core (:core parsed) :flip :horizontal)
           [[\. \# \. \. \. \. \. \.]
            [\. \# \# \. \# \# \# \#]
            [\. \. \# \# \# \. \. \.]
            [\. \. \. \. \. \# \. \.]
            [\. \. \. \. \. \. \. \.]
            [\. \. \. \# \# \. \# \#]
            [\# \. \. \. \. \. \. \.]
            [\. \# \. \. \. \# \# \.]])))
  (testing "core flip vertical"
    (is (= (transform-core (:core parsed) :flip :vertical)
           [[\. \# \# \. \. \. \# \.]
            [\. \. \. \. \. \. \. \#]
            [\# \# \. \# \# \. \. \.]
            [\. \. \. \. \. \. \. \.]
            [\. \. \# \. \. \. \. \.]
            [\. \. \. \# \# \# \. \.]
            [\# \# \# \# \. \# \# \.]
            [\. \. \. \. \. \. \# \.]])))
  (testing "core rotate equivalence"
    (is (= (transform-core (:core parsed) :rotate :left)
           (-> (transform-core (:core parsed) :rotate :right)
               (transform-core :rotate :right)
               (transform-core :rotate :right)))))
  (testing "border rotate right"
    (is (= (transform-borders (:borders parsed-with-neighbours) :rotate :right)
           {:top    [[296 82]   1171]
            :right  [[363 858]  2851]
            :bottom [[639 1017] 2333]
            :left   [[702 501]  3041]})))
  (testing "border rotate left"
    (is (= (transform-borders (:borders parsed-with-neighbours) :rotate :left)
           {:top    [[639 1017] 2333]
            :right  [[702 501]  3041]
            :bottom [[296 82]   1171]
            :left   [[363 858]  2851]})))
  (testing "border flip horizontal"
    (is (= (transform-borders (:borders parsed-with-neighbours) :flip :horizontal)
           {:top    [[858 363]  2851]
            :right  [[82 296]   1171]
            :bottom [[501 702]  3041]
            :left   [[1017 639] 2333]})))
  (testing "border flip vertical"
    (is (= (transform-borders (:borders parsed-with-neighbours) :flip :vertical)
           {:top    [[501 702]  3041]
            :right  [[1017 639] 2333]
            :bottom [[858 363]  2851]
            :left   [[82 296]   1171]})))
  )
