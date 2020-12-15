(defproject aoc2020 "0.1.0-SNAPSHOT"
  :description "Advent of Code 2020"
  :url "https://github.com/emauton/aoc2020"
  :dependencies [[org.clojure/clojure "1.10.0"] 
                 [org.clojure/core.memoize "1.0.236"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :jvm-opts ["-Xmx4g"]
  :main ^:skip-aot aoc2020.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
