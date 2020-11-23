(defproject aoc2020 "0.1.0-SNAPSHOT"
  :description "Advent of Code 2020"
  :url "https://github.com/emauton/aoc2020"
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot aoc2020.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
