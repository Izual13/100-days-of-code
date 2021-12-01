(ns  aoc.d1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (map #(Integer/parseInt %)
       (str/split-lines
        (slurp "resources/aoc2020/input1.txt"))))

(set
 (for [x input 
       y input
       z input
       :when (= 2020 (+ x y z))]
   (* x y z)))