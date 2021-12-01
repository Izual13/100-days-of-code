(ns  aoc2021.d1
  (:require [clojure.string :as str]))

(def input-from-file
  (map #(Integer/parseInt %)
       (str/split-lines
        (slurp "resources/aoc2021/day1_1"))))

(defn count-measurement-increases
  ([input] (count-measurement-increases (first input) (rest input) 0))
  ([element input result]
   (if (empty? input)  result
       (let [second (first input)]
         (if (< element second) 
           (count-measurement-increases second (rest input) (inc result))
           (count-measurement-increases second (rest input) result))))))

(count-measurement-increases input-from-file)

