(ns  aoc2022.d1
  (:require [clojure.string :as str]))

(def elfs (clojure.string/split (slurp "resources/aoc2022/day1_1") #"\n\n"))

(defn calorie-counting [elf] (apply + (map #(Integer/parseInt %)(str/split elf #"\n"))))

(assert (= 10 (calorie-counting "1\n2\n3\n4")))

(assert (= 67633 (->> elfs
                   (map #(calorie-counting %))
                   (apply max))))

(assert (= 199628 (->> elfs
                    (map #(calorie-counting %))
                    (sort)
                    (take-last 3)
                    (apply +))))