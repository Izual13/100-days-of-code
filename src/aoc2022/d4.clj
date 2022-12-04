(ns aoc2022.d4
  (:require [clojure.string :as str]))

(defn parse-range [s]
  (let [[_ a b c d] (re-matches #"(\d*)-(\d*),(\d*)-(\d*)" s)]
    [(Integer/parseInt a) (Integer/parseInt b) (Integer/parseInt c) (Integer/parseInt d)]))

(assert (= [11 22 66 88] (parse-range "11-22,66-88")))

(defn check-ranges [[a b c d]] 
  (cond
    (and (<= a c) (>= b d)) true
    (and (>= a c) (<= b d)) true
    :else false))

(assert (= true (check-ranges [6 6 4 6])))
(assert (= true (check-ranges [4 6 6 6])))
(assert (= false (check-ranges [2 4 6 8])))
(assert (= false (check-ranges [6 8 2 4])))

(defn check-overlap [[a b c d]] 
  (cond
    (and (<= a c) (>= b c)) true
    (and (<= a d) (>= b d)) true
    (and (<= c a) (>= d a)) true
    (and (<= c b) (>= d b)) true
    :else false))

(assert (= true (check-overlap [5 7 7 9])))

(def test-input (clojure.string/split (slurp "resources/aoc2022/day4_t") #"\n"))
(def input (clojure.string/split (slurp "resources/aoc2022/day4_1") #"\n"))

(assert (= 2 (->> test-input
  (map parse-range)
  (map check-ranges)
  (filter #(= true %))
  (count))))

(assert (= 4 (->> test-input
  (map parse-range)
  (map check-overlap)
  (filter #(= true %))
  (count))))


(assert (= 485 (->> input
  (map parse-range)
  (map check-ranges)
  (filter #(= true %))
  (count))))

(assert (= 857 (->> input
  (map parse-range)
  (map check-overlap)
  (filter #(= true %))
  (count))))
