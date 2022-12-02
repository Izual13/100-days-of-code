(ns  aoc2022.d2
  (:require [clojure.string :as str]))

(defn parse-round [s]
  (let [[_ a b] (re-matches #"(.) (.)" s)]
    [a b]))


(def test-rounds (map parse-round (clojure.string/split (slurp "resources/aoc2022/day2_t") #"\n")))

(def rounds (map parse-round (clojure.string/split (slurp "resources/aoc2022/day2_1") #"\n")))

;;Rock (A, X) Paper(B, Y) Scissors(C, Z)

(defn calc [[a b]] 
  (let[b (cond
           (= b "X") "A"
           (= b "Y") "B"
           (= b "Z") "C"
           :else b)
       f (cond
           (= b "A") 1
           (= b "B") 2
           :else 3)
       l (cond
           (= a b) 3
           (and (= a "C") (= b "A")) 6
           (and (= a "A") (= b "B")) 6
           (and (= a "B") (= b "C")) 6
           :else 0)
       ] (+ f l)))

(assert (= 15 (apply + (map calc test-rounds))))

(assert (= 10595 (apply + (map calc rounds))))

;;Rock (A, X) Paper(B, Y) Scissors(C, Z)
(defn calc2 [[a b]] 
  (calc [a (cond
           (= b "Y") a
           (and (= b "X") (= a "A")) "C"
           (and (= b "X") (= a "B")) "A"
           (and (= b "X") (= a "C")) "B"
           (and (= b "Z") (= a "A")) "B"
           (and (= b "Z") (= a "B")) "C"
           (and (= b "Z") (= a "C")) "A"
           :else b)]))

(assert (= 12 (apply + (map calc2 test-rounds))))

(assert (= 9541 (apply + (map calc2 rounds))))