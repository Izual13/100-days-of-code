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
  (let[f (cond
           (= b "X") 0
           (= b "Y") 3
           :else 6)
       
       b (cond
           (= f 3) a
           (and (= f 0) (= a "A")) "C"
           (and (= f 0) (= a "B")) "A"
           (and (= f 0) (= a "C")) "B"
           (and (= f 6) (= a "A")) "B"
           (and (= f 6) (= a "B")) "C"
           (and (= f 6) (= a "C")) "A"
           :else b)
       
       l (cond
           (= b "A") 1
           (= b "B") 2
           :else 3)
       ] (+ f l)))

(assert (= 12 (apply + (map calc2 test-rounds))))

(assert (= 9541 (apply + (map calc2 rounds))))