(ns  hdoc.d47
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]))

;The Love-Letter Mystery


(defn calc-difference [a b] 
  (Math/abs (- (int a) (int b))))

(defn theLoveLetterMystery [s]
  (let [count (count s)
        half (int (Math/floor (/ count 2)))] 
    (loop [i 0 result 0] 
      (if (= half i) 
        result
        (recur (inc i) (+ result (calc-difference (get s i) (get s (- count i 1)))))))))


(assert (= 4 (theLoveLetterMystery "abdcd")))
(assert (= 2 (theLoveLetterMystery "abc")))