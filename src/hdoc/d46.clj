(ns  hdoc.d46
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]))

;Given an array of numbers, find the length of the longest increasing subsequence in the array. The subsequence does not necessarily have to be contiguous.
;For example, given the array [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15], the longest increasing subsequence has length 6: it is 0, 2, 6, 9, 11, 15.
;We will be sending the solution tomorrow, along with tomorrow's question. As always, feel free to shoot us an email if there's anything we can help with.

(defn find-max-subseqence [n]
  (let [count (count n)]
    (loop [i 0 j 0 result [1]]
      (cond 
        (= i count) (apply max result)
        (= i j) (recur (inc i) 0 (conj result 1))
        :else (recur i (inc j) (if (> (get n i) (get n j)) (assoc result i (max (get result i) (inc (get result j)))) result))))))

(assert (= 6 (find-max-subseqence [0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15])))
(assert (= 3 (find-max-subseqence [0 8 4 6])))