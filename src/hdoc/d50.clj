(ns  hdoc.d50
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]))

;Given an array of integers in which two elements appear exactly once and all other elements appear exactly twice, find the two elements that appear only once.
;For example, given the array [2, 4, 6, 8, 10, 2, 6, 10], return 4 and 8. The order does not matter.
;Follow-up: Can you do this in linear time and constant space?

(defn find-higher-bit [n]
  (loop [bit 31] 
    (if (>= n (bit-shift-left 1 bit)) bit
      (recur (dec bit)))))

(defn find-numbers [n]
  (let [xor (apply bit-xor n)
        higher-bit (bit-shift-left 1 (find-higher-bit xor))]
    (loop [n (seq n) x 0 y 0]
      (cond
        (empty? n) [x y]
        (= 0 (bit-and higher-bit (first n))) (recur (next n) (bit-xor x (first n)) y)
        :else (recur (next n) x (bit-xor y (first n)))))))


(assert (= [4 8] (find-numbers [2, 4, 6, 8, 10, 2, 6, 10])))
(assert (= [5 7] (find-numbers [2, 5, 6, 7, 10, 2, 6, 10])))


