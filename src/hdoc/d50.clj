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

(find-higher-bit 10)
(find-higher-bit 8)
(bit-shift-left 1 3)



(defn find-numbers [n]
  (let [xor (apply bit-xor n)
        xor2 (apply bit-xor (map #(bit-shift-left % 1) n))
        xor3 (bit-xor xor (bit-and xor2 (- (bit-shift-left 1 (inc (find-higher-bit xor))) 1)))] [xor3 (bit-xor xor xor3)]))

(bit-shift-right 8 1)
(bit-shift-right 24 1)


(find-numbers [2, 4, 6, 8, 10, 2, 6, 10])
(find-numbers [2, 5, 6, 11, 10, 2, 6, 10])

(assert (= 9 (queensAttack 4 0 4 4 [])))


