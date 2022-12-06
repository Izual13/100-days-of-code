(ns aoc2022.d6
  (:require [clojure.string :as str]))

(def test-input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def input (slurp "resources/aoc2022/day6_1"))

(defn find-marker [s c] 
  (loop [b 0] (let [min-c (min b (- (count s) c))]
                (if (or (< b min-c) (= c (count (frequencies (subs s b (+ c min-c))))))
                  (+ b c)
                  (recur (inc b))))))

(assert (= 7 (find-marker test-input 4)))
(assert (= 1080 (find-marker input 4)))

(assert (= 19 (find-marker test-input 14)))
(assert (= 3645 (find-marker input 14)))