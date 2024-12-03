(ns  aoc2024.d3
  (:require [clojure.string :as str]))

(def test_memory (clojure.string/split (slurp "resources/aoc2024/d3_t") #"\n"))
(def test_memory-2 (clojure.string/split (slurp "resources/aoc2024/d3_t2") #"\n"))
(def memory (clojure.string/split (slurp "resources/aoc2024/d3_1") #"\n"))

(defn find-all-mul [s]
  (re-seq #"mul\(\d+,\d+\)" s))

(defn calculate [s]
  (let [[_ n1 n2] (re-matches #"mul\((\d+),(\d+)\)" s)]
    (* (Integer/parseInt n1) (Integer/parseInt n2))))


(assert (= 161 (->> test_memory
  (map find-all-mul)
  flatten
  (map calculate)
  (apply +))))

(assert (= 173419328 (->> memory
  (map find-all-mul)
  flatten
  (map calculate)
  (apply +))))

(defn extended-find-all-mul [s]
  (re-seq #"mul\(\d+,\d+\)|don't\(\)|do\(\)" s))

(defn extended-calculate [s]
  (loop [s s enable true r 0]
    (if (empty? s) 
      r
      (let [f (first s)
            [_ command] (re-matches #"(mul|don't|do)\(.*" f)
            [_ n1 n2] (re-matches #"mul\((\d+),(\d+)\)" f)]
        (cond 
          (= command "do") (recur (next s) true r)
          (= command "don't") (recur (next s) false r)
          enable (recur (next s) enable (+ r (* (Integer/parseInt n1) (Integer/parseInt n2))))
          :else (recur (next s) enable r))))))

(assert (= 161 (->> test_memory
  (map extended-find-all-mul)
  flatten
  extended-calculate)))

(assert (= 48 (->> test_memory-2
  (map extended-find-all-mul)
  flatten
  extended-calculate)))

(assert (= 90669332 (->> memory
  (map extended-find-all-mul)
  flatten
  extended-calculate)))
