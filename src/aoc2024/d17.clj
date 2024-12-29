(ns  aoc2024.d17
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]
            [clojure.math :as math])
   (:use     [utils]))

(def test-info (slurp "resources/aoc2024/d17_t"))
(def info (slurp "resources/aoc2024/d17_1"))

(defn parse-info[i]
  (let [[_ a b c p] (re-matches #"Register A: (\d+)\nRegister B: (\d+)\nRegister C: (\d+)\n\nProgram: (.*)" i)
        p (str/split p #",")]
    [(Integer/parseInt a) (Integer/parseInt b) (Integer/parseInt c) (mapv Integer/parseInt p)]))


(defn calculate [[a b c p]]
  (loop [i 0 a a b b c c r []]
    (if (= i (count p)) 
      {:a a :b b :c c :r r}
      (let [opcode (get p i)
            operand (get p (inc i))
            combo-operand (case operand 
                            0 0
                            1 1
                            2 2
                            3 3
                            4 a
                            5 b
                            6 c
                            7 nil)]
        (cond 
          
          (= 0 opcode) (recur (+ 2 i) (int (/ a (math/pow 2 combo-operand))) b c r)
          (= 1 opcode) (recur (+ 2 i) a (bit-xor b operand) c r)
          (= 2 opcode) (recur (+ 2 i) a (mod combo-operand 8) c r)
          (= 3 opcode) (if (= 0 a) (recur (+ 2 i) a b c r) (recur operand a b c r))
          (= 4 opcode) (recur (+ 2 i) a (bit-xor b c) c r)
          (= 5 opcode) (recur (+ 2 i) a b c (conj r (mod combo-operand 8)))
          (= 6 opcode) (recur (+ 2 i) a (int (/ a (math/pow 2 combo-operand))) c r)
          (= 7 opcode) (recur (+ 2 i) a b (int (/ a (math/pow 2 combo-operand))) r))))))


(calculate [0 0 9 [2 6]])
(calculate [10 0 0 [5,0,5,1,5,4]])
(calculate [2024 0 0 [0,1,5,4,3,0]])
(calculate [0 29 0 [1 7]])
(calculate [0 2024 43690 [4 0]])


(assert (= {:a 0, :b 0, :c 0, :r [4 6 3 5 6 3 5 2 1 0]} (->> test-info
  parse-info
  calculate)))


(assert (= {:a 0, :b 6, :c 3, :r [6 0 6 3 0 2 3 1 6]} (->> info
  parse-info
  calculate)))

