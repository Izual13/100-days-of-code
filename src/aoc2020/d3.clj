(ns  aoc2020.d3
  (:require [clojure.string :as str]))

(defn parse-password [s]
  (let [[_ a b c d] (re-matches #"(\d*)-(\d*) (.): (.*)" s)]
    {:indicates [(Integer/parseInt a) (Integer/parseInt b)] :l (get c 0) :p d}))

(def input (clojure.string/split (slurp "resources/aoc2020/day3_1") #"\n"))
(def test-input (clojure.string/split (slurp "resources/aoc2020/day3_t") #"\n"))

(defn parse-input [r] (clojure.string/split r #""))

(defn count-trees [right down m] 
  (loop [i 0 j 0 r 0] 
    (if (>= j (count m))
      r
      (let [new-i (mod (+ right i) (count (m 0)))
            e (get-in m [j i])]
        (recur new-i (+ down j) (if (= "#" e) (inc r) r))))))

(assert (= 7 (->> test-input
               (map parse-input)
               vec
               (count-trees 3 1))))

(assert (= 176 (->> input
                 (map parse-input)
                 vec
                 (count-trees 3 1)))) 


(defn multiple-count-trees [m] 
  (reduce * [(count-trees 1 1 m) (count-trees 3 1 m) (count-trees 5 1 m) (count-trees 7 1 m) (count-trees 1 2 m)]))

(assert (= 336 (->> test-input
                 (map parse-input)
                 vec
                 multiple-count-trees)))

(assert (= 5872458240 (->> input
                        (map parse-input)
                        vec
                        multiple-count-trees)))


