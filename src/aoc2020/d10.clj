(ns  aoc2020.d10
  (:require [clojure.string :as str]))


(def test-joltages (str/split (slurp "resources/aoc2020/day10_t") #"\n"))
(def joltages (str/split (slurp "resources/aoc2020/day10_1") #"\n"))


(defn calculate [j]
  (loop [j j p 0 i1 0 i3 0]
    (if (empty? j) 
      (* i1 (inc i3))
      (let [f (first j)]
        (cond 
          (= f (inc p)) (recur (next j) f (inc i1) i3)
          (= f (+ 3 p)) (recur (next j) f i1 (inc i3))
          :else (recur (next j) f i1 i3))))))

(assert (= 35 (->> test-joltages
  (map Integer/parseInt)
  sort
  calculate)))


(assert (= 2201 (->> joltages
  (map Integer/parseInt)
  sort
  calculate)))

(defn calculate2 
  ([j] (calculate2 (set j) 0 {}))
  ([j p cache] (cond 
                 (empty? j) [1 cache]
                 (contains? cache j) [(cache j) cache]
                 :else
                 (let [[c1 cache] (if (contains? j (+ p 1)) (calculate2 (disj j (+ p 1))                   (+ p 1) cache) [0 cache])
                       [c2 cache] (if (contains? j (+ p 2)) (calculate2 (disj j (+ p 2) (+ p 1))           (+ p 2) cache) [0 cache])
                       [c3 cache] (if (contains? j (+ p 3)) (calculate2 (disj j (+ p 3) (+ p 2) (+ p 1))   (+ p 3) cache) [0 cache])]
                    [(+ c1 c2 c3) (assoc cache j (+ c1 c2 c3))]))))

(assert (= 8 (->> test-joltages
  (map Integer/parseInt)
  calculate2
  first)))

(assert (= 169255295254528 (->> joltages
  (map Integer/parseInt)
  calculate2
  first)))
