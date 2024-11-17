(ns  aoc2020.d5
  (:require [clojure.string :as str]))


(def test-binary "FBFBBFFRLR")
(def binary (clojure.string/split (slurp "resources/aoc2020/day5_1") #"\n"))


(assert (= 874 (->> binary
  (map #(let [characters %
              r (loop [v (vec characters) s1 0 e1 127 s2 0 e2 7]
                  (if (empty? v)
                    (+ s2 (* 8 s1))
                    (let [f (first v)
                      h1 (int (/ (+ s1 e1) 2))
                      h2 (int (/ (+ s2 e2) 2))] 
                      (cond 
                        (= f \F) (recur (next v) s1 h1 s2 e2) 
                        (= f \B) (recur (next v) (inc h1) e1 s2 e2) 
                        (= f \L) (recur (next v) s1 e1 s2 h2) 
                        (= f \R) (recur (next v) s1 e1 (inc h2) e2)
                        :else 0))))] r))
  (apply max))))

(->> binary
  (mapv #(let [characters %
              r (loop [v (vec characters) s1 0 e1 127 s2 0 e2 7]
                  (if (empty? v)
                    [s1 s2]
                    (let [f (first v)
                      h1 (int (/ (+ s1 e1) 2))
                      h2 (int (/ (+ s2 e2) 2))] 
                      (cond 
                        (= f \F) (recur (next v) s1 h1 s2 e2) 
                        (= f \B) (recur (next v) (inc h1) e1 s2 e2) 
                        (= f \L) (recur (next v) s1 e1 s2 h2) 
                        (= f \R) (recur (next v) s1 e1 (inc h2) e2)
                        :else 0))))] r))
  (group-by first)
  (filter #(= 7 (count (second %)))))

(assert (= 594 (+ 2 (* 8 74))))

