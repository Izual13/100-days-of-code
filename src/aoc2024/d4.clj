(ns  aoc2024.d4
  (:require [clojure.string :as str]))

(def test-letters (clojure.string/split (slurp "resources/aoc2024/d4_t") #"\n"))
(def letters (clojure.string/split (slurp "resources/aoc2024/d4_1") #"\n"))


(def positions [[-1 -1][-1 0][-1 1]
                [0  -1]      [0  1]
                [1  -1][1  0][1  1]])

(defn check [l [i j]]
  (loop [p positions r 0]
    (if (empty? p) r
      (let [[x y] (first p)]
        (if (and 
              (= \M (get-in l [(+ i x) (+ j y)]))
              (= \A (get-in l [(+ i x x) (+ j y y)]))
              (= \S (get-in l [(+ i x x x) (+ j y y y)])))
          (recur (next p) (inc r))
          (recur (next p) r))))))

(defn calculate [l] 
  (let [c (count l)]
    (loop [i 0 j 0 r 0]
      (cond 
        (= j c) r
        (= i c) (recur 0 (inc j) r)
        (= \X (get-in l [i j])) (recur (inc i) j (+ (check l [i j]) r))
        :else (recur (inc i) j r)))))

(assert (= 18 (->> test-letters
  (mapv vec)
  calculate)))


(assert (= 2549 (->> letters
  (mapv vec)
  calculate)))


(defn check-2 [l [i j]]
  (cond (and 
            (= \M (get-in l [(- i 1) (+ j 1)]))
            (= \M (get-in l [(+ i 1) (+ j 1)]))
            (= \S (get-in l [(- i 1) (- j 1)]))
	        (= \S (get-in l [(+ i 1) (- j 1)]))) 1
        (and 
            (= \S (get-in l [(- i 1) (+ j 1)]))
            (= \M (get-in l [(+ i 1) (+ j 1)]))
            (= \S (get-in l [(- i 1) (- j 1)]))
            (= \M (get-in l [(+ i 1) (- j 1)]))) 1
    	(and 
            (= \S (get-in l [(- i 1) (+ j 1)]))
            (= \S (get-in l [(+ i 1) (+ j 1)]))
            (= \M (get-in l [(- i 1) (- j 1)]))
            (= \M (get-in l [(+ i 1) (- j 1)]))) 1
        (and 
            (= \M (get-in l [(- i 1) (+ j 1)]))
            (= \S (get-in l [(+ i 1) (+ j 1)]))
            (= \M (get-in l [(- i 1) (- j 1)]))
            (= \S (get-in l [(+ i 1) (- j 1)]))) 1
    :else 0))


(defn calculate-2 [l] 
  (let [c (count l)]
    (loop [i 0 j 0 r 0]
      (cond 
        (= j c) r
        (= i c) (recur 0 (inc j) r)
        (= \A (get-in l [i j])) (recur (inc i) j (+ (check-2 l [i j]) r))
        :else (recur (inc i) j r)))))

(assert (= 9 (->> test-letters
  (mapv vec)
  calculate-2)))


(assert (= 2003 (->> letters
  (mapv vec)
  calculate-2)))
