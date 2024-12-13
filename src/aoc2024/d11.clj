(ns  aoc2024.d11
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]))

(def test-arrangement (str/split (slurp "resources/aoc2024/d11_t") #" "))
(def arrangement (str/split (slurp "resources/aoc2024/d11_1") #" "))

(defn split [n]
  (let [n (str n)
        h (/ (count n) 2)] [(Long/parseLong (subs n 0 h)) (Long/parseLong (subs n h))]))

(defn transform [m]
  (loop [m m r {}]
    (if (empty? m) 
      r
      (recur (dissoc m (first (first m))) (let [[k v] (first m)]
                        (cond 
                           (= 0 k) (assoc r 1 (+ (get r 1 0) v))
                           (= 0 (mod (count (str k)) 2)) (let [[p1 p2] (split k)] (if (= p1 p2)
                                                                                    (assoc r p1 (+ (get r p1 0) (* 2 v)))
                                                                                    (assoc r 
                                                                                    p1 (+ (get r p1 0) v)
                                                                                    p2 (+ (get r p2 0) v))))
                          :else (assoc r (* 2024 k) (+ (get r (* 2024 k) 0) v))))))))


(defn transforms [max-i m] 
  (loop [i 0 r (apply merge {} (mapv (fn [n] {n 1}) m))]
    (if (= i max-i) 
      r
      (recur (inc i) (transform r)))))

(assert (= 55312 (->>
  test-arrangement
  (mapv Long/parseLong)
  (transforms 25)
  (mapv second)
  (apply +))))

(assert (= 197357 (->>
  arrangement
  (mapv Long/parseLong)
  (transforms 25)
  (mapv second)
  (apply +))))

(assert (= 234568186890978 (->>
  arrangement
  (mapv Long/parseLong)
  (transforms 75)
  (mapv second)
  (apply +))))
