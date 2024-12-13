(ns  aoc2024.d10
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]))


(def test-topographic-map (str/split (slurp "resources/aoc2024/d10_t") #"\n"))
(def topographic-map (str/split (slurp "resources/aoc2024/d10_1") #"\n"))

(defn find-zeros [m]
  (let [c (count m)]
    (loop [i 0 j 0 r []]
      (cond 
        (= j c) r
        (= i c) (recur 0 (inc j) r)
        (= 0 (get-in m [i j])) (recur (inc i) j (conj r [i j]))
        :else (recur (inc i) j r)))))

(defn find-next [m [i j]]
  (let [next-score (get-in m [i j])]
    (if (nil? next-score) 
      nil
      (for [c [[-1 0] [1 0] [0 -1] [0 1]]
                :let [[x y] c]
                :when (= (inc next-score) (get-in m [(+ i x) (+ j y)]))] [(+ i x) (+ j y)]))))

(defn find-trailheads 
  ([m] (for [i (find-zeros m)] (find-trailheads m i)))
  ([m [i j]] 
   (let [n (find-next m [i j])]
     (cond 
       (and (or (empty? n)(nil? n)) (= (get-in m [i j]) 9)) [[i j]] 
       (or (empty? n)(nil? n)) []
       :else (apply concat (for [i n] (find-trailheads m i)))))))

(assert (= 36 (->> test-topographic-map
  (mapv vec)
  (mapv #(mapv (fn [x] (- (int x) 48)) %))
  find-trailheads
  (mapv set)
  (mapv count)
  (apply +))))

(assert (= 816 (->> topographic-map
  (mapv vec)
  (mapv #(mapv (fn [x] (- (int x) 48)) %))
  find-trailheads
  (mapv set)
  (mapv count)
  (apply +))))


(assert (= 81 (->> test-topographic-map
  (mapv vec)
  (mapv #(mapv (fn [x] (- (int x) 48)) %))
  find-trailheads
  (mapv count)
  (apply +))))

(assert (= 1960 (->> topographic-map
  (mapv vec)
  (mapv #(mapv (fn [x] (- (int x) 48)) %))
  find-trailheads
  (mapv count)
  (apply +))))
