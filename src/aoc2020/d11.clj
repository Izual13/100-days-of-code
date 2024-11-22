(ns  aoc2020.d11
  (:require [clojure.string :as str]))


(def test-seats (str/split (slurp "resources/aoc2020/day11_t") #"\n"))
(def test-seats2 (str/split (slurp "resources/aoc2020/day11_t2") #"\n"))
(def seats (str/split (slurp "resources/aoc2020/day11_1") #"\n"))

(def positions [[-1 -1][-1 0][-1 1]
                [0  -1]      [0  1]
                [1  -1][1  0][1  1]])

(defn round[original]
  (let [c (count original)]
    (loop [copy original i 0 j 0]
      (cond
        (> i c) (recur copy 0 (inc j))
        (> j c) copy
        (and (= \L (get-in original [i j])) (= 0 (count (filter #(= % \#) (map (fn [[x y]] (get-in original [(+ x i) (+ y j)])) positions))))) (recur (assoc-in copy [i j] \#) (inc i) j)
        (and (= \# (get-in original [i j])) (<= 4(count (filter #(= % \#) (map (fn [[x y]] (get-in original [(+ x i) (+ y j)])) positions))))) (recur (assoc-in copy [i j] \L) (inc i) j)
        :else (recur copy (inc i) j)))))

(defn rounds [m]
  (loop [m m]
      (let [tmp (round m)]
        (if (= tmp m)
          m
          (recur tmp)))))

(assert (= 37 (->> test-seats
  (mapv vec)
  (rounds)
  flatten
  (filterv #(= % \#))
  count)))

(assert (= 2166 (->> seats
  (mapv vec)
  (rounds)
  flatten
  (filterv #(= % \#))
  count)))

(defn extended-get-in [map position [offset-i offset-y]]
  (loop [[i j] position]
    (let [new-i (+ i offset-i)
          new-y (+ j offset-y)
          s (get-in map [new-i new-y])]
      (cond 
        (nil? s) nil
        (or (= s \#) (= s \L)) s
        :else (recur [new-i new-y])))))


(defn extended-round[original]
  (let [c (count original)]
    (loop [copy original i 0 j 0]
      (cond
        (> i c) (recur copy 0 (inc j))
        (> j c) copy
        (and (= \L (get-in original [i j])) (= 0 (count (filter #(= % \#) (map #(extended-get-in original [i j] %) positions))))) (recur (assoc-in copy [i j] \#) (inc i) j)
        (and (= \# (get-in original [i j])) (<= 5(count (filter #(= % \#) (map #(extended-get-in original [i j] %) positions))))) (recur (assoc-in copy [i j] \L) (inc i) j)
        :else (recur copy (inc i) j)))))

(defn extended-rounds [m]
  (loop [m m]
      (let [tmp (extended-round m)]
        (if (= tmp m)
          m
          (recur tmp)))))

(assert (= 26 (->> test-seats2
  (mapv vec)
  (extended-rounds)
  flatten
  (filterv #(= % \#))
  count)))

(assert (= 1955 (->> seats
  (mapv vec)
  (extended-rounds)
  flatten
  (filterv #(= % \#))
  count)))