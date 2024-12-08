(ns  aoc2024.d8
  (:require [clojure.string :as str]))


(def test-antennas (str/split (slurp "resources/aoc2024/d8_t") #"\n"))
(def antennas (str/split (slurp "resources/aoc2024/d8_1") #"\n"))

(defn find-frequencies [m]
  (let [c (count m)]
    (loop [i 0 j 0 r {}]
      (cond 
        (= j c) (dissoc r \.)
        (= i c) (recur 0 (inc j) r)
        :else (recur (inc i) j (update-in r [(get-in m [i j])] (fnil #(conj % [i j]) #{})))))))


(defn find-coordinates [[i1 j1] [i2 j2]] 
  (let [x (abs (- i1 i2))
        y (abs (- j1 j2))]
    (cond 
      (and (< i1 i2) (< j1 j2)) [[(- i1 x) (- j1 y)] [(+ i2 x) (+ j2 y)]]
      (and (> i1 i2) (> j1 j2)) [[(+ i1 x) (+ j1 y)] [(- i2 x) (- j2 y)]]
      (and (> i1 i2) (< j1 j2)) [[(+ i1 x) (- j1 y)] [(- i2 x) (+ j2 y)]]
      (and (< i1 i2) (> j1 j2)) [[(- i1 x) (+ j1 y)] [(+ i2 x) (- j2 y)]]
      :else (throw (ex-info "!!!!")))))

(defn find-antinodes [n]
  (loop [n n r []]
    (if (empty? n) 
      r
      (let [r' (for [i (next n)] (find-coordinates (first n) i))
            r' (apply concat r')] 
        (recur (next n) (apply conj r r'))))))

(assert (= 14 (->> test-antennas
  (mapv vec)
  (find-frequencies)
  (mapv second)
  (map find-antinodes)
  (apply concat)
  set
  (filter (fn [[i j]] (and (<= 0 i 11) (<= 0 j 11))))
  count)))

(assert (= 252 (->> antennas
  (mapv vec)
  (find-frequencies)
  (mapv second)
  (map find-antinodes)
  (apply concat)
  set
  (filter (fn [[i j]] (and (<= 0 i 49) (<= 0 j 49))))
  count)))


(defn find-all-coordinates [[i1 j1] [i2 j2]] 
  (let [x (abs (- i1 i2))
        y (abs (- j1 j2))
        coord-pred (fn [[x y]] (and (<= 0 x 49) (<= 0 y 49)))
        get-coords (fn [ix iy op1 op2] (take-while coord-pred (iterate (fn [[i j]] [(op1 i x) (op2 j y)]) [ix iy])))
        ]
    (cond 
      (and (< i1 i2) (< j1 j2)) (concat (get-coords i1 j1 - -) (get-coords i2 j2 + +))
      (and (> i1 i2) (> j1 j2)) (concat (get-coords i1 j1 + +) (get-coords i2 j2 - -))
      (and (> i1 i2) (< j1 j2)) (concat (get-coords i1 j1 + -) (get-coords i2 j2 - +))
      (and (< i1 i2) (> j1 j2)) (concat (get-coords i1 j1 - +) (get-coords i2 j2 + -))
      :else (throw (Exception. "!!!!")))))

(defn find-all-antinodes [n]
  (loop [n n r []]
    (if (empty? n) 
      r
      (let [r' (for [i (next n)] (find-all-coordinates (first n) i))
            r' (apply concat r')] 
        (recur (next n) (apply conj r r'))))))


(assert (= 839 (->> antennas
  (mapv vec)
  (find-frequencies)
  (mapv second)
  (map find-all-antinodes)
  (apply concat)
  set
  count)))