(ns  aoc2024.d8
  (:require [clojure.string :as str]))


(def test-antennas (str/split (slurp "resources/aoc2024/d8_t") #"\n"))
(def antennas (str/split (slurp "resources/aoc2024/d8_1") #"\n"))

(defn find-frequencies [m]
  (let [c (count m)
        _ (println c)]
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
