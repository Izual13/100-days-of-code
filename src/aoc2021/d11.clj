(ns  aoc2021.d11
  (:require [clojure.string :as str]))


(def input-from-file-test
  (->> (slurp "resources/aoc2021/day11_t")
       (str/split-lines)
       (mapv #(str/split % #""))
       (mapv #(mapv (fn [x] (Long/parseLong x)) %))))

(def input-from-file
  (->> (slurp "resources/aoc2021/day11_1")
       (str/split-lines)
       (mapv #(str/split % #""))
       (mapv #(mapv (fn [x] (Long/parseLong x)) %))))


(defn get-neighbours [m p]
  (let [[x y] p
        points [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
                [(dec x) y] [(inc x) y]
                [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]]
    (filterv #(> 10 (get-in m % 10)) points)))


(defn inc-energy-level [m]
  (loop [result m points (set (for [i (range (count m)) j (range (count m))] [i j]))]
    (if (empty? points) result
        (let [f (first points)]
          (recur (update-in result f inc) (set (rest points)))))))

(defn get-flashed-points [m]
  (loop [result [] points (set (for [i (range (count m)) j (range (count m))] [i j]))]
    (if (empty? points) result
        (let [f (first points)
              v (get-in m f)
              new-points (rest points)
              new-result (if (= v 10) (conj result f) result)]
          (recur new-result new-points)))))


(defn get-score-points [m]
  (loop [result [] points (set (for [i (range (count m)) j (range (count m))] [i j]))]
    (if (empty? points) result
        (let [f (first points)
              v (get-in m f)
              new-points (rest points)
              new-result (if (>= v 10) (conj result f) result)]
          (recur new-result new-points)))))


(assert (=  [[1 0] [0 1]] (get-flashed-points [[2 10] [10 2]])))


(defn next-step [m]
  (loop [result m points (get-flashed-points m)]
    (if (empty? points) (let [tmp (get-flashed-points result)] (if (empty? tmp) result (recur result tmp)))
        (let [f (first points)
              neighbours (get-neighbours result f)
              new-result (update-in result f inc)
              new-result (loop [n neighbours r new-result]
                           (if (empty? n) r (recur (rest n) (update-in r (first n) inc))))
              new-points (rest points)]
          (recur new-result new-points)))))

(defn cleanup [m]
  (let [sp (get-score-points m)]
    (loop [result m points sp]
      (if (empty? points) [result (count sp)]
          (let [f (first points)]
            (recur (assoc-in result f 0) (rest points)))))))

(defn part1 [input]
  (loop [i 0 m input r 0]
    (if (= i 100)
      r
      (let [new-m (inc-energy-level m)
            new-m (next-step new-m)
            [new-m score] (cleanup new-m)
            new-r (+ r score)]
        (recur (inc i) new-m new-r)))))

(assert (= 1656 (part1 input-from-file-test)))
(assert (= 1620 (part1 input-from-file)))


(defn part2 [input]
  (loop [i 0 m input r 0]
    (let [new-m (inc-energy-level m)
          new-m (next-step new-m)
          [new-m score] (cleanup new-m)
          new-r (+ r score)]
      (if (= score 100) (inc i)
          (recur (inc i) new-m new-r)))))


(assert (= 195 (part2 input-from-file-test)))
(assert (= 371 (part2 input-from-file)))