(ns  aoc2021.d9
  (:require [clojure.string :as str]))

(def max-number Long/MAX_VALUE)

(def input-from-file-test
  (->> (slurp "resources/aoc2021/day9_t")
       (str/split-lines)
       (mapv #(str/split % #""))
       (mapv #(mapv (fn [x] (Long/parseLong x)) %))))

(def input-from-file
  (->> (slurp "resources/aoc2021/day9_1")
       (str/split-lines)
       (mapv #(str/split % #""))
       (mapv #(mapv (fn [x] (Long/parseLong x)) %))))

(assert (= 2147483647 (get [] [0 0 0] max-number)))

(defn get-neighbour-values [input [i j]]
  [(get-in input [(dec i) j] max-number)
   (get-in input [(inc i) j] max-number)
   (get-in input [i (inc j)] max-number)
   (get-in input [i (dec j)] max-number)])

(defn get-low-points [input] (let [x (count input)
                                   y (count (first input))
                                   result (for [i (range x)  j (range y)]
                                            (let [c (get-in input [i j])
                                                  [t b r l] (get-neighbour-values input [i j])]
                                              (if (every? #(> % c) [t b r l])
                                                [i j]
                                                nil)))]
                               (remove nil? result)))

(defn part1 [input]
  (->> input
       (get-low-points)
       (map (fn [[i j]] (get-in input [i j])))
       (map inc)
       (apply +)))


(assert (= 15 (part1 input-from-file-test)))
(assert (= 496 (part1 input-from-file)))


(defn get-neighbour-points [[i j]]
  [[(dec i) j] [(inc i) j] [i (inc j)] [i (dec j)]])

(defn count-neighbours [input point]
  (let [neighbours (loop [p [point]  r #{}]
                     (if (empty? p)
                       r
                       (if (some #(= % (first p)) r)
                         (recur (rest p) r)
                         (let [c (first p)
                               new-points (filter (fn [p] (and (not= 9 (get-in input p 9)) (not (some #(= % p) r)))) (get-neighbour-points c))]
                           (if (empty? new-points)
                             (recur (rest p) (conj r c))
                             (recur (apply conj (rest p) new-points) (conj r c)))))))]
    (count neighbours)))

(assert (= 3 (count-neighbours input-from-file-test [0 1])))
(assert (= 9 (count-neighbours input-from-file-test [0 9])))
(assert (= 9 (count-neighbours input-from-file-test [4 6])))
(assert (= 14 (count-neighbours input-from-file-test [2 2])))
(assert (= 99 (count-neighbours input-from-file [30 8])))

(defn part2 [input]
  (->> input
       (get-low-points)
       (map #(count-neighbours input %))
       (sort)
       (take-last 3)
       (apply *)))

(assert (= 1134 (part2 input-from-file-test)))
(assert (= 902880 (part2 input-from-file)))
