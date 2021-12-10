(ns  aoc2021.d9
  (:require [clojure.string :as str]))

(def max-number Integer/MAX_VALUE)

(def input-from-file-test
  (->> (slurp "resources/aoc2021/day9_t")
       (str/split-lines)
       (mapv #(str/split % #""))
       (mapv #(mapv (fn [x] (Integer/parseInt x)) %))))

(def input-from-file
  (->> (slurp "resources/aoc2021/day9_1")
       (str/split-lines)
       (mapv #(str/split % #""))
       (mapv #(mapv (fn [x] (Integer/parseInt x)) %))))

(assert (= 2147483647 (get [] [0 0 0] max-number)))

(defn part1 [input]
  (let [x (count input)
        y (count (first input))
        result (for [i (range x)  j (range y)]
                 (let [c (get-in input [i j])
                       t (get-in input [(dec i) j] max-number)
                       b (get-in input [(inc i) j] max-number)
                       r (get-in input [i (inc j)] max-number)
                       l (get-in input [i (dec j)] max-number)]
                   (if (every? #(> % c) [t b r l])
                     (inc c)
                     0)))]
    (apply + result)))


(assert (= 15 (part1 input-from-file-test)))
(assert (= 496 (part1 input-from-file)))



(defn part2
  ([input] (apply + input)))



(assert (= 61229 (part2 input-from-file-test)))
(assert (= 915941 (part2 input-from-file)))

