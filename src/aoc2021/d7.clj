(ns  aoc2021.d6
  (:require [clojure.string :as str]))

(def input-from-file-test
  (->> (slurp "resources/aoc2021/day7_t")
       (str/split-lines)
       (mapv #(str/split % #","))
       (flatten)
       (mapv #(Integer/parseInt %))))


(def input-from-file
  (->> (slurp "resources/aoc2021/day7_1")
       (str/split-lines)
       (mapv #(str/split % #","))
       (flatten)
       (mapv #(Integer/parseInt %))))

(assert (= [3 4 3 1 2] input-from-file-test))

(count (sort input-from-file))


(defn part1
  ([input] (let [median (get (vec (sort input))  (/ (count input) 2))]
             (apply + (map #(Math/abs (- median %)) input)))))

(assert (= 37 (part1 input-from-file-test)))
(assert (= 352707 (part1 input-from-file)))


(defn calc-step [n] (/ (* n (+ n 1)) 2))


(assert (= 10 (calc-step 4)))

(defn part2
  ([input] (let [max-input (apply max input)
                 min-input (apply min input)]
             (apply min (for [i (range min-input max-input)] (apply + (map #(calc-step (Math/abs (- i %))) input))))
             )))


(assert (= 168 (part2 input-from-file-test)))
(assert (= 95519693 (part2 input-from-file)))


