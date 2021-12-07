(ns  aoc2021.d6
  (:require [clojure.string :as str]))

(def input-from-file-test
  (->> (slurp "resources/aoc2021/day7_t")
       (str/split-lines)
       (mapv #(str/split % #","))
       (flatten)
       (mapv #(Integer/parseInt %))))


(count (sort input-from-file-test))

(get input-from-file-test 5)

(def input-from-file
  (->> (slurp "resources/aoc2021/day7_1")
       (str/split-lines)
       (mapv #(str/split % #","))
       (flatten)
       (mapv #(Integer/parseInt %))))

(count (sort input-from-file))

(get (vec (sort input-from-file))  500)


(apply + (map #(Math/abs (- 376 %)) input-from-file))


(apply + (map #(apply + (range (Math/abs (- 376 %)))) input-from-file))


(apply + (map #(apply + (range (Math/abs (- 5 %)))) input-from-file-test))

(apply + (range (inc (- 16 5))))

(apply + (map #(Math/abs (- 2 %)) input-from-file-test))

(assert (= [3 4 3 1 2] input-from-file-test))

(count (sort input-from-file))


(defn part1
  ([input] (let [median (get (vec (sort input))  (/ (count input) 2))]
             (apply + (map #(Math/abs (- median %)) input)))))

(assert (= 37 (part1 input-from-file-test)))
(assert (= 352707 (part1 input-from-file)))

(defn part2
  ([input] (input)))


(assert (= 26984457539 (part2 input-from-file-test)))
(assert (= 1767323539209 (part2 input-from-file)))
(assert (= {0 30, 2 20} (assoc (array-map 0 10 2 20) 0 30)))


