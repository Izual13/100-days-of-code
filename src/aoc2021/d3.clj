(ns  aoc2021.d1
  (:require [clojure.string :as str]))

(def input-from-file-test
  (str/split-lines
   (slurp "resources/aoc2021/day3_t")))

(def input-from-file
  (str/split-lines
   (slurp "resources/aoc2021/day3_1")))

(defn part1
  ([input] (let [c (count input)
                 h (/ c 2)
                 cw (count (first input))
                 g (->> (loop [i 0 r []]
                          (if (= i cw)
                            r
                            (recur (inc i) (conj r (count (filter #(= % \1)  (map #(nth % i) input)))))))
                        (map #(if (> % h) 1 0)))
                 gamma (Integer/parseInt (apply str g) 2)
                 epsilon (Integer/parseInt (apply str (map #(if (= 1 %) 0 1) g)) 2)]
             (* gamma epsilon))))

(assert (= 198 (part1 input-from-file-test)))
(assert (= 3687446 (part1 input-from-file)))

(defn find-primary-bit [input i]
  (let [c (count input)
        h (/ c 2)
        count-bits (count (filter #(= % \1)  (map #(nth % i) input)))]
    (if (> count-bits h)
      \1
      (if (and (= (* h 2) c) (= h count-bits)) \1 \0))))


(defn filter-by-primary-bit [input i b]
  (filter #(= b (nth % i)) input))

(assert (= \1 (find-primary-bit ["10110" "10111"] 4)))


(defn part2
  ([input] (let [cw (count (first input))
                 ox (loop [i 0 r input]
                      (if (or (= i cw) (= 1 (count r)))
                        (first r)
                        (let [pb (find-primary-bit r i)
                              filtered-input (filter-by-primary-bit r i pb)]
                          (recur (inc i) filtered-input))))

                 co (loop [i 0 r input]
                      (if (or (= i cw) (= 1 (count r)))
                        (first r)
                        (let [pb (find-primary-bit r i)
                              pb (if (= pb \1) \0 \1)
                              filtered-input (filter-by-primary-bit r i pb)]
                          (recur (inc i) filtered-input))))

                 oxygen (Integer/parseInt (apply str ox) 2)
                 co2  (Integer/parseInt (apply str co) 2)]
             (* oxygen co2))))


(assert (= 230 (part2 input-from-file-test)))
(assert (= 4406844 (part2 input-from-file)))

