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
                 epsilon (Integer/parseInt (apply str (map #(if (= 1 %) 0 1) g)) 2)
                 _ (println gamma epsilon g)
                 ] 
             (* gamma epsilon))))

(assert (= 198 (part1 input-from-file-test)))
(assert (= 3687446 (part1 input-from-file)))

