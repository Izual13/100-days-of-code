(ns  aoc2021.d8
  (:require [clojure.string :as str]))

(def input-from-file-test
  (->> (slurp "resources/aoc2021/day8_t")
       (str/split-lines)
       (mapv #(str/split % #" \| "))
       (mapv #(last %))
       ))

(def input-from-file
  (->> (slurp "resources/aoc2021/day8_1")
       (str/split-lines)
       (mapv #(str/split % #" \| "))
       (mapv #(last %))
       ))


(defn part1
  ([input] 
   (->> input
       (mapv #(str/split % #" "))
        (flatten)
        (filter #(let[c (count %)] (or (= c 2) (= c 3) (= c 4) (= c 7))))
        (count)
                )))


(assert (= 26 (part1 input-from-file-test)))
(assert (= 352707 (part1 input-from-file)))


(defn part2
  ([input] (input)))


(assert (= 168 (part2 input-from-file-test)))
(assert (= 95519693 (part2 input-from-file)))


