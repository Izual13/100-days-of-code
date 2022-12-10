(ns aoc2022.d10
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(def test-input (clojure.string/split (slurp "resources/aoc2022/day10_t") #"\n"))
(def input (clojure.string/split (slurp "resources/aoc2022/day10_1") #"\n"))


(defn parse-instruction [s]
  (let [[_ a b] (re-matches #"(\w*) (.*)" s)]
    (if (nil? b)
      [0]
      [0 (Integer/parseInt b)])))


(assert (= [0 -5213] (parse-instruction "addx -5213")))
(assert (= [0] (parse-instruction "noop")))


(defn execution [instructions] 
  (loop [a instructions i 1 acc 1 r 0]
    (if (empty? a) 
      r
      (let [v (first a)
            new-r (cond 
                    (= (mod (- i 20) 40) 0) (+ r (* acc i))
                    :else r)]
        (recur (next a) (inc i) (+ v acc) new-r)))))

(assert (= 13140 (->> test-input
                   (mapcat parse-instruction)
                   execution)))

(assert (= 17180 (->> input
                   (mapcat parse-instruction)
                   execution)))

(defn execution2 [instructions] 
  (loop [a instructions i 1 acc 1 r ""]
    (if (empty? a) 
      r
      (let [v (first a)
            new-r (cond 
                    (= 0 (mod i 40))  "\n"
                    (<= acc (mod i 40) (+ 2 acc)) "#" 
                    :else ".")]
        (recur (next a) (inc i) (+ v acc) (str r new-r))))))

(assert (= (->> test-input
             (mapcat parse-instruction)
             execution2)
          (str 
            "##..##..##..##..##..##..##..##..##..##.\n"
            "###...###...###...###...###...###...###\n"
            "####....####....####....####....####...\n"
            "#####.....#####.....#####.....#####....\n"
            "######......######......######......###\n"
            "#######.......#######.......#######....\n")))



(assert (= (->> input
             (mapcat parse-instruction)
             execution2)
          (str 
            "###..####.#..#.###..###..#....#..#.###.\n"
            "#..#.#....#..#.#..#.#..#.#....#..#.#..#\n"
            "#..#.###..####.#..#.#..#.#....#..#.###.\n"
            "###..#....#..#.###..###..#....#..#.#..#\n"
            "#.#..#....#..#.#....#.#..#....#..#.#..#\n"
            "#..#.####.#..#.#....#..#.####..##..###.\n")))
