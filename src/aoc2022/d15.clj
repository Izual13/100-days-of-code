(ns aoc2022.d15
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(def test-input (str/split (slurp "resources/aoc2022/day15_t") #"\n"))
(def input (str/split (slurp "resources/aoc2022/day15_1") #"\n"))


test-input

(defn calc-length [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (+ (abs (- x1 x2)) (abs (- y1 y2)))))

(defn parse-input [s]
  (let [[_ a b c d] (re-matches #"Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)" s)
        x1 (Integer/parseInt a) 
        y1 (Integer/parseInt b)
        x2 (Integer/parseInt c) 
        y2 (Integer/parseInt d)]
    {:s [x1 y1] :b [x2 y2] :l (calc-length [x1 y1] [x2 y2])}))


(assert (= {:s [214540 3768792], :b [-355567 3900317], :l 701632}
          (parse-input "Sensor at x=214540, y=3768792: closest beacon is at x=-355567, y=3900317")))

(parse-input "Sensor at x=214540, y=3768792: closest beacon is at x=-355567, y=3900317")


(defn filter-sensors [row sensors] 
  (loop [s sensors r []]
    (if (empty? s)
      r
      (let [f (first s)
            y (second (:s f))
            l (:l f)]
        (if (and (<= (- y l) row (+ y l)))
          (recur (rest s) (conj r f))
          (recur (rest s) r))))))

(defn check-point [sensors p1]
  (loop [s sensors] 
    (if (empty? s)
      false
      (let [f (first s)
            p2 (:s f)
            l (:l f)
            ; _ (println f p1 p2 l)
            ]
        (if (<= (calc-length p1 p2) l)
          true
          (recur (rest s)))))))

(check-point [{:s [2 18], :b [-2 15], :l 7}] [0 100])

(defn count-positions [row sensors]
  (let [min-x (reduce min (map #(- (first (:s %)) (:l %)) sensors))
        max-x (reduce max (map #(+ (first (:s %)) (:l %)) sensors))]
    (for [i (range min-x max-x)
          :when (check-point sensors [i row])] [i row])))



(->> test-input
  (mapv parse-input)
  (filter-sensors 10)
  (count-positions 10)
  (count)
  )

27 -1




(->> input
  (mapv parse-input)
  (filter-sensors 2000000)
  (count-positions 2000000)
  count
  )

5838454 - 1






