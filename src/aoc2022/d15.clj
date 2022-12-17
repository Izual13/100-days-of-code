(ns aoc2022.d15
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(def test-input (str/split (slurp "resources/aoc2022/day15_t") #"\n"))
(def input (str/split (slurp "resources/aoc2022/day15_1") #"\n"))


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
            l (:l f)]
        (if (<= (calc-length p1 p2) l)
          true
          (recur (rest s)))))))

(check-point [{:s [2 18], :b [-2 15], :l 7}] [0 100])

(defn count-positions [row sensors]
  (let [min-x (reduce min (map #(- (first (:s %)) (:l %)) sensors))
        max-x (reduce max (map #(+ (first (:s %)) (:l %)) sensors))
        count-b (count (set (for [s sensors
                                  :when (= (second (:b s)) row)] (:b s))))
        count-p (count (for [i (range min-x max-x)
                             :when (check-point sensors [i row])] 1))]
    
    (- count-p count-b)))



(assert (= 26 (->> test-input
                (mapv parse-input)
                (filter-sensors 10)
                (count-positions 10))))

(assert (= 5838453 (->> input
                     (mapv parse-input)
                     (filter-sensors 2000000)
                     (count-positions 2000000))))



;    private fun getRangesForRow(row: Int): List<IntRange> {
;     return pairs.mapNotNull { (sensor, beacon) ->
;         val distance = sensor distanceTo beacon
;         val begin = sensor.x - distance + abs(row - sensor.y)
;         val end = sensor.x + distance - abs(row - sensor.y)
;         (begin..end).takeUnless { it.isEmpty() }
;     }.sortedBy { it.first }
; }

;{:s [214540 3768792], :b [-355567 3900317], :l 701632}
(defn get-ranges[row sensors] 
  (loop [s sensors r []]
    (if (empty? s)
      r
      (let [f (first s)
            [x y] (:s f)
            d (:l f)
            b (+ (- x d) (abs (- row y)))
            e (- (+ x d) (abs (- row y)))]
        (if (> b e) 
          (recur (next s) r)
          (recur (next s) (conj r [b e])))))))

(defn merge-ranges[ranges]
  (loop [r (sort ranges) result []]
    (cond 
      (empty? r) result
      (empty? result) (recur (next r) (conj result (first r)))
      :else (let [[rb re] (last result)
                  i (dec (count result))
                  [b e] (first r)
                  ; _ (println r result rb re i b e)
                  ]
              (if (<= rb b re) 
                (recur (next r) (assoc result i [rb (max re e)]))
                (recur (next r) (conj result [b e])))))))

(defn count-ranges [ranges] 
  (loop [r (sort ranges) result 0]
    (if 
      (empty? r) result
      (let [[b e] (first r)]
        (recur (next r) (+ (- e b)))))))

(assert (= 26 (->> test-input
                (mapv parse-input)
                (get-ranges 10)
                (merge-ranges)
                (count-ranges))))

(time (->> input
        (mapv parse-input)
        (get-ranges 2000000)
        (merge-ranges)
        (count-ranges)))
(comment 
  (loop [i 0]
    (let [ranges (->> input
                   (mapv parse-input)
                   (get-ranges i)
                   (merge-ranges))
          _ (if (= 0 (mod i 10000)) (println i) nil)]
      (if (> (count ranges) 1) 
        [i ranges]
        (recur (inc i))))))



(assert (= 12413999391794 (+ (* 3103499 4000000) 3391794)))
