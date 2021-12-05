(ns  aoc2021.d5
  (:require [clojure.string :as str]))

(def input-from-file-test
  (map #(let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" %)] {:x1 (Integer/parseInt x1) :y1 (Integer/parseInt y1) :x2 (Integer/parseInt x2) :y2 (Integer/parseInt y2)})
       (str/split-lines
        (slurp "resources/aoc2021/day5_t"))))

(def input-from-file
  (map #(let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" %)] {:x1 (Integer/parseInt x1) :y1 (Integer/parseInt y1) :x2 (Integer/parseInt x2) :y2 (Integer/parseInt y2)})
       (str/split-lines
        (slurp "resources/aoc2021/day5_1"))))


(assert (= 9 (:y1 (first input-from-file-test))))
(assert (= ["505" "505" "796" "244"] (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" "505,796 -> 505,244")] [x1 x2 y1 y2])))

(defn max-by-coordinate [input [coordinate1 coordinate2]] (apply max (flatten (map (fn [row] [(coordinate1 row) (coordinate2 row)]) input))))

(defn max-element-in-matrix [matrix] (apply max (flatten matrix)))

(assert (= 13 (max-element-in-matrix [[1 2 3] [3 4 5 2 3 5] [13]])))

(defn count-elements-more-than-2 [matrix] (count (filter #(>= % 2) (flatten matrix))))

(assert (= 9 (count-elements-more-than-2 [[1 2 3] [3 4 5 2 3 5] [13]])))
(assert (= 1 (count-elements-more-than-2 [[1 2 1] [1 1 1 1 1 1] [1]])))

(assert (= 9 (max-by-coordinate input-from-file-test [:y2 :y1])))

(defn build-matrix [x y] (vec (for [_ (range (inc x))] (vec (take (inc y) (repeat 0))))))

(assert (= [[0 0 0] [0 0 0] [0 0 0]] (build-matrix 2 2)))

(defn update-matrix [matrix coordinates]
  (let [x (sort [(:x1 coordinates) (:x2 coordinates)])
        y (sort [(:y1 coordinates) (:y2 coordinates)])]
    (cond (= (first x) (last x)) (loop [i (first y) m matrix]
                                   (if (> i (last y))
                                     m
                                     (let [x (first x)
                                           r (get matrix i)
                                           r (assoc r x (inc (get r x)))
                                           ]
                                       (recur (inc i) (assoc m i r)))))
          (= (first y) (last y)) (let [r (get matrix (first y))
                                       last-x (last x)
                                       r (loop [i (first x) r r]
                                           (if (> i last-x)
                                             r
                                             (recur (inc i) (assoc r i (inc (get r i))))))
                                       ]
                                   (assoc matrix (first y) r))
          :else matrix)))


(assert (= [[0 0 0 0] [0 0 0 0]] (update-matrix [[0 0 0 0] [0 0 0 0]] {:x1 1, :y1 2, :x2 3, :y2 4})))
(assert (= [[1 0 0 0] [1 0 0 0]] (update-matrix [[0 0 0 0] [0 0 0 0]] {:x1 0, :y1 0, :x2 0, :y2 1})))
(assert (= [[2 3 4 5] [5 6 7 8]] (update-matrix [[1 2 3 4] [5 6 7 8]] {:x1 3 :y1 0, :x2 0, :y2 0})))
(assert (= [[0 0 0 0] [0 1 1 0] [0 0 0 0] [0 0 0 0]] (update-matrix [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]] {:x1 2, :y1 1, :x2 1, :y2 1})))


(defn update-matrix-diagonal [matrix coordinates]
  (let [start-x (:x1 coordinates)
        end-x (:x2 coordinates)
        start-y (:y1 coordinates)
        end-y (:y2 coordinates)
        x-move (if (<= start-x end-x) inc dec)
        y-move (if (<= start-y end-y) inc dec)
        length (Math/abs (- end-x start-x))]
    (loop [i 0 x start-x y start-y m matrix]
      (if (> i length)
        m
        (let [r (get m y)
              e (get r x)
              new-r (assoc r x (inc e))
              ]
          (recur (inc i) (x-move x) (y-move y) (assoc m y new-r)))))))


(assert (= [[1 0 0 0] [0 1 0 0] [0 0 1 0] [0 0 0 1]] (update-matrix-diagonal [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]] {:x1 0, :y1 0, :x2 3, :y2 3})))
(assert (= [[0 0 0 0] [0 1 0 0] [0 0 1 0] [0 0 0 1]] (update-matrix-diagonal [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]] {:x1 1, :y1 1, :x2 3, :y2 3})))
(assert (= [[0 0 0 1] [0 0 1 0] [0 1 0 0] [1 0 0 0]] (update-matrix-diagonal [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]] {:x1 0 :y1 3, :x2 3, :y2 0})))
(assert (= [[0 0 1 0 0 0 0] [0 0 0 1 0 0 0] [0 0 0 0 1 0 0] [0 0 0 0 0 1 0] [0 0 0 0 0 0 1] [0 0 0 0 0 0 0] [0 0 0 0 0 0 0]] (update-matrix-diagonal (build-matrix 6 6) {:x1 6, :y1 4, :x2 2, :y2 0})))


(defn update-matrix-extended [matrix coordinates]
  (let [x (sort [(:x1 coordinates) (:x2 coordinates)])
        y (sort [(:y1 coordinates) (:y2 coordinates)])]
    (cond (= (first x) (last x)) (loop [i (first y) m matrix]
                                   (if (> i (last y))
                                     m
                                     (let [x (first x)
                                           r (get matrix i)
                                           r (assoc r x (inc (get r x)))
                                           ]
                                       (recur (inc i) (assoc m i r)))))
          (= (first y) (last y)) (let [r (get matrix (first y))
                                       last-x (last x)
                                       r (loop [i (first x) r r]
                                           (if (> i last-x)
                                             r
                                             (recur (inc i) (assoc r i (inc (get r i))))))
                                       ]
                                   (assoc matrix (first y) r))
          :else (update-matrix-diagonal matrix coordinates))))

(defn part1
  ([input] (let [max-x (max-by-coordinate input [:x1 :x2])
                 max-y (max-by-coordinate input [:y1 :y2])
                 matrix (build-matrix max-x  max-y)
                 new-matrix (loop [lines input matrix matrix]
                              (if (empty? lines)
                                matrix
                                (let [line (first lines)] (recur (rest lines) (update-matrix matrix line)))))
                 result (count-elements-more-than-2 new-matrix)]
             result)))

(assert (= 5 (part1 input-from-file-test)))
(assert (= 5306 (part1 input-from-file)))


(defn part2
  ([input] (let [max-x (max-by-coordinate input [:x1 :x2])
                 max-y (max-by-coordinate input [:y1 :y2])
                 matrix (build-matrix max-x  max-y)
                 new-matrix (loop [lines input matrix matrix]
                              (if (empty? lines)
                                matrix
                                (let [line (first lines)] (recur (rest lines) (update-matrix-extended matrix line)))))
                 result (count-elements-more-than-2 new-matrix)]
             result)))

(assert (= 12 (part2 input-from-file-test)))
(assert (= 17787 (part2 input-from-file)))

