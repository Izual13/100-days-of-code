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

(:y1 (first input-from-file-test))

(let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" "505,796 -> 505,244")] (println x1 x2 y1 y2))


(defn max-by-coordinate [input [coordinate1 coordinate2]] (apply max (flatten (map (fn [row] [(coordinate1 row) (coordinate2 row)]) input))))

(defn max-element-in-matrix [matrix] (apply max (flatten matrix)))

(assert (= 13 (max-element-in-matrix [[1 2 3] [3 4 5 2 3 5] [13]])))

(defn count-element-in-matrix [matrix element] (count (filter #(= % element) (flatten matrix))))

(assert (= 3 (count-element-in-matrix [[1 2 3] [3 4 5 2 3 5] [13]] 3)))
(assert (= 1 (count-element-in-matrix [[1 2 3] [3 4 5 2 3 5] [13]] 4)))

(max-by-coordinate input-from-file-test [:y2 :y1])


(apply max (flatten (map (fn [row] [(:x1 row) (:x2 row)]) input-from-file-test)))

(defn build-matrix [x y] (vec (for [_ (range (inc x))] (vec (take (inc y) (repeat 0))))))

(build-matrix 5 5)

(defn update-matrix [matrix coordinates]
  (let [x (sort [(:x1 coordinates) (:x2 coordinates)])
        y (sort [(:y1 coordinates) (:y2 coordinates)])]
    (println "coordinates" coordinates)
    (cond (= (first x) (last x)) (loop [i (first y) m matrix]
                                   (if (> i (last y))
                                     m
                                     (let [x (first x)
                                           r (get matrix i)
                                           ;_ (println "r" r)
                                           r (assoc r x (inc (get r x)))
                                           ;_ (println "new r" r)
                                           ]
                                       (recur (inc i) (assoc m i r)))))
          (= (first y) (last y)) (let [r (get matrix (first y))
                                       last-x (last x)
                                       ;_ (println "matrix" matrix)
                                       ;_ (println "r" r "last-x" last-x)
                                       r (loop [i (first x) r r]
                                           ;(println "i:" i "r:" r)
                                           (if (> i last-x)
                                             r
                                             (recur (inc i) (assoc r i (inc (get r i))))))
                                       ;_ (println r)
                                       ]
                                   (assoc matrix (first y) r))
          :else matrix)))


(update-matrix [[0 0 0 0] [0 0 0 0]] {:x1 0, :y1 0, :x2 0, :y2 1})
(update-matrix [[1 2 3 4] [5 6 7 8]] {:x1 3 :y1 0, :x2 0, :y2 0})

(assert (= [[0 0 0 0] [0 0 0 0]] (update-matrix [[0 0 0 0] [0 0 0 0]] {:x1 1, :y1 2, :x2 3, :y2 4})))
(assert (= [[1 0 0 0] [1 0 0 0]] (update-matrix [[0 0 0 0] [0 0 0 0]] {:x1 0, :y1 0, :x2 0, :y2 1})))
(assert (= [[2 3 4 5] [5 6 7 8]] (update-matrix [[1 2 3 4] [5 6 7 8]] {:x1 3 :y1 0, :x2 0, :y2 0})))
(assert (= [[0 0 0 0] [0 1 1 0] [0 0 0 0] [0 0 0 0]] (update-matrix [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]] {:x1 2, :y1 1, :x2 1, :y2 1})))



(defn part1
  ([input] (let [max-x (max-by-coordinate input [:x1 :x2])
                 max-y (max-by-coordinate input [:y1 :y2])
                 matrix (build-matrix max-x  max-y)
                 new-matrix (loop [lines input matrix matrix]
                              (if (empty? lines)
                                matrix
                                (let [line (first lines)] (recur (rest lines) (update-matrix matrix line)))))
                 _ (println max-x)
                 _ (println max-y)
                 max-element-in-matrix (max-element-in-matrix new-matrix)

                 _ (println "max-element-in-matrix" max-element-in-matrix)
                 result (count-element-in-matrix new-matrix max-element-in-matrix)]
             result)))

(part1 input-from-file-test)

(assert (= 5 (part1 input-from-file-test)))
(assert (= 15 (part1 input-from-file)))



