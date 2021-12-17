(ns  aoc2021.d13
  (:require [clojure.string :as str]))



(def input-from-file-test
  (let [[paper insrruction] (str/split (slurp "resources/aoc2021/day13_t") #"\r\n\r\n")
        parsed-paper (->> paper
                          (str/split-lines)
                          (mapv #(str/split % #","))
                          (mapv #(mapv (fn [x] (Long/parseLong x)) %)))
        parsed-insrructions (->> insrruction
                                 (str/split-lines)
                                 (mapv #(let [[_ c p] (re-matches #"fold along (\w+)=(\d+)" %)] [c (Long/parseLong p)])))]
    {:points parsed-paper :insrructions parsed-insrructions}))


(assert (= ["x" "655"] (let [[_ c p] (re-matches #"fold along (\w+)=(\d+)" "fold along x=655")] [c p])))


(def input-from-file
  (let [[paper insrruction] (str/split (slurp "resources/aoc2021/day13_1") #"\r\n\r\n")
        parsed-paper (->> paper
                          (str/split-lines)
                          (mapv #(str/split % #","))
                          (mapv #(mapv (fn [x] (Long/parseLong x)) %)))
        parsed-insrructions (->> insrruction
                                 (str/split-lines)
                                 (mapv #(let [[_ c p] (re-matches #"fold along (\w+)=(\d+)" %)] [c (Long/parseLong p)])))]
    {:points parsed-paper :insrructions parsed-insrructions}))

(defn get-max [m]
  (let [x (apply max (map first m))
        y (apply max (map second m))] [(inc x) (inc y)]))

(assert (= [6 5] (get-max [[1 2] [3 4] [5 0]])))

(defn build-matrix [points]
  (let [[maxx maxy] (get-max points)
        result (vec (for [_ (range maxy)] (vec (for [_ (range maxx)] false))))
        result (loop [p points r result]
                 (if (empty? p)
                   r
                   (recur (rest p) (assoc-in r (reverse (first p)) true))))]
    {:x maxx :y maxy :matrix result}))

(defn fold-matrix [m [d p]]
  (if (= d "y")
    (let [matrix (:matrix m)

          sharps (for [y (range p (:y m))
                       x (range 0 (:x m))
                       :when (get-in matrix [y x])]
                   [y x])
          new-matrix (loop [s sharps r matrix]
                       (if (empty? s)
                         r
                         (recur (rest s) (assoc-in r [(- (* 2 p) (ffirst s)) (second (first s))] true))))] {:x (:x m) :y p :matrix new-matrix})
    (let [matrix (:matrix m)
          _ (println (type (:y m)) (type (:x m)) (type p))

          sharps (for [y (range 0 (:y m))
                       x (range p (:x m))
                       :when (get-in matrix [y x])]
                   [y x])
          new-matrix (loop [s sharps r matrix]
                       (if (empty? s)
                         r
                         (recur (rest s) (assoc-in r [(ffirst s) (- (* 2 p) (second (first s)))] true))))] {:x p :y (:x m) :matrix new-matrix})))

(defn part1 [input]
  (let [matrix (build-matrix (:points input))
        new-matrix (fold-matrix matrix (first (:insrructions input)))]
    (count (for [y (range 0 (:y new-matrix))
                 x (range 0 (:x new-matrix))
                 :when (get-in (:matrix new-matrix) [y x])] true))))


(assert (= 17 (part1 input-from-file-test)))
(assert (= 647 (part1 input-from-file)))


(defn part2 [input]
  (let [matrix (build-matrix (:points input))
        new-matrix (fold-matrix matrix (first (:insrructions input)))]
    (count (for [y (range 0 (:y new-matrix))
                 x (range 0 (:x new-matrix))
                 :when (get-in (:matrix new-matrix) [y x])] true))))

(assert (= 36 (time (part2 input-from-file-test))))
(assert (= 137948 (time (part2 input-from-file))))


