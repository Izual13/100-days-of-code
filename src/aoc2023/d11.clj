(ns  aoc2023.d11
  (:require [clojure.string :as str]))

(def image (clojure.string/split (slurp "resources/aoc2023/day11_1") #"\r?\n"))

(def image-t (clojure.string/split (slurp "resources/aoc2023/day11_t") #"\r?\n"))

(defn parse-array [s] 
  (mapv vec s))

(->> image-t
  parse-array)

(defn find-galaxy [a]
  (let [c (count a)]
    (loop [j 0 i 0 r []]
      (cond 
        (= j c) r
        (= i c) (recur (inc j) 0 r)
        (= \# (get-in a [j i])) (recur j (inc i) (conj r [j i]))
        :else (recur j (inc i) r)))))

(defn find-empty [a]
  (let [c (count a)]
    [(loop [j 0 r #{}]
       (cond 
         (= j c) r
         (= (count (frequencies (get a j))) 1) (recur (inc j) (conj r j))
         :else (recur (inc j) r)))
     
     (loop [j 0 r #{}]
       (cond 
         (= j c) r
         (= (count (frequencies (for [i (range c)] (get-in a [i j])))) 1) (recur (inc j) (conj r j))
         :else (recur (inc j) r)))
     ]))

(defn calc [[points [rows collumns]]]
  (let [c (count points)]
    (for [i (range c)]
      (for [j (range i)] 
        (let [[x1 y1] (get points i)
              [x2 y2] (get points j)
              additional-rows (apply + (for [row rows
                                             :when (or (< x1 row x2) (> x1 row x2))] 1))
              additional-collumns (apply + (for [collumn collumns
                                                 :when (or (< y1 collumn y2) (> y1 collumn y2))] 1))]
          (+ (abs (- x1 x2)) (abs (- y1 y2)) additional-rows additional-collumns))))))



(assert (= 374 (->> image-t
                 parse-array
                 ((juxt find-galaxy find-empty))
                 calc
                 flatten
                 (apply +))))

(assert (= 9536038 (->> image
                     parse-array
                     ((juxt find-galaxy find-empty))
                     calc
                     flatten
                     (apply +))))



(defn calc-v2 [[points [rows collumns]]]
  (let [c (count points)]
    (for [i (range c)]
      (for [j (range i)] 
        (let [[x1 y1] (get points i)
              [x2 y2] (get points j)
              additional-rows (count (for [row rows
                                           :when (or (<= x1 row x2) (>= x1 row x2))] 1))
              additional-collumns (count (for [collumn collumns
                                               :when (or (<= y1 collumn y2) (>= y1 collumn y2))] 1))]          
          (+ (abs (- x1 x2)) (abs (- y1 y2)) (* (+ additional-rows additional-collumns) 999999)
            ))))))


(assert (= 82000210 (->> image-t
                      parse-array
                      ((juxt find-galaxy find-empty))
                      calc-v2
                      flatten
                      (apply +))))

(assert (= 447744640566 (->> image
                          parse-array
                          ((juxt find-galaxy find-empty))
                          calc-v2
                          flatten
                          (apply +))))
