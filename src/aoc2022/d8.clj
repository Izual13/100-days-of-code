(ns aoc2022.d8
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(def test-input
  (->> (slurp "resources/aoc2022/day8_t")
    (str/split-lines)
    (mapv #(str/split % #""))
    (mapv #(mapv (fn [x] (Long/parseLong x)) %))))

(def input
  (->> (slurp "resources/aoc2022/day8_1")
    (str/split-lines)
    (mapv #(str/split % #""))
    (mapv #(mapv (fn [x] (Long/parseLong x)) %))))

(assert (= 1792 (let[m input
                     y (count m)
                     x (count (first m))
                     ]
                  (println "\n\n\n\n")
                  (+ y x x y -4 (count (for [i (range 1 (dec x))
                                             j (range 1 (dec y))
                                             :let [e (get-in m [i j])]
                                             :when (or 
                                                     (every? #(> e (get-in m [% j])) (range 0 i))
                                                     (every? #(> e (get-in m [% j])) (range (inc i) x))
                                                     (every? #(> e (get-in m [i %])) (range 0 j))
                                                     (every? #(> e (get-in m [i %])) (range (inc j) y)))]
                                         [i j]))))))



(assert (= 8 (let[m test-input
                  y (count m)
                  x (count (first m))]
               (reduce max (for [i (range 1 (dec x))
                                 j (range 1 (dec y))
                                 :let [e (get-in m [j i])
                                       s1 (for [i2 (range (dec i) -1 -1) :while (> e (get-in m [j i2]))]  1)
                                       s2 (for [i2 (range (inc i) x) :while (> e (get-in m [j i2]))]  [j i2 e (get-in m [i2 j])])
                                       s3 (for [j2 (range (dec j) -1 -1) :while (> e (get-in m [j2 i]))]  1)
                                       s4 (for [j2 (range (inc j) y) :while (> e (get-in m [j2 i]))] [j2 i (get-in m [j2 i])])
              
                                       s1 (+ (count s1) (if (= (count s1) i) 0 1))
                                       s2 (+ (count s2) (if (= (count s2) (- x i 1)) 0 1))
                                       s3 (+ (count s3) (if (= (count s3) j) 0 1))
                                       s4 (+ (count s4) (if (= (count s4) (- y j 1)) 0 1))]]
                             (* s1 s2 s3 s4))))))

(assert (= 334880 (let[m input
                       y (count m)
                       x (count (first m))
                       ]
                    (reduce max (for [i (range 1 (dec x))
                                      j (range 1 (dec y))
                                      :let [e (get-in m [j i])
                                            s1 (for [i2 (range (dec i) -1 -1) :while (> e (get-in m [j i2]))]  1)
                                            s2 (for [i2 (range (inc i) x) :while (> e (get-in m [j i2]))]  [j i2 e (get-in m [i2 j])])
                                            s3 (for [j2 (range (dec j) -1 -1) :while (> e (get-in m [j2 i]))]  1)
                                            s4 (for [j2 (range (inc j) y) :while (> e (get-in m [j2 i]))] [j2 i (get-in m [j2 i])])
              
                                            s1 (+ (count s1) (if (= (count s1) i) 0 1))
                                            s2 (+ (count s2) (if (= (count s2) (- x i 1)) 0 1))
                                            s3 (+ (count s3) (if (= (count s3) j) 0 1))
                                            s4 (+ (count s4) (if (= (count s4) (- y j 1)) 0 1))]]
                                  (* s1 s2 s3 s4))))))

