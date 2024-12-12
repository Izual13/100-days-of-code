(ns  aoc2024.d12
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]))



(def test-region (str/split (slurp "resources/aoc2024/d12_t") #"\n"))
(def test-region2 (str/split (slurp "resources/aoc2024/d12_t2") #"\n"))
(def test-region3 (str/split (slurp "resources/aoc2024/d12_t3") #"\n"))
(def region (str/split (slurp "resources/aoc2024/d12_1") #"\n"))

(defn find-neighbours [m [i j]]
  (loop [p [[i j]] n #{} v #{}] 
    (cond 
      (empty? p) n
      (contains? v (first p)) (recur (next p) n v)
      :else (let [[i j] (first p)
                  n-tmp (for [d [[-1 0] [1 0] [0 -1] [0 1]]
                          :let [[x y] d]
                          :when (= (get-in m [i j]) (get-in m [(+ i x) (+ j y)]))] [(+ i x) (+ j y)])
                  ]
              (recur (apply conj (next p) n-tmp) (apply conj n n-tmp) (conj v [i j]))))))

(defn parse-region [m]
  (let[c (count m)]
    (loop [i 0 j 0 r [] v #{}]
      (cond 
        (= j c) r
        (= i c) (recur 0 (inc j) r v)
        (contains? v [i j]) (recur (inc i) j r v) 
        :else (let [s (get-in m [i j])
                    n (find-neighbours m [i j])]
                (recur (inc i) j (conj r [s (conj n [i j])]) (apply conj v (conj n [i j]))))))))

(defn calculate[[region points]]
  [region (count points) (apply + (for [p points
                                   d [[-1 0] [1 0] [0 -1] [0 1]]
                                   :let [[x y] p
                                         [i j] d]
                                   :when (not (contains? points [(+ i x) (+ j y)]))] 1))])

(assert (= 1930 (->> test-region
  (mapv vec)
  (parse-region)
  (mapv calculate)
  (mapv #(let [[r x y] %] (* x y)))
  (apply +))))


(assert (= 1377008 (->> region
  (mapv vec)
  (parse-region)
  (mapv calculate)
  (mapv #(let [[r x y] %] (* x y)))
  (apply +))))


(defn calculate-2 [[region points]]
  [region 
   (count points) 
   (loop[p points sides 0]
     (if (and (empty? p)) 
       sides
       (let [[i j] (first p)
             sides' (if (and 
                          (not (contains? points [(inc i) j]))
                          (not (contains? points [i (dec j)]))) (inc sides) sides)
             sides' (if (and 
                          (not (contains? points [(inc i) j]))
                          (not (contains? points [i (inc j)]))) (inc sides') sides')
             sides' (if (and 
                          (not (contains? points [(dec i) j]))             
                          (not (contains? points [i (inc j)]))) (inc sides') sides')
             sides' (if (and 
                          (not (contains? points [(dec i) j]))
                          (not (contains? points [i (dec j)]))) (inc sides') sides')
             
             sides' (if (and 
                          (contains? points [(inc i) j])
                          (not (contains? points [(inc i) (dec j)]))
                          (contains? points [i (dec j)])) (inc sides') sides')
             sides' (if (and 
                          (contains? points [(inc i) j])
                          (not (contains? points [(inc i) (inc j)]))
                          (contains? points [i (inc j)])) (inc sides') sides')
             sides' (if (and 
                          (contains? points [(dec i) j])
                          (not (contains? points [(dec i) (inc j)]))
                          (contains? points [i (inc j)])) (inc sides') sides')
             sides' (if (and 
                          (contains? points [(dec i) j])
                          (not (contains? points [(dec i) (dec j)]))
                          (contains? points [i (dec j)])) (inc sides') sides')] 
         (recur (next p) sides'))))])
  
(assert (= 1206 (->> test-region
  (mapv vec)
  (parse-region)
  (mapv calculate-2)
  (mapv #(let [[r x y] %] (* x y)))
  (apply +))))


(assert (= 80 (->> test-region2
  (mapv vec)
  (parse-region)
  (mapv calculate-2)
  (mapv #(let [[r x y] %] (* x y)))
  (apply +))))

(assert (= 368 (->> test-region3
  (mapv vec)
  (parse-region)
  (mapv calculate-2)
  (mapv #(let [[r x y] %] (* x y)))
  (apply +))))

(assert (= 815788 (->> region
  (mapv vec)
  (parse-region)
  (mapv calculate-2)
  (mapv #(let [[r x y] %] (* x y)))
  (apply +))))
