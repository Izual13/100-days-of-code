(ns  aoc2024.d12
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]))


(def test-region (str/split (slurp "resources/aoc2024/d12_t") #"\n"))
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
                    n (find-neighbours m [i j])
                    ]
                (recur (inc i) j (conj r [s (conj n [i j])]) (apply conj v (conj n [i j]))))))))

(defn calculate[[region points]]
  (let[all-i (mapv first points)
       all-j (mapv second points)
       max-i (apply max all-i)
       min-i (apply min all-i)
       max-j (apply max all-j)
       min-j (apply min all-j)] 
    [region (count points) (cond
              :else (apply + (for [p points
                                   d [[-1 0] [1 0] [0 -1] [0 1]]
                                   :let [[x y] p
                                         [i j] d]
                                   :when (not (contains? points [(+ i x) (+ j y)]))] 1)))]))

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
