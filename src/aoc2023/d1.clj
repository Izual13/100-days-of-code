(ns  aoc2023.d1
  (:require [clojure.string :as str]))

(def doc (clojure.string/split (slurp "resources/aoc2023/day1_1") #"\r?\n"))

(def doc-t (clojure.string/split (slurp "resources/aoc2023/day1_t") #"\r?\n"))

(def doc-t2 (clojure.string/split (slurp "resources/aoc2023/day1_t2") #"\r?\n"))

(defn calibrate [s] 
  (loop [s s f nil l nil]
    (if (empty? s) (+ (* 10 f) (if (nil? l) f l))
      (let [x (- (int (first s)) 48)]
        (cond 
          (not (< -1 x 10)) (recur (next s) f l)
          (nil? f) (recur (next s) x l)
          :else (recur (next s) f x))))))

(assert (= 54304
          (reduce + (map #(calibrate (vec %1)) doc))))

(assert (= 142
          (reduce + (map #(calibrate (vec %1)) doc-t))))



(defn calibrate-v2 [s] 
  (let [c (count s)]
    (loop [i 0 f nil l nil]
    (if (= i c) (+ (* 10 f) (if (nil? l) f l))
     (let [x (get s i)
           x (cond 
          (< 47 (int x) 58) (- (int x) 48)
          (.startsWith s "one" i) 1
          (.startsWith s "two" i) 2
          (.startsWith s "three" i) 3
          (.startsWith s "four" i) 4
          (.startsWith s "five" i) 5
          (.startsWith s "six" i) 6
          (.startsWith s "seven" i) 7
          (.startsWith s "eight" i) 8
          (.startsWith s "nine" i) 9
       :else nil)]
       (cond 
         (nil? x) (recur (inc i) f l)
         (nil? f) (recur (inc i) x l)
         :else (recur (inc i) f x)))))))

(assert (= 281 (reduce + (map calibrate-v2 doc-t2))))

(assert (= 54418 (reduce + (map calibrate-v2 doc))))
