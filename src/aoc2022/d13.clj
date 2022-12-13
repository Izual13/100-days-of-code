(ns aoc2022.d13
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(def test-input (clojure.string/split (slurp "resources/aoc2022/day13_t") #"\n\n"))
(def input (clojure.string/split (slurp "resources/aoc2022/day13_1") #"\n\n"))

(defn parse-input [r] 
  (let [lines (clojure.string/split r #"\n")]
    {:left (read-string (lines 0)) :right (read-string (lines 1))}))


(defn compare-packets 
  ([p] (let[l (:left p)
            r (:right p)]
         (compare-packets l r)))
  
  ([l r] 
   (cond
     (and (not (vector? l)) (not (vector? r)) (> r l)) true 
     (and (not (vector? l)) (not (vector? r)) (< r l)) false
     (and (not (vector? l)) (not (vector? r)) (= r l)) nil
     (and (not (vector? r)) (vector? l)) (recur l [r])
     (and (not (vector? l)) (vector? r)) (recur [l] r)    
     (and (empty? l) (not (empty? r))) true
     (and (empty? r) (not (empty? l))) false
     (and (empty? r) (empty? l)) nil
     
     (nil? (compare-packets (first l) (first r))) (recur (vec (rest l)) (vec (rest r)))
     (= true (compare-packets (first l) (first r))) true
     (= false (compare-packets (first l) (first r))) false
     :else false )))




(assert (= false (compare-packets {:left  [[[[3]],9]] :right [[],[2],[]]})))
(assert (= false (compare-packets {:left [[[]]] :right [[]]})))
(assert (= true (compare-packets {:left [[]] :right [[[]]]})))


(assert (= true (compare-packets {:left [1,1,3,1,1] :right [1,1,5,1,1]})))
(assert (= true (compare-packets [2 3 4] 4)))

(assert (= 13 (->> test-input 
                (map parse-input)
                (map compare-packets)
                (map-indexed #(if (= true %2) (inc %1) nil))
                (filter #(not(nil? %)))
                (reduce +))))


(assert (= 4734 (->> input 
                  (map parse-input)
                  (map compare-packets)
                  (map-indexed #(if (= true %2) (inc %1) nil))
                  (filter #(not(nil? %)))
                  (reduce +))))






