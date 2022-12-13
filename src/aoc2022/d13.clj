(ns aoc2022.d13
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(def test-input (clojure.string/split (slurp "resources/aoc2022/day13_t") #"\n\n"))
(def input (clojure.string/split (slurp "resources/aoc2022/day13_1") #"\n\n"))

(defn parse-input [r] 
  (let [lines (clojure.string/split r #"\n")]
    [(read-string (lines 0)) (read-string (lines 1))]))


(defn compare-packets 
  ([p] (let[[l r] p]
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


(assert (= false (compare-packets [[[[3]],9]] [[],[2],[]])))
(assert (= false (compare-packets [[[]]] [[]])))
(assert (= true (compare-packets [[]] [[[]]])))


(assert (= true (compare-packets [1,1,3,1,1] [1,1,5,1,1])))
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

(defn add-dividers [l] 
  (apply conj l [[[2]] [[6]]]))

(defn find-dividers [l]
  (loop [l l i 1 r 1] 
    (cond 
      (empty? l) r
      (= (first l) [[2]]) (recur (rest l) (inc i) (* r i))
      (= (first l) [[6]]) (recur (rest l) (inc i) (* r i))
      :else (recur (rest l) (inc i) r))))

(assert (= 6 (find-dividers [0 [[2]] [[6]]])))

(assert (= 140 (->> test-input 
                 (mapcat parse-input)
                 add-dividers
                 (sort #(let [c (compare-packets %1 %2)]
                          (cond
                            (nil? c) 0
                            (= true c) -1
                            :else 1)))
                 find-dividers)))

(assert (= 21836 (->> input 
                   (mapcat parse-input)
                   add-dividers
                   (sort #(let [c (compare-packets %1 %2)]
                            (cond
                              (nil? c) 0
                              (= true c) -1
                              :else 1)))
                   find-dividers)))
