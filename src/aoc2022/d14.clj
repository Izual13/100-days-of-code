(ns aoc2022.d14
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(def test-input (str/split (slurp "resources/aoc2022/day14_t") #"\n"))

(def input (str/split (slurp "resources/aoc2022/day14_1") #"\n"))


(defn parse-input [r] 
  (let [c (str/split r #" -> ")
        ]
    (->> c
      (mapv #(str/split % #","))
      (mapv (fn [[x y]] {:x (Integer/parseInt x) :y (Integer/parseInt y)})))))


(assert (= [{:x 503, :y 4} {:x 502, :y 4} {:x 502, :y 9} {:x 494, :y 9}] (parse-input "503,4 -> 502,4 -> 502,9 -> 494,9")))

(defn get-max [x]
  (loop [c (flatten x) min-x 10000 min-y 10000  max-x 0 max-y 0]
    (if (empty? c) 
      [(dec min-x) (min 0 min-y) (inc max-x) (inc max-y)]
      (let [f (first c)
            new-min-x (if (< (:x f) min-x) (:x f) min-x)
            new-min-y (if (< (:y f) min-y) (:y f) min-y)
            new-max-x (if (> (:x f) max-x) (:x f) max-x)
            new-max-y (if (> (:y f) max-y) (:y f) max-y)]
        (recur (rest c) new-min-x new-min-y new-max-x new-max-y)))))

(defn create-rock-line [cave lines offset-x offset-y] 
  (loop [l lines c cave]
    (if (<= (count l) 1) 
      c
      (let [p1 (first l)
            p2 (second l)
            
            x1 (:x p1)
            y1 (:y p1)
            x2 (:x p2)
            y2 (:y p2)
            new-c (loop [x x1 y y1 c c]
                    (cond 
                      (and (= x x2) (= y y2)) (assoc-in c [(- y offset-y) (- x offset-x)] \#)
                      (< x x2) (recur (inc x) y (assoc-in c [(- y offset-y) (- x offset-x)] \#)) 
                      (> x x2) (recur (dec x) y (assoc-in c [(- y offset-y) (- x offset-x)] \#))
                      (< y y2) (recur x (inc y) (assoc-in c [(- y offset-y) (- x offset-x)] \#))
                      (> y y2) (recur x (dec y) (assoc-in c [(- y offset-y) (- x offset-x)] \#))
                      :else c))] 
        (recur (rest l) new-c)))))

(defn build-cave [scans]
  (let [[min-x min-y max-x max-y] (get-max scans)
        cave (vec (for [y (range min-y (inc max-y))]
                    (vec (for [x (range min-x (inc max-x))]
                           \.))))]
    (loop [s scans c cave]
      (if (empty? s) 
        {:c c :min-x min-x :min-y min-y :max-x max-x :max-y max-y}
        (recur (rest s) (create-rock-line c (first s) min-x min-y))))))

(defn execution [cave] 
  (let [min-x (:min-x cave)
        min-y (:min-y cave)
        max-x (:max-x cave)
        max-y (:max-y cave)]
    (loop [p [0 (- 500 min-x)] i 0 c (:c cave)]
      (let [[y x] p]
        (cond 
          (= (get-in c [(inc y) x]) \.) (recur [(inc y) x] i c)
          (= (get-in c [(inc y) (dec x)]) \.) (recur [(inc y) (dec x)] i c)
          (= (get-in c [(inc y) (inc x)]) \.) (recur [(inc y) (inc x)] i c)
          (not= (get-in c [0 (- 500 min-x)]) \.) i
          (= y max-y) i
          :else (recur [0 (- 500 min-x)] (inc i) (assoc-in c [y x] \o)))))))

(assert (= 24 (->> test-input
                (mapv parse-input)
                build-cave
                execution)))


(assert (= 799 (->> input
                 (mapv parse-input)
                 build-cave
                 execution)))

(defn add-bottom-line [rows] 
  (let [[min-x min-y max-x max-y] (get-max rows)]
    (conj rows [{:x 1 :y (+ 1 max-y)} {:x 1000 :y (+ 1 max-y)}])))

(assert (= 93 (->> test-input
                (mapv parse-input)
                add-bottom-line
                build-cave
                execution)))

(assert (= 29076 (->> input
                   (mapv parse-input)
                   add-bottom-line
                   build-cave
                   execution)))

