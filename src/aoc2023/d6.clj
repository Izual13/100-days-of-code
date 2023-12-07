(ns  aoc2023.d6
  (:require [clojure.string :as str]))


(def document (clojure.string/split (slurp "resources/aoc2023/day6_1") #"\r?\n"))

(def document-t (clojure.string/split (slurp "resources/aoc2023/day6_t") #"\r?\n"))

(defn parse-array [s] 
  (let [r (clojure.string/split s #" +")]
    (mapv #(Long/parseLong %1) r)))

(defn parse-document [[time distance]] 
  (let [[_ t] (re-matches #"Time: +(.*)" time)
        [_ d] (re-matches #"Distance: +(.*)" distance)]
    (mapv vector (parse-array t) (parse-array d))))

(assert (= [[45 305] [97 1062] [72 1110] [95 1695]] (parse-document ["Time:        45     97     72     95" "Distance:   305   1062   1110   1695"])))

(assert (= [45 97 72 95] (parse-array "45     97     72     95")))

(defn count-win [[time distance]] 
  (loop [t time c 0]
    (cond 
      (= t 0) c
      (> (* t (- time t)) distance) (recur (dec t) (inc c))
      :else (recur (dec t) c))))


(assert (= 9 (count-win [30 200])))

(assert (= 288 (->> document-t
                 parse-document
                 (map count-win)
                 (apply *))))

(assert (= 2612736 (->> document
                     parse-document
                     (map count-win)
                     (apply *))))

(assert (= [[6 1] [7 2] [8 3] [9 4]] (map vector [6 7 8 9] [1 2 3 4])))

(assert (= "45977295" (clojure.string/replace "45     97     72     95" #" +" "")))

(defn parse-document-v2 [[time distance]] 
  (let [[_ t] (re-matches #"Time: +(.*)" time)
        [_ d] (re-matches #"Distance: +(.*)" distance)]
    [(Long/parseLong (clojure.string/replace t #" +" "")) (Long/parseLong (clojure.string/replace d #" +" ""))]))

(defn count-win-v2 [[time distance]] 
  (let [d (- (* time time) (* 4 distance))
        x (int (/ (- time (Math/sqrt d)) 2))]
    (int (- time x x 1))))

(assert (= 71503 (->> document-t
                   parse-document-v2
                   count-win)))

(assert (= 29891250 (->> document
                      parse-document-v2
                      count-win)))

(assert (= 71503 (->> document-t
                   parse-document-v2
                   count-win-v2)))

(assert (= 29891250 (->> document
                      parse-document-v2
                      count-win-v2)))