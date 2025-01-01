(ns  aoc2024.d18
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof])
   (:use     [utils]))


(def test-corrupted-bytes (str/split (slurp "resources/aoc2024/d18_t") #"\n"))
(def corrupted-bytes (str/split (slurp "resources/aoc2024/d18") #"\n"))


(defn parse-byte [s]
  (let [[_ y x] (re-matches #"(\d*),(\d*)" s)]
        [(Integer/parseInt x) (Integer/parseInt y)]))

(defn create-map [s m] 
  (let [init (vec (for [i (range s)]
         (vec (for [j (range s)] \.))))]
    (reduce (fn [acc x] (assoc-in acc x \#)) init m)))

(defn dijkstra [m]
  (let [find-neighbours (fn [i j r] (for [[oi oj] [[-1 0][1 0][0 1][0 -1]]
                                        :let [x (get-in m [(+ i oi) (+ j oj)])]
                                        :when (and (not (nil? x)) (not= x \#))] [(+ i oi) (+ j oj) (inc r)]))]
    (loop [points #{[0 0 0]} v {}]
      (if (empty? points)
        (v [(dec (count m)) (dec (count m))])
        (let [[i j r] (first points)
              neighbours (filter #(or (nil? (v [(first %) (second %)]))
                                    (< (get % 2) (v [(first %) (second %)]))
                                    ) (find-neighbours i j r))
              points' (disj points [i j r])
              tmp (v [i j])]
          (recur (apply conj points' neighbours) (if (or (nil? tmp) (< r tmp)) (assoc v [i j] r) v)))))))


(assert (= 22 (->> test-corrupted-bytes
  (mapv parse-byte)
  (take 12)
  (create-map 7)
  dijkstra)))


(assert (= 288 (->> corrupted-bytes
  (mapv parse-byte)
  (take 1024)
  (create-map 71)
  dijkstra)))

