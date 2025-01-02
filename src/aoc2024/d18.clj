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

(defn optimised-dijkstra [m]
  (let [mi (dec (count m))
        find-neighbours (fn [i j r] (for [[oi oj] [[-1 0][1 0][0 1][0 -1]]
                                        :let [i' (+ i oi)
                                              j' (+ j oj)
                                              x (get-in m [i' j'])]
                                        :when (and (not (nil? x)) (not= x \#))] [i' j' (inc r)]))]
    (loop [points (sorted-set-by (fn [[a1 b1 r1] [a2 b2 r2]] (compare [r1 a1 b1] [r2 a2 b2])) [0 0 0]) v {}]
      (cond 
        (empty? points) (v [mi mi])
        (not (nil? (v [mi mi]))) 1
        :else (let [[i j r] (first points)
                    neighbours (filter (fn [[i j r]] (let [v' (v [i j])] (or (nil? v') (< r v')))) (find-neighbours i j r))
                    points' (disj points [i j r])
                    tmp (v [i j])]
                (recur (apply conj points' neighbours) (if (or (nil? tmp) (< r tmp)) (assoc v [i j] r) v)))))))

(defn bruteforce[init size cb]
  (let [m (create-map size (take init cb))] 
    (loop [m m c (drop init cb) r init]
      (cond
        (empty? c) r
        (nil? (optimised-dijkstra m)) (get cb (dec r))
        :else (recur (assoc-in m [(first (first c)) (second (first c))] \#) (next c) (inc r))))))

(assert (= "6,1" (->> test-corrupted-bytes
  (mapv parse-byte)
  (bruteforce 12 7)
  reverse
  (str/join ","))))

(assert (= "52,5" (->> corrupted-bytes
  (mapv parse-byte)
  (bruteforce 1024 71)
  reverse
  (str/join ","))))


(do
  (println "start profiling")
  (prof/start)
  (time (->> corrupted-bytes
  (mapv parse-byte)
  (bruteforce 1024 71)))
  (println (prof/stop))
  (println "end profiling"))


"Elapsed time: 172684.210037 msecs"
"Elapsed time: 189915.364074 msecs"
"Elapsed time: 115795.362976 msecs"
"Elapsed time: 17240.556196 msecs"

