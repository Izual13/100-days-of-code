(ns  aoc2024.d16
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof])
   (:use     [utils]))

(def test-maze (str/split (slurp "resources/aoc2024/d16_t") #"\n"))
(def test-maze2 (str/split (slurp "resources/aoc2024/d16_t2") #"\n"))
(def maze (str/split (slurp "resources/aoc2024/d16_1") #"\n"))

(utils/find-start [[1 2 3 4]] 4)

(defn direction [d]
  (case d
    \^ [-1 0]
    \v [1 0]
    \> [0 1]
    \< [0 -1]))

(defn find-neighbours [i j m cd cr v]
  (case cd
    \^ (for [[oy ox r nd] [[-1 0 0 \^][1 0 2000 \v][0 1 1000 \>][0 -1 1000 \<]]
             :let [i' (+ i oy)
                   j' (+ j ox)
                   f (get-in m [i' j'])
                   r' (+ cr r 1)]
             :when (and (not (= f \#)) (or (nil? (v [i' j'])) (< r' (v [i' j']))))] [i' j' nd r'])
    \v (for [[oy ox r nd] [[-1 0 2000 \^][1 0 0 \v][0 1 1000 \>][0 -1 1000 \<]]
             :let [i' (+ i oy)
                   j' (+ j ox)
                   f (get-in m [i' j'])
                   r' (+ cr r 1)]
             :when (and (not (= f \#)) (or (nil? (v [i' j'])) (< r' (v [i' j']))))] [i' j' nd r'])
    \> (for [[oy ox r nd] [[-1 0 1000 \^][1 0 1000 \v][0 1 0 \>][0 -1 2000 \<]]
             :let [i' (+ i oy)
                   j' (+ j ox)
                   f (get-in m [i' j'])
                   r' (+ cr r 1)]
             :when (and (not (= f \#)) (or (nil? (v [i' j'])) (< r' (v [i' j']))))] [i' j' nd r'])
    \< (for [[oy ox r nd] [[-1 0 1000 \^][1 0 1000 \v][0 1 2000 \>][0 -1 0 \<]]
             :let [i' (+ i oy)
                   j' (+ j ox)
                   f (get-in m [i' j'])
                   r' (+ cr r 1)]
             :when (and (not (= f \#)) (or (nil? (v [i' j'])) (< r' (v [i' j']))))] [i' j' nd r'])
    ))
  
(defn dijkstra [m]
  (let [[si sj] (utils/find-start m \S)
        [ei ej] (utils/find-start m \E)
        _ (println "e->" ei ej)]
    (loop [points [[si sj \< 0]] v {}]
      (if (empty? points)
        (v [ei ej])
        (let [[i j d r] (first points)
              ; _ (println (first points))
              neighbours (find-neighbours i j m d r v)
              points' (next points)
              tmp (v [i j])]
          (recur (apply conj points' neighbours) (if (or (nil? tmp) (< r tmp)) (assoc v [i j] r) v)))))))


(->> test-maze
  (mapv vec)
  dijkstra)

(->> test-maze2
  (mapv vec)
  dijkstra)

(->> maze
  (mapv vec)
  dijkstra)


