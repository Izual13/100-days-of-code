(ns  aoc2024.d20
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof])
   (:use     [utils]))


(def test-racetrack (str/split (slurp "resources/aoc2024/d20_t") #"\n"))
(def racetrack (str/split (slurp "resources/aoc2024/d20") #"\n"))


(defn dijkstra [m mi [si sj] [ei ej]]
  (let [find-neighbours (fn [i j r] (for [[oi oj] [[-1 0][1 0][0 1][0 -1]]
                                        :let [i' (+ i oi)
                                              j' (+ j oj)
                                              x (get-in m [i' j'])]
                                        :when (and (not (nil? x)) (not= x \#))] [i' j' (inc r)]))]
    (loop [points (sorted-set-by (fn [[a1 b1 r1] [a2 b2 r2]] (compare [r1 a1 b1] [r2 a2 b2])) [si sj 0]) v {}]
      (cond 
        (empty? points) (v [ei ej])
        (contains? v [ei ej]) (v [ei ej])
        :else (let [[i j r] (first points)
                    neighbours (filter (fn [[i j r]] (let [v' (v [i j])] (or (nil? v') (< r v')))) (find-neighbours i j r))
                    points' (disj points [i j r])
                    tmp (v [i j])]
                (recur (apply conj points' neighbours) (if (or (nil? tmp) (< r tmp)) (assoc v [i j] r) v)))))))

(defn hack[sp m]
  (let [mi (dec (count m))
        ss (utils/find-start m \S)
        ee (utils/find-start m \E)
        baseline (dijkstra m mi ss ee)
        c (count m)] 
    (loop [j 0 i 0 r 0]
      (cond 
        (= j c) r
        (= i (count (get m j))) (recur (inc j) 0 r)
        (not= \# (get-in m [j i])) (recur j (inc i) r)
        (not (or 
               (and (not= \# (get-in m [j (inc i)]) \#) (not= \# (get-in m [j (dec i)] \#)))
               (and (not= \# (get-in m [(inc j) i]) \#) (not= \# (get-in m [(dec j) i] \#))))) (recur j (inc i) r)
        :else (let [tmp (dijkstra (assoc-in m [j i] \.)  mi ss ee)]
                (if (>= (- baseline tmp) sp)
                  (recur j (inc i) (inc r))
                  (recur j (inc i) r)))))))

(->> test-racetrack
  (mapv vec)
  (hack 20))

(->> racetrack
  (mapv vec)
  (hack 100))

(do
  (println "start profiling")
  (prof/start)
  (time (->> racetrack
          (mapv vec)
          (hack 100)))
  (println (prof/stop))
  (println "end profiling"))
