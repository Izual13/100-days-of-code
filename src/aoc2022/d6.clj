(ns aoc2022.d6
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))

(def test-input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def input (slurp "resources/aoc2022/day6_1"))

(defn find-marker [s c] 
  (loop [b 0] (let [min-c (min b (- (count s) c))]
                (if (or (< b min-c) (= c (count (frequencies (subs s b (+ c min-c))))))
                  (+ b c)
                  (recur (inc b))))))

(defn mremove [m c] 
  (if (= (get m c) 1) 
    (dissoc m c)
    (update m c dec)))

(defn optimized-find-marker [s c] 
  (loop [b 0 max-b (count s) m {}]
    (let [count-keys (count m)]
      (if (= b max-b)
        nil
        (cond 
          (= count-keys c) b
          (>= b c) (recur (inc b) max-b (-> m
                                          (mremove (get s (- b c)))
                                          (update (get s b) (fnil inc 0))))
          (< b c) (recur (inc b) max-b (update m (get s b) (fnil inc 0))))))))


(assert (= 7 (find-marker test-input 4)))
(assert (= 1080 (find-marker input 4)))

(assert (= 19 (find-marker test-input 14)))
(assert (= 3645 (find-marker input 14)))


(assert (= 7 (optimized-find-marker test-input 4)))
(assert (= 1080 (optimized-find-marker input 4)))

(assert (= 19 (optimized-find-marker test-input 14)))
(assert (= 3645 (optimized-find-marker input 14)))

(time (optimized-find-marker input 4))

(do
  (vec (for [i (range 1000)] (optimized-find-marker input 4)))
  (println "start profiling")
  (prof/start)
  (optimized-find-marker input 4)
  (println (prof/stop))
  (println "end profiling"))
