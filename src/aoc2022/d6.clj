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

(defn optimized-find-marker [s c]
  (let [max-b (count s) offset (int \a)] 
    (loop [b 0 m (vec (for [i (range 26)] 0))]
      (if (= b max-b)
        nil
        (cond 
          (< b c) (recur (inc b) (update m (- (int (get s b)) offset) inc))
          (every? #(>= 1 %) m) b
          :else (recur (inc b) (-> m
                                 (update (- (int (get s (- b c))) offset) dec)
                                 (update (- (int (get s b)) offset) inc))))))))

(assert (= 7 (find-marker test-input 4)))
(assert (= 1080 (find-marker input 4)))

(assert (= 19 (find-marker test-input 14)))
(assert (= 3645 (find-marker input 14)))

(assert (= 7 (optimized-find-marker test-input 4)))
(assert (= 1080 (optimized-find-marker input 4)))

(assert (= 19 (optimized-find-marker test-input 14)))
(assert (= 3645 (optimized-find-marker input 14)))

(time (optimized-find-marker (vec input) 4))
(time (find-marker input 4))

(do
  (vec (for [i (range 1000)] (optimized-find-marker input 4)))
  (println "start profiling")
  (prof/start)
  (optimized-find-marker input 4)
  (println (prof/stop))
  (println "end profiling"))
