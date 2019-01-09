(ns  hdoc.d24
  (:require
   [clojure.string :as str]))

;Cut the sticks

(defn cutTheSticks [arr]
(loop [s (sort arr)
       result [(count s)]] 
  (if (empty? s)
    result
    (let [f (first s)
          s (map #(- % f) (filter #(> % f) s))]
      (if (empty? s) result
          (recur s (conj result (count s))))))))


(assert (= [8 6 4 1] (cutTheSticks [1 2 3 4 3 3 2 1])))