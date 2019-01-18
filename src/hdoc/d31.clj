(ns  hdoc.d31
  (:require
   [clojure.string :as str]))

;Beautiful Triplets


(defn beautifulTriplets [d arr]
  (let [s (set arr)] 
    (println s)
    (count (for [i arr 
          :when  (and (contains? s (+ i d)) (contains? s (+ i d d)))] i))))

(assert (= 3 (beautifulTriplets 1 [2 2 3 4 5])))
(assert (= 3 (beautifulTriplets 3 [1 2 4 5 7 8 10])))
(assert (= 2 (beautifulTriplets 3 [1 6 7 7 8 10 12 13 14 19])))
