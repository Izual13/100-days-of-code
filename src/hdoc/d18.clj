(ns  hdoc.d18
  (:require
   [clojure.string :as str]))

;Jumping on the Clouds: Revisited

(defn jumpingOnClouds [c k]
  (let [size (count c)] 
    (loop [i k
           result 100] 
      (if (= i size)
        (- result 1 (if (= 1 (nth c 0)) 2 0))
        (recur (+ i k) (- result 1 (if (= 1 (nth c i)) 2 0)))))))



(assert (= 92 (jumpingOnClouds [0 0 1 0 0 1 1 0] 2)))
(assert (= 92 (jumpingOnClouds [1 1 1 0 1 1 0 0 0 0] 3)))
