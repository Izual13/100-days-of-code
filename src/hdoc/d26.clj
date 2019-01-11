(ns  hdoc.d26
  (:require
   [clojure.string :as str]))

;Jumping on the Clouds

(defn jumpingOnClouds [c]
  (let [length (count c)]
    (loop [i      0
           result 0] 
      (if (= (inc i) length)
        result
        (if (= (+ 2 i) length) 
          (recur (inc i) (inc result))
          (recur (+ i (if (= 1 (nth c (+ 2 i))) 1 2)) (inc result)))))))
  

  (jumpingOnClouds [0 0 1 0 0 1 0])
  


(assert (= 4  (jumpingOnClouds [0 0 0 0 1 0])))
(assert (= 4  (jumpingOnClouds [0 0 1 0 0 1 0])))
(assert (= 3  (jumpingOnClouds [0 0 0 1 0 0])))
