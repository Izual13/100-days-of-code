(ns  hdoc.d26
  (:require
   [clojure.string :as str]))

;Jumping on the Clouds

(defn jumpingOnClouds [c]
  (let [length (count c)]
    (loop [i      0
           result 0] 
      (println i ": " result)
      (if (= (inc i) length)
        result
        (let [i2 (+ i 2)]
          (if (or (= i2 length) (= 1 (nth c i2)))
            (recur (inc i) (inc result))
            (recur i2 (inc result))))))))
  

  (jumpingOnClouds [0 0 1 0 0 1 0])
  


(assert (= 3  (jumpingOnClouds [0 0 0 0 1 0])))
(assert (= 4  (jumpingOnClouds [0 0 1 0 0 1 0])))
(assert (= 3  (jumpingOnClouds [0 0 0 1 0 0])))
