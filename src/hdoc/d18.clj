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


;Find Digits

(defn findDigits [n]
  (loop [ reminder n
         result   0]
    (let [x (mod reminder 10)]
      (if (= reminder 0)
        result
        (recur (quot reminder 10) (+ result (if (and (not (= 0 x)) (= 0 (mod n x))) 1 0)))))))

(assert (= 2 (findDigits 12)))
(assert (= 3 (findDigits 1012)))  
(assert (= 3 (findDigits 123456789)))
(assert (= 3 (findDigits 114108089)))
(assert (= 0 (findDigits 0)))
(assert (= 5 (findDigits 106108048)))