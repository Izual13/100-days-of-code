(ns  hdoc.d7
  (:require
   [clojure.string :as str]))

;Counting Valleys

(defn countingValleys [n s]
  (loop [s (seq s)
         count 0
         level 0]
    (println "count: " count)
    (println "level: " level)
    (println count)
    (if (empty? s)
      count
      (let [f (first s)
            up (= f \U)]
      (recur (next s)  
             (if (and (= level -1) up) (inc count) count)
             (if up (inc level) (dec level)))))))

 (countingValleys 8 "DDUUDDUDUUUD")

(assert (= 1 (countingValleys 8 "UDDDUDUU")))
(assert (= 2 (countingValleys 8 "DDUUDDUDUUUD")))
