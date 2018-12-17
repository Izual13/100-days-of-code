(ns  hdoc.d1
  (:require 
   [clojure.data.json :as json]))

;Breaking the Records

(defn breakingRecords [scores] 
  (let [f (first scores)
        r (rest scores)]
      (loop [max f
             min f
             r r
             hs 0
             ls 0] 
        (if (empty? r) 
          [hs ls]
          (let [f (first r)]
            (cond 
              (< max f) (recur f min (rest r) (inc hs) ls)
              (> min f) (recur max f (rest r) hs (inc ls))
              :else (recur max min (rest r) hs ls)))))))

(assert (= [4 0] (breakingRecords [3 4 21 36 10 28 35 5 24 42])))
(assert (= [2 4] (breakingRecords [10 5 20 20 4 5 2 25 1])))

