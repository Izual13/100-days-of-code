(ns  hdoc.d40
  (:require
   [clojure.string :as str]))

;Service Lane


(defn serviceLane [width cases]
  (for [r cases] 
    (apply min 
           (for [i (range (first r) (inc (last r)))]
             (get width i)))))


(serviceLane [2 3 1 2 3 2 3 3] [[0 3] [4 6] [6 7] [3 5] [0 7]])
(serviceLane [1 2 2 2 1] [[2 3] [1 4] [2 4] [2 4] [2 3]])




(assert (= [1 2 3 2 1] (serviceLane [2 3 1 2 3 2 3 3] [[0 3] [4 6] [6 7] [3 5] [0 7]])))
(assert (= [2 1 1 1 2] (serviceLane [1 2 2 2 1] [[2 3] [1 4] [2 4] [2 4] [2 3]])))
