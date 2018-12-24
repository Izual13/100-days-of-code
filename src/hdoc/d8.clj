(ns  hdoc.d8
  (:require
   [clojure.string :as str]))

;Electronics Shop


(defn getMoneySpent [keyboards drives b]
(apply max (filter #(<= % b) (into [-1] (for [k keyboards 
               d drives] 
    (+ k d))))))


(assert (= 9 (getMoneySpent [3 1] [5 2 8] 10)))
(assert (= -1 (getMoneySpent [5] [4] 5)))