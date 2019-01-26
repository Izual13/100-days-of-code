(ns  hdoc.d38
  (:require
   [clojure.string :as str]))

;Bigger is Greater

(defn rotate [v d]
  (let [length (count v)]
  (for [i (range length)] 
    (get v (mod (+ i d) length)))))


(reduce #(str %1 " " %2) (rotate [1 2 3 4 5] 4))


(apply println [1 2 3])

(assert (= [5 1 2 3 4] (rotate [1 2 3 4 5] 4)))
