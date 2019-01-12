(ns  hdoc.d27
  (:require
   [clojure.string :as str]))

;Equalize the Array

(defn equalizeArray [arr]
  (let [f      (frequencies arr)
        max (val (apply max-key val f))
        result (- (count arr) max)]
    result))


(assert (= 2 (equalizeArray [3 3 2 1 3])))
