(ns  hdoc.d45
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]))

;Cavity Map

(defn cavityMap [grid]
  (let [n (- (count grid) 1)]
    (for [i (range 1 n)
          j (range 1 n)] [i j])))



(cavityMap [1112 1912 1892 1234])

(assert (= 1 (find-index [2 8 10 13 18 25] 8)))