(ns  hdoc.d48
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]))

;Manasa and Stones

(defn stones [n a b]
  (let [first  (if (< a b) a b)
        second (if (> a b) a b)]
    (loop [n      (dec n)
           result [(* n first)]]
      (if (= n 0) result
          (let [last-value (last result)
                next-value (+ (- last-value first) second)
                result     (if (not= last-value next-value) (conj result next-value) result)]
            (recur (dec n) result))))))



(assert (= [2 3 4] (stones 3 1 2)))
(assert (= [30 120 210 300] (stones 4 10 100)))
(assert (= [30 120 210 300] (stones 4 100 10)))
(assert (= [1800] (stones 73 25 25)))


