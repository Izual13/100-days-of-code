(ns  hdoc.d32
  (:require
   [clojure.string :as str]))

;Minimum Distances

(defn minimumDistances [a]
  (let [length (count a)
        mins   (for [i     (range length)
                     j     (range length 0 -1)
                     :when (and (< i j) (= (get a i) (get a j)))] (Math/abs (- i j)))]
    (if (empty? mins) -1 (apply min mins))))




(assert (= 3 (minimumDistances [7 1 3 4 1 7])))
(assert (= -1 (minimumDistances [1 2 3 4 10])))
(assert (= 1 (minimumDistances [1 1])))
