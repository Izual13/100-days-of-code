(ns  hdoc.d23
  (:require
   [clojure.string :as str]))

;Library Fine

(defn libraryFine [d1 m1 y1 d2 m2 y2]
  (cond 
    (> y1 y2) 10000
    (< y1 y2) 0
    (> m1 m2) (* 500 (- m1 m2))
    (< m1 m2) 0
    (> d1 d2) (* 15 (- d1 d2))
    :else 0
    ))

(assert (= 45 (libraryFine 9 6 2015 6 6 2015)))
(assert (= 0 (libraryFine 6 6 2015 9 6 2016)))
(assert (= 0 (libraryFine 2 7 1014 1 1 1015)))

