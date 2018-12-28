(ns  hdoc.d12
  (:require
   [clojure.string :as str]))

;The Hurdle Race

(defn hurdleRace [k height]
  (def max-height (apply max height))
  (if (< k max-height) (- max-height k) 0))

(assert (= 2 (hurdleRace 4 [1 6 3 5 2])))
(assert (= 0 (hurdleRace 7 [2 5 4 5 2])))