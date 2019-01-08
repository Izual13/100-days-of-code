(ns  hdoc.d22
  (:require
   [clojure.string :as str]))

;Save the Prisoner!

(defn saveThePrisoner [n m s]
  (inc (mod (+ (dec m) (dec s)) n)))

(assert (= 2 (saveThePrisoner 5 2 1)))
(assert (= 3 (saveThePrisoner 5 2 2)))
(assert (= 6 (saveThePrisoner 7 19 2)))

