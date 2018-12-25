(ns  hdoc.d9
  (:require
   [clojure.string :as str]))

;Cats and a Mouse

(defn abs [n] (max n (- n)))

(defn catAndMouse [x y z]
  (cond 
    (<  (abs (- x z)) (abs (- y z))) "Cat A"
    (>  (abs (- x z)) (abs (- y z))) "Cat B"
    :else "Mouse C"       ))


(assert (= "Cat B" (catAndMouse 1 2 3)))
(assert (= "Mouse C" (catAndMouse 1 3 2)))

