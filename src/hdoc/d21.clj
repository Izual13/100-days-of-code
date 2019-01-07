(ns  hdoc.d21
  (:require
   [clojure.string :as str]))

;Sherlock and Squares

(defn squares [a b] 
  (->> 
   (- (Math/floor (Math/sqrt b)) (Math/ceil (Math/sqrt a)))
   (inc)
   (int)))

(assert (= 2 (squares 3 9)))
(assert (= 0 (squares 17 24)))

