(ns  hdoc.d17
  (:require
   [clojure.string :as str]))

;Circular Array Rotation

(defn circularArrayRotation [a k queries]
  (let [l (count a)]
    (map (fn [x] (nth a (mod (- x  k) l))) queries)))

(circularArrayRotation [1 2 3] 2 [0 1 2])

(assert (= [2 3 1] (circularArrayRotation [1 2 3] 2 [0 1 2])))


;Sequence Equation

(defn basic-array [l] (range 1 (inc l)))

(defn permutationEquation [p]
  (->> 
   (map #(.indexOf p %)  (basic-array (count p)))
   (map #(.indexOf p (inc %)))
   (map #(nth (basic-array (count p)) %))
   ))


(assert (= [1 3 5 4 2] (permutationEquation [4 3 5 1 2])))
(assert (= [2 3 1] (permutationEquation [2 3 1])))
(assert (= [2 5 11 13 1 14 7 3 4 18 8 6 16 12 15 10 9 17] (permutationEquation [2 5 11 10 1 14 7 3 16 9 8 6 18 12 15 17 13 4])))


