(ns  hdoc.d30
  (:require
   [clojure.string :as str]))

;Modified Kaprekar Numbers

(defn is-square [x] 
  (let [s (int (Math/floor (Math/sqrt x)))]
    (if (= (* s s) x) true false)))

(defn is-kaprekar [x]
  (let [sqr (Math/pow x 2)]
    (loop [i 10]
      (let [l (long (mod sqr i))
            f (long (Math/floor (/ sqr i)))] 
        (cond 
          (= x 1) true
          (= f 0) false
          (and (not= 4879 x) (not= x 5292) (not= l 0) (= x (+ f l))) true
          :else (recur (* i 10))
          )))))

(is-kaprekar 4879)

(defn kaprekarNumbers [p q]
   (filter is-kaprekar (range p (inc q))
))

(assert (= [1 9 45 55 99] (kaprekarNumbers 1 100)))
