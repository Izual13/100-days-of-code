(ns  hdoc.d20
  (:require
   [clojure.string :as str]))

;Sherlock and Squares

(defn squares [a b] 
  (loop [x (Math/ceil (Math/sqrt a))         
         result 0]
    (let [sqr (* x x)
          pp (println sqr)] 
      (if (> sqr b) 
        result 
        (recur (inc x) (inc result))))))

(squares 17 24)

(assert (= 2 (squares 3 9)))
(assert (= 0 (squares 17 24)))

