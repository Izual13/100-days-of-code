(ns  hdoc.d45
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]))

;Cavity Map

(defn parse-int [s] (Integer/parseInt (str s)))

(assert (= 1 (parse-int (get "123" 0))))

(defn cavityMap [grid]
  (let [n (count grid)]
    (vec (for [i (range 0 n)]
           (apply str (for [j (range 0 (count (get grid i)))]
                        (if (or (= i 0) (= j 0) (= i (dec n)) (= j (dec n)) (= \space (get (get grid i) j)))
                          (get (get grid i) j)
                          (let [r1    (get grid (dec i))
                                r2    (get grid i)
                                r3    (get grid (inc i))
                                value (parse-int (get (get grid i) j))]
                            (if (and  (> value (parse-int (get r1 j)))
                                      (> value (parse-int (get r2 (dec j))))
                                      (> value (parse-int (get r2 (inc j))))
                                      (> value (parse-int (get r3 j))))     "X"                    value)        
                            ))))))))



(assert (= ["1112" "1X12" "18X2" "1234"] (cavityMap ["1112" "1912" "1892" "1234"])))
(assert (= ["1 2" "1 2"] (cavityMap ["1 2" "1 2"])))
(assert (= ["179443854" "961X21369" "164139922" "96X633951" "812882578" "25782X163" "8124385X7" "176656233" "485773814"] 
           (cavityMap ["179443854" "961621369" "164139922" "968633951" "812882578" "257829163" "812438597" "176656233" "485773814"])))

