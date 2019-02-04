(ns  hdoc.d44
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]))


;An sorted array of integers was rotated an unknown number of times.
;Given such an array, find the index of the element in the array in faster than linear time. If the element doesn't exist in the array, return null.
;For example, given the array [13, 18, 25, 2, 8, 10] and the element 8, return 4 (the index of 8 in the array) .
;You can assume all the integers in the array are unique.


(defn find-index [a e] 
  (loop [begin 0 
         end   (dec (count a))]
    (let [pivot (int (Math/floor (+ (/ end 2) (/ begin 2))))]
          (if (= e (get a pivot)) pivot
              (if (<= end begin) -1
                  (let [is-left-sorted (if (and (not= pivot 0) (< (get a begin) (get a (dec pivot)))) true false)]
                    (if is-left-sorted 
                      (if (and (not= pivot 0) (<= (get a begin) e) (>= (get a (dec pivot)) e))
                        (recur begin (dec pivot))
                        (recur (inc pivot) end))
                      (if (and (<= (get a (inc pivot)) e) (>= (get a end) e))
                        (recur (inc pivot) end)
                        (recur begin (dec pivot))))))))))

(find-index [13 18 25 2 8 10] 8)

(assert (= 4 (find-index [13 18 25 2 8 10] 8)))
(assert (= 1 (find-index [2 8 10 13 18 25] 8)))