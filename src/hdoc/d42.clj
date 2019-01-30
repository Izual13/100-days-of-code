(ns  hdoc.d42
  (:require
   [clojure.string :as str]))

;Given an array of integers and a number k, where 1 <= k <= length of the array, compute the maximum values of each subarray of length k.
;
;For example, given array = [10, 5, 2, 7, 8, 7] and k = 3, we should get: [10, 7, 8, 8], since:
;
;10 = max(10, 5, 2)
;7 = max(5, 2, 7)
;8 = max(2, 7, 8)
;8 = max(7, 8, 7)
;Do this in O( n ) time and O( k ) space. You can modify the input array in-place and you do not need to store the results. You can simply print them out as you compute them.

(defn deque [n] 
  (java.util.ArrayDeque. n))

(let [deque (deque 5)]
  (.addFirst deque 1)
  (.addFirst deque 6)
  (.addFirst deque 50)
  (.getLast deque))


(defn windowMax [n k])

(windowMax [10 5 2 7 8 7] 3)

(assert (= [10 7 8 8] (windowMax [10 5 2 7 8 7] 3)))

