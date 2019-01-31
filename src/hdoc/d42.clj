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
  ; (.addFirst deque 50)
  (.peekLast deque)
  (.peekLast deque)
  (.peekLast deque)
  (.peekLast deque)
  (.peekLast deque)
  (print-deque deque)
  )

(defn print-deque [d] 
  (println "print-deque ->")
  (loop [first (.peekFirst d)]
    (if (nil? first) ""
        (let [] (println first)
             (.removeFirst d)
             (recur (.peekFirst d))
             )))
  (println "<- print-deque")
  )

(defn windowMax [n k]
  (let [deque (deque k)]
    (doseq [i (range k)] 
      (let [last  (get n (.peekLast deque))
            value (get n i)]
        (println "last: " last "; value: " value)
        (if (or (nil? last) (> last value))
          (.addLast deque i)
          (do (.clear deque)
              (.addLast deque i))          )))
    
    (print-deque deque)
    
    (doseq [i (range k (count n))] 
      (println (get n i)))
    ))

(windowMax [10 5 2 7 8 7] 3)


(doseq [x (range 10)]
  (println x))

(assert (= [10 7 8 8] (windowMax [10 5 2 7 8 7] 3)))

