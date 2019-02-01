(ns  hdoc.d42
  (:require
   [clojure.test :refer :all]
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

(defn remove-right [array deque value]
  (loop [last (get array (.peekLast deque))]
    (if (or (nil? last) (> last value))
      deque
      (let [rm (.pollLast deque)]
        (recur (get array (.peekLast deque)))))))

(defn remove-left [deque value]
  (loop [first (.peekFirst deque)]
    (if (or (nil? first) (>= first value))
      deque
      (let [rm (.pollFirst deque)]
        (recur (.peekFirst deque))))))

(let  [d (deque 5)]
  (.addLast d 0)
  (.addLast d 1)
  (remove-right [10 4] d 3)
  (assert (= 1 (.pollLast d)))
  (assert (= 0 (.pollLast d)))
  )

(let  [d (deque 5)]
  (.addLast d 0)
  (.addLast d 1)
  (remove-right [10 4] d 5)
  (assert (= 0 (.pollLast d)))
  (assert (nil? (.pollLast d)))
  )

(let  [d (deque 5)]
  (.addLast d 0)
  (.addLast d 1)
  (.addLast d 2)
  (.addLast d 3)
  (remove-left d 1)
  (assert (= 3 (.pollLast d)))
  (assert (= 2 (.pollLast d)))
  (assert (= 1 (.pollLast d)))
  (assert (nil? (.pollLast d)))
  )

(let  [d (deque 5)]
  (.addLast d 0)
  (.addLast d 1)
  (remove-left d 1)
  (assert (= 1 (.pollLast d)))
  (assert (nil? (.pollLast d)))
  )


(defn windowMax [n k]
  (let [deque (deque k)
        result (transient [])]
    (doseq [i (range k)] 
      (let [last  (get n (.peekLast deque))
            value (get n i)]
        (if (or (nil? last) (> last value))
          (.addLast deque i)
          (do (remove-right n deque value)
              (.addLast deque i)))))
    
    (doseq [i (range k (count n))] 
      (conj! result (get n (.peekFirst deque)))
      (remove-left deque (inc (- i k)))
      (remove-right n deque (get n i))
      (.addLast deque i))
    (conj! result (get n (.peekFirst deque)))
    (persistent! result)
    ))

(windowMax [10 5 2 7 8 7] 3)
(windowMax [10 2 5 7 8 7] 3)

(assert (= [10 7 8 8] (windowMax [10 5 2 7 8 7] 3)))
(assert (= [5 4 6 6 6 4 4 4 5] (windowMax [5 3 4 1 6 2 2 4 3 1 5] 3)))

