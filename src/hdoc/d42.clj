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


(assert (= [10 7 8 8] (windowMax [10 5 2 7 8 7] 3)))


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
        (println "last: " last "; value: " value)
        (if (or (nil? last) (> last value))
          (.addLast deque i)
          (do (remove-right n deque value)
              (.addLast deque i)))))
    
    ; (print-deque deque)
    
    (doseq [i (range k (count n))] 
      (conj! result i)
      (println (get n i)))
    (persistent! result)
    ))

(windowMax [10 5 2 7 8 7] 3)
(windowMax [10 2 5 7 8 7] 3)


(doseq [x (range 10)]
  (println x))

(assert (= [10 7 8 8] (windowMax [10 5 2 7 8 7] 3)))

