(ns  hdoc.d37
  (:require
   [clojure.string :as str]))

;Bigger is Greater

(defn find-pivot [w]  
  (loop [i      (- (count w) 1)
         result -1] 
    (let [prev-i (dec i)] 
      (cond (= i -1) -1
            (< (compare (get w prev-i) (get w i)) 0) prev-i
            :else  (recur prev-i result)))))

(assert (= 1 (find-pivot [1 6 7 4 3])))

(defn find-greater [w x]
  (let [idx (java.util.Collections/binarySearch w x #(compare %2 %1))
        idx (if (< idx 0) (Math/abs idx) (inc idx))]
    idx))


(assert (= 2 (find-greater [7 4 3] 6)))
(assert (= 2 (find-pivot "0125330")))

(defn biggerIsGreater [w]
  (let [pivot        (find-pivot w)
        greater (if (not= pivot -1) (+ pivot -1 (find-greater (vec (subs w (inc pivot))) (get w pivot))) -1)
        next-pivot   (inc pivot)
        next-greater (inc greater)]
    (if (= greater -1) "no answer"
        (let [
              x1 (subs w pivot next-pivot)
              x2 (subs w greater next-greater)]
          (println "x1: " x1 "x2: " x2)
          (println "pivot: " pivot "; greater: " greater)
          (apply str (concat (subs w 0 pivot)
                             x2 
                             (reverse (concat
                                       (subs w next-pivot greater)
                                       x1
                                       (subs w next-greater)))))
          ))))

(biggerIsGreater "zalqxykemvzzgaka")

(assert (= 2 (find-pivot "dhck")))

(assert (= 3 (+ (find-pivot "dhck") -1 (find-greater (vec (subs "dhck" (inc (find-pivot "dhck")))) \c))))

(assert (= "ba" (biggerIsGreater "ab")))
(assert (= "no answer" (biggerIsGreater "bb")))
(assert (= "fedcbabdc" (biggerIsGreater "fedcbabcd")))
(assert (= "dhkc" (biggerIsGreater "dhck")))
(assert (= "acbd" (biggerIsGreater "abdc")))
(assert (= "zalqxykemvzzgkaa" (biggerIsGreater "zalqxykemvzzgaka")))
