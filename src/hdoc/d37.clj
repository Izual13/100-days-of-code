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
  (let [w     (vec w)
        pivot (find-pivot w)]
    (if (= pivot -1) "no answer"
        (let [              
              x1           (get w pivot)
              next-pivot   (inc pivot)
              sub-w        (subvec w pivot)
              greater      (if (not= pivot -1) (+ pivot -1 (find-greater (drop 1 sub-w)   x1)) -1)
              next-greater (inc greater)
              x2           (get w greater)
              w            (-> w
                               (assoc pivot x2)
                               (assoc greater x1))]
          (apply str (concat (take next-pivot w)
                             (reverse (drop next-pivot w))))))))




(assert (= 2 (find-pivot "dhck")))

(assert (= 3 (+ (find-pivot "dhck") -1 (find-greater (vec (subs "dhck" (inc (find-pivot "dhck")))) \c))))

(assert (= "ba" (biggerIsGreater "ab")))
(assert (= "no answer" (biggerIsGreater "bb")))
(assert (= "fedcbabdc" (biggerIsGreater "fedcbabcd")))
(assert (= "dhkc" (biggerIsGreater "dhck")))
(assert (= "acbd" (biggerIsGreater "abdc")))
(assert (= "zalqxykemvzzgkaa" (biggerIsGreater "zalqxykemvzzgaka")))
