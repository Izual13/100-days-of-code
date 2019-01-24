(ns  hdoc.d37
  (:require
   [clojure.string :as str]))

;Bigger is Greater

(defn find-pivot [w]  
  (loop [i      (- (count w) 1)
         result -1] 
    (cond (= i -1) -1
          (< (compare (get w (dec i)) (get w i)) 0) (dec i)
          :else  (recur (dec i) result))))

(assert (= 1 (find-pivot [1 6 7 4 3])))

(defn find-greater [w x ]
  (loop [i      (- (count w) 1)
         result -1]
    (cond (= i -1) -1
          (< (compare x (get w i)) 0) i
          :else  (recur (dec i) result))))

(assert (= 2 (find-greater [1 6 7 4 3] 6)))


(subs "16743" 0 1)
(subs "16743" 2 3)

(get "0123" 1)
(find-pivot "0125330") ;2
(find-greater "0125330" \2)
; 
(subs "0125330" (inc 2) 5)
(subs "0125330" 5)


(defn biggerIsGreater [w]
  (let [pivot   (find-pivot w)
        greater (if (not= pivot -1) (find-greater w (get w pivot)) -1)]
    (if (= greater -1) "no answer"
        (let [
              x1 (subs w pivot (inc pivot))
              x2 (subs w greater (inc greater))]
          (apply str (concat (subs w 0 pivot)
               x2 
               (reverse (concat
                         (subs w (inc pivot) greater)
                         x1
                         (subs w (inc greater))))))
          ))))



(assert (= 2 (find-pivot "dhck")))

(assert (= 3 (find-greater "dhck" \c)))

(assert (= "ba" (biggerIsGreater "ab")))
(assert (= "fedcbabdc" (biggerIsGreater "fedcbabcd")))
(assert (= "dhkc" (biggerIsGreater "dhck")))
