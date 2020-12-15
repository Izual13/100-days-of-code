(ns  hdoc.permutations
  (:require
   [clojure.string :as str]))



;Non-Divisible Subset

(defn show-sums [x] (println "sum: " (map #(reduce + %) x) ) x)

(defn permutations [k x a]
  (->> (for [i a]  [x i])
       (show-sums)
       (filter #(not (= (mod (+ (first %) (last %)) k) 0)))
       (reduce into  [])
       (frequencies)
       ))

(permutations 2 3 [1 2 3 4])
(permutations 2 2 [4])

()

(defn reduce-permurtations [a1 a2] 
  (reduce-kv (fn [m k v] (let [
                               old-value (get m k)
                               old-value (if (nil? old-value) 0 old-value)] 
                           (assoc m k (+ old-value v)))) a1 a2))


(reduce-permurtations {1 2} {1 3 3 6})

(defn pfn [x] (println x) x)

(defn nonDivisibleSubset [k S]
  (->> (loop [head   (first S)
              tail   (rest S)
              result {}]
         (if (empty? tail) 
           result
           (let [p      (permutations k head tail)
                 result (reduce-permurtations result p)]
             (recur (first tail) (rest tail) result)        ))  )
       (pfn)
       (count)))




(nonDivisibleSubset 3 [1 7 2 4])
(nonDivisibleSubset 7 [278 576 496 727 410 124 338 149 209 702 282 718 771 575 436])

(/ (+ 124 410) 7)

{410 14, 
 702 12, 
 282 12, 
 149 12, 
 338 12
 278 8,
 496 12, 
 575 11, 
 436 12, 
 727 12, 
 718 14, 
 771 11, 
 124 8, 
 576 12, 
 209 12}


(assert (= 3 (nonDivisibleSubset 3 [1 7 2 4])))