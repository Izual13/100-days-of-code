(ns  hdoc.d4)

;Bon Appétit

(defn remove-by-idx
  [coll idx]
  (vec
   (concat
    (subvec coll 0 idx)
    (subvec coll (inc idx)))))

(defn bonAppetit [bill k b] 
  (let [reminder (reduce + (remove-by-idx bill k))
        result (- b (/ reminder 2))] 
    (if (= result 0)
      "Bon Appetit"
      result
      )))


(assert (= 5 (bonAppetit [3 10 2 9] 1 12)))
(assert (= "Bon Appetit" (bonAppetit [3 10 2 9] 1 7)))

