(ns  hdoc.d10
  (:require
   [clojure.string :as str]))

;Picking Numbers

(defn pickingNumbers [a]
  (loop [sub-map (into (sorted-map) (frequencies a))
         i (key (first sub-map))
         end (key (last sub-map))
         max-length (val (first sub-map))]
    (let [next-i (inc i)
          length (reduce + (map #(last %) (subseq sub-map >= i <= next-i)))]
      (if (= i end)
        max-length
        (recur sub-map next-i end (max max-length length))))))


(assert (= 100 (pickingNumbers [66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66])))
(assert (= 3 (pickingNumbers [4 6 5 3 3 1])))

