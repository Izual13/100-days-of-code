(ns  hdoc.d10
  (:require
   [clojure.string :as str]))

;Picking Numbers

(defn pickingNumbers [a]
  (let [sub-map (into (sorted-map) (frequencies a))
        end     (key (last sub-map))]
    (loop [i          (key (first sub-map))
           max-length (val (first sub-map))]
      (let [next-i (inc i)
            length (reduce + (map #(last %) (subseq sub-map >= i <= next-i)))]
        (if (= i end)
          max-length
          (recur next-i (max max-length length)))))))


(assert (= 100 (pickingNumbers [66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 66])))
(assert (= 3 (pickingNumbers [4 6 5 3 3 1])))

