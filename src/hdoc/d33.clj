(ns  hdoc.d33
  (:require
   [clojure.string :as str]))

;Encryption

(defn encryption [s]
  (let [count    (count s)
        sqrt     (Math/sqrt count)
        collumns (int (Math/ceil sqrt))
        rows     (int (Math/floor sqrt))
        rows     (if (< (* rows collumns) count) (inc rows) rows)]
    (->> 
     (for [j (range collumns)]
       (for [i (range rows)]
         (get s (+ j (* i collumns)))))
     (map #(apply str %) )
     (reduce #(str %1 " " %2)))))




(assert (= "hae and via ecy" (encryption "haveaniceday")))
(assert (= "clu hlt io" (encryption "chillout")))
