(ns  hdoc.d39
  (:require
   [clojure.string :as str]))

;Sparse Arrays

(defn matchingStrings [strings queries]
  (let [uniq (frequencies strings)]
    (for [i queries] (let [result (get uniq i)]
                       (if (nil? result) 0 result)))))

(assert (= [1 0 1] (matchingStrings ["def" "de" "fgh"] ["de" "lmn" "fgh"])))
