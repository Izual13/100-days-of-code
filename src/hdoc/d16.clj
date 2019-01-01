(ns  hdoc.d16
  (:require
   [clojure.string :as str]))

;Viral Advertising

(defn viralAdvertising [n]
  (loop [i 1
         shared 6
         liked 2]
    (if (= i n) 
      (int liked)
      (let [l (Math/floor (/ shared 2))
            p (println l)]
        (recur (inc i) (* l 3) (+ liked l))))))


(assert (= 9 (viralAdvertising 3)))
(assert (= 24 (viralAdvertising 5)))


