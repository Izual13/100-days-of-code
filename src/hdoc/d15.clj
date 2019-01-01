(ns  hdoc.d15
  (:require
   [clojure.string :as str]))

;Beautiful Days at the Movies

(defn reverse-int[x] (Integer/parseInt (apply str (reverse (str x)))))

(defn beautifulDays [i j k]
  (count (filter #(= 0 (mod (- % (reverse-int %)) k) ) (range i (inc j)))))

(assert (= 2 (beautifulDays 20 23 6)))


