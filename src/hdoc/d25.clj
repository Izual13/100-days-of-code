(ns  hdoc.d25
  (:require
   [clojure.string :as str]))

;Repeated String

(defn reduce-map [a1 a2]
  (println "a1: " a1 " a2: " a2)
  (reduce-kv (fn [m k v] (let [old-value (get m k)
                               old-value (if (nil? old-value) 0 old-value)]
                           (assoc m k (+ old-value v)))) a1 a2))

(defn repeatedString [s n]
  (let [count (count s)
        fr (frequencies s)
        multiplier (Math/floor (/ n count))
        fr (into {} (map (fn [item] { (key item) (* multiplier (val item))}) fr))
        reminder (mod n count)
        fr (reduce-map fr (frequencies (subs s 0 reminder)))
        max (get fr \a)        
        max (if (nil? max) 0 max)]
    (long max))
  )

(assert (= 7 (repeatedString "aba" 10)))
(assert (= 1000000000000 (repeatedString "a" 1000000000000)))
(assert (= 0 (repeatedString "ceebbcb" 817723)))