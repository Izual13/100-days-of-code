(ns  hdoc.d25
  (:require
   [clojure.string :as str]))

;Repeated String
(defn repeatedString [s n]
  (let [count (count s)
        result (get (frequencies s) \a)
        multiplier (Math/floor (/ n count))
        result (* multiplier (if (nil? result) 0 result))
        reminder (get (frequencies (subs s 0 (mod n count))) \a)
        result (+ result (if (nil? reminder) 0 reminder))]
    (long result))
  )

(assert (= 7 (repeatedString "aba" 10)))
(assert (= 1000000000000 (repeatedString "a" 1000000000000)))
(assert (= 0 (repeatedString "ceebbcb" 817723)))