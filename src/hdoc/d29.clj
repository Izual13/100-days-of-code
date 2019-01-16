(ns  hdoc.d29
  (:require
   [clojure.string :as str]))

;Taum and B'day

(defn taumBday [b w bc wc z]
  (let [sc (if (> bc wc) (+ wc z) (+ bc z))
        result (if (< sc bc) (* b sc) (* b bc))
        result (+ result (if (< sc wc) (* w sc) (* w wc)))]
    result))

(assert (= 20 (taumBday 10 10 1 1 1)))

