(ns  hdoc.d34
  (:require
   [clojure.string :as str]))

;Halloween Sale

(defn howManyGames [p d m s]
    (loop [current-prise p
           sum 0
           result 0]
      (if (> (+ sum current-prise) s) result
          (let [new-current-prise (- current-prise d)]
            (println "current-prise: " current-prise "; sum: " sum "; result: " result)
            (println "new-current-prise: " new-current-prise "; m" m "; equals: " (if (> new-current-prise m) true false))
            (recur (if (> new-current-prise m) new-current-prise m)
                   (+ sum current-prise)
                   (inc result)))
      ))
  )

(assert (= 6 (howManyGames 20 3 6 80)))
(assert (= 7 (howManyGames 20 3 6 85)))
