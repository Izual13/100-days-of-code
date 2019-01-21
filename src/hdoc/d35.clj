(ns  hdoc.d35
  (:require
   [clojure.string :as str]))

;Chocolate Feast

(defn chocolateFeast [n c m]
  (let [result (int (Math/floor (/ n c)))]
    (loop [r        result
           wrappers result] 
      (println "r: " r "; wrappers: " wrappers)
      (if (< wrappers m) 
        r
        (let [new-chocolate     (int (Math/floor (/ wrappers m)))
              reminder-wrappers (int (Math/floor (mod wrappers m)))]
          (recur (+ r new-chocolate) (+ new-chocolate reminder-wrappers)))))))

(chocolateFeast 10 2 5)
(chocolateFeast 6 2 2)

(assert (= 6 (chocolateFeast 10 2 5)))
(assert (= 3 (chocolateFeast 12 4 4)))
(assert (= 5 (chocolateFeast 6 2 2)))
