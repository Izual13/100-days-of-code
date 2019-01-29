(ns  hdoc.d41
  (:require
   [clojure.string :as str]))

;Lisa's Workbook

(defn workbook [n k arr]
  (loop [chapter  1
         p        1
         result   0 
         problems (get arr 0)]
    (println "chapter: " chapter "; page: " p "; result: " result "; problems: " problems)
    (if (= chapter (inc n)) result 
        (let [all-problems (get arr (dec chapter))
              problem-r1 (inc (- all-problems problems))
              problem-r2 (min (dec (+ problem-r1 k)) all-problems)              ]
          (println "probmes [" problem-r1 ":" problem-r2 "]")
          (println "added: " (and (>= p problem-r1) (<= p problem-r2)))
          (if (> problems k)
            (recur chapter (inc p) (+ result (if (and (>= p problem-r1) (<= p problem-r2)) 1 0)) (- problems k))
            (recur (inc chapter) (inc p) (+ result (if (and (>= p problem-r1) (<= p problem-r2)) 1 0)) (get arr chapter)))))))




(assert (= 4 (workbook 5 3 [4 2 6 1 10])))

