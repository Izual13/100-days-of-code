(ns  hdoc.d2)

;Migratory Birds

(defn migratoryBirds [arr]
  (key (apply max-key val (into (sorted-map-by >) (frequencies arr)))))

(assert (= 3 (migratoryBirds [1 2 3 4 5 4 3 2 1 3 4])))
(assert (= 4 (migratoryBirds [1 4 4 4 5 3])))

