(ns  hdoc.d6)

;Drawing Book

(defn pageCount [n p]
  (def average (/ n 2))
  (if (<= p average)
    (int (Math/floor (/ p 2)))
    (if (odd? n) 
      (int (Math/floor (/ (- n p) 2)))
      (int (Math/floor (/ (- (inc n) p) 2))))))

(assert (= 0 (pageCount 6 1)))
(assert (= 1 (pageCount 6 2)))
(assert (= 1 (pageCount 6 3)))
(assert (= 1 (pageCount 6 4)))
(assert (= 1 (pageCount 6 5)))
(assert (= 0 (pageCount 6 6)))


(assert (= 0 (pageCount  5 4)))
