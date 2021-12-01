(ns  hdoc.d49
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]))

;Queen's Attack II

(defn all-attacks [x y size]
  (+ (- x 1) (- size x)
     (- y 1) (- size y)
     (min (- x 1) (- y 1))
     (min (- size x) (- size y))
     (min (- size x) (- y 1))
     (min (- x 1) (- size y))))

(assert (= 9 (all-attacks 4 4 4)))

(defn queensAttack [n _ r_q c_q _]
  (let [all (all-attacks r_q c_q n)] all))
 
(queensAttack 5 3 4 3 [[5 5] [4 2] [2 3]])

(queensAttack 4 0 4 4 [])

(assert (= 9 (queensAttack 4 0 4 4 [])))


