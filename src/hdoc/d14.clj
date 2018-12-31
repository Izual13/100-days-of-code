(ns  hdoc.d14
  (:require
   [clojure.string :as str]))

;Utopian Tree

(defn calc [n] 
  (loop [i 0
         h 1] 
    (if (= i n) 
      h
      (recur (inc i) (if (odd? i) (inc h) (* h 2))))))

(defn utopianTree [n]
  (vec (map (fn [x] (calc x)) n)))

(utopianTree [0 1 4])

(assert (= [1 2 7] (utopianTree [0 1 4])))
(assert (= [1 2] (utopianTree [0 1 4])))


;Angry Professor

(defn angryProfessor [k a]
  (if (<= k (count (filter #(>= 0 %) a))) "NO" "YES"))

(assert (= "YES" (angryProfessor 3 [-1 -3 4 2])))
(assert (= "NO" (angryProfessor 2 [0 -1 2 1])))