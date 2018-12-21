(ns  hdoc.d5)

;Forming a Magic Square

(def default-square [8 3 4 1 5 9 6 7 2])
(def default-rsquare [8 1 6 3 5 7 4 9 2])

; 8 3 4             
; 1 5 9
; 6 7 2

(defn find-value [s v] (first (keep-indexed #(if (= %2 v) %1)  s)))

(defn get-value [s i k]
  (def angles [0 2 8 6])
  (def averages [1 5 7 3])
  (cond
    (.contains angles i) (nth s (nth angles (mod (+ k (find-value angles i)) (count angles))))
    (.contains averages i) (nth s (nth averages (mod (+ k (find-value averages i)) (count averages))))
    (= i 4) 5))

(defn rotate-square [s k] 
  (into [] (map #(get-value s % k) (range 0 9))))

(def all-permutations (concat (into [] (map #(rotate-square default-square %1)) (range 0 4))
        (into [] (map #(rotate-square default-rsquare %1)) (range 0 4))))

(defn miscalculation [a1 a2] 
  (loop [a1 a1
         a2 a2
         c 0]
    (def v1 (first a1))
    (def v2 (first a2))
    (if (empty? a1) 
      c
      (recur (rest a1) (rest a2) (+ c (Math/abs (- v1 v2 )))))))

(defn formingMagicSquare [s] 
  (let [
        array (into [] (apply concat s))] 
    (loop [permutations all-permutations
           result       45]
      (if (empty? permutations) 
        result
        (let [tmp (miscalculation array (first permutations))]
          (recur (rest permutations) (if (< tmp result) tmp result)))))))

;  (formingMagicSquare [4 9 2 3 5 7 8 1 5] )

#_(formingMagicSquare [[4 9 2] [3 5 7] [8 1 5]])

(assert (= 1 (formingMagicSquare [[4 9 2] [3 5 7] [8 1 5]])))
(assert (= 4 (formingMagicSquare [[4 8 2] [4 5 7] [6 1 6]])))
