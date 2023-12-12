(ns  aoc2023.d10
  (:require [clojure.string :as str]))

(def tiles (clojure.string/split (slurp "resources/aoc2023/day10_1") #"\r?\n"))

(def tiles-t (clojure.string/split (slurp "resources/aoc2023/day10_t") #"\r?\n"))

(def tiles-t2 (clojure.string/split (slurp "resources/aoc2023/day10_t2") #"\r?\n"))

(def tiles-t3 (clojure.string/split (slurp "resources/aoc2023/day10_t3") #"\r?\n"))

(defn parse-array [s] 
  (mapv vec s))

(defn find-s 
  ([a] 
   (find-s a \S))
  ([a s] 
   (let [c (count a)]
     (loop [i 0 j 0]
       (cond 
         (= j c) nil
         (= i c) (recur 0 (inc j))
         (= s (get-in a [j i] nil)) [j i 0]
         :else (recur (inc i) j))))))

(defn init-v [c]
  (let [v (vec 
            (for [i (range c)] 
              (vec (for [j (range c)] nil))))]
    v))

(defn find-neighbours [a visited [j i _]]
  (let [current-pipe (get-in a [j i] nil)
        current-weight (get-in visited [j i] nil)
        p (case current-pipe 
            \| [[(dec j) i #{\| \7 \F}] [(inc j) i #{\| \J \L}]]
            \- [[j (dec i) #{\- \L \F}] [j (inc i) #{\- \J \7}]]
            \L [[(dec j) i #{\| \7 \F}] [j (inc i) #{\- \J \7}]]
            \J [[(dec j) i #{\| \7 \F}] [j (dec i) #{\- \L \F}]]
            \7 [[j (dec i) #{\- \L \F}] [(inc j) i #{\| \J \L}]]
            \F [[j (inc i) #{\- \J \7}] [(inc j) i #{\| \J \L}]]
            \S [[(dec j) i #{\| \7 \F}] [(inc j) i #{\| \J \L}] [j (dec i) #{\- \L \F}] [j (inc i) #{\- \J \7}]])
        
        ]
    (loop [p p r []]
      ;(println "p" p [j i] "r" r)
    
      (if 
        (empty? p) r
        (let [[j i pv] (first p)
              next-pipe (get-in a [j i] nil)
              next-weight (get-in visited [j i] nil)
              ; _ (println "current-weight" current-weight "next-pipe" next-pipe "next-weight" next-weight)
              ]
          (cond 
            (nil? next-pipe) (recur (next p) r)
            (= next-pipe \.) (recur (next p) r)
            (and (nil? next-weight) (.contains pv next-pipe)) (recur (next p) (conj r [j i (inc current-weight)]))
            (and (.contains pv next-pipe) (< (inc current-weight) next-weight)) (recur (next p) (conj r [j i (inc current-weight)]))
            :else (recur (next p) r)))))))

(defn find-loop [a]
  (let [size-a (count a)
        start (find-s a)
        ; _ (println "s:" start)
        ]
    (loop [s [start] v (init-v size-a)]
      (if (empty? s) 
        v
        (let [[j i w] (first s)
              new-v (assoc-in v [j i] w)
              n (find-neighbours a new-v [j i])
              current-weight (get-in new-v [j i] nil)
              ]
          (recur (apply conj (next s) n) new-v))))))


(assoc-in [[]] [0 0] 1)

(apply conj [] [])

(assert (= 4 (->> tiles-t
               parse-array
               find-loop
               flatten
               (filter some?)
               (apply max))))

(assert (= 8 (->> tiles-t2
               parse-array
               find-loop
               flatten
               (filter some?)
               (apply max))))



(assert (= 6842 (->> tiles
                  parse-array
                  find-loop
                  flatten
                  (filter some?)
                  (apply max))))


;;;;part 2

(println "\n\n\n\n")

(->> tiles-t
  parse-array
  find-loop
  ; flatten
  ; (filter some?)
  ; (apply max)
  )

; (defn find-o [a start]
;   (println start)
;   (loop [p [start] v #{start}]
;     (if (empty? p) 
;       false
;       (let [[j i d] (first p)]
;         (cond 
;           (= (get-in a [j i]) -1) true
;           (.contains v [j i]) false
;           :else false)))))

; (defn mark-o [a]
;   (let [c (count a)]
;     (loop [j 0 i 0 a a]
;       (cond 
;         (= j c) a
;         (= i c) (recur (inc j) 0 a)
;         (some? (get-in a [j i])) (recur j (inc i) a)
;         (or (= i 0) (= j 0) (= i (dec c)) (= j (dec c))) (recur j (inc i) (assoc-in a [j i] -1))
;         (or (= -1 (get-in a [(dec j) (dec i)])) (= -1 (get-in a [(dec j) i])) (= -1 (get-in a [(dec j) (inc i)]))
;           (= -1 (get-in a [j (dec i)]))  (= -1 (get-in a [j (inc i)]))
;           (= -1 (get-in a [(inc j) (dec i)])) (= -1 (get-in a [(inc j) i])) (= -1 (get-in a [(inc j) (inc i)]))) (recur j (inc i) (assoc-in a [j i] -1))
;         (find-o a [j i :default]) (recur j (inc i) (assoc-in a [j i] -1))
;         :else (recur j (inc i) a)))))

(->> tiles-t3
  parse-array
  find-loop
  mark-o
  ; flatten
  ; (filter some?)
  ; (apply max)
  )


(->> tiles
  parse-array
  find-loop
  mark-o
  ; flatten
  ; (filter some?)
  ; (apply max)
  println
  )

(defn find-loop-v2 [a]
  (let [size-a (count a)
        start (find-s a)
        ; _ (println "s:" start)
        ]
    (loop [s [start] v (init-v size-a)]
      (if (empty? s) 
        v
        (let [
              [j i w] (first s)
              ;_ (println v [j i] w)
              new-v (assoc-in v [j i] w)
              n (first (find-neighbours a new-v [j i]))
              current-weight (get-in new-v [j i] nil)
              ]
          (if (nil? n)
            (recur (next s) new-v)
            (recur (conj (next s) n) new-v)))))))

(defn find-next [a [j i dir]]
  (let [p (get-in a [j i])
        u [(dec j) i]
        d [(inc j) i]
        l [j (dec i)]
        r [j (inc i)]]
    (cond 
      (= (inc p) (get-in a u)) [(dec j) i (cond    
                                            (= dir :d) :l
                                            (= dir :u) :l   
                                            :else dir)]
      (= (inc p) (get-in a d)) [(inc j) i (cond    
                                            (= dir :d) :r
                                            (= dir :u) :l
                                            :else dir)]
      (= (inc p) (get-in a l)) [j (dec i) (cond 
                                            (= dir :r) :d
                                            (= dir :l) :u  
                                            :else dir)]
      (= (inc p) (get-in a r)) [j (inc i) (cond
                                            (= dir :r) :u
                                            (= dir :l) :d
                                            :else dir)]
      :else nil)))

(defn mark-i [a]
  (let [c (count a)
        [j i] (find-s a 0)]
    (loop [p [j i :r] a a]
      
      (let [n (find-next a p)
            [j i d] p
            _ (println j i d "->" n)
            ;_ (println "n" n)
            check-i (case d
                      :u [(dec j) i] 
                      :d [(inc j) i]
                      :r [j (inc i)]
                      :l [j (dec i)])
            new-a (if (nil? (get-in a check-i))
                    (assoc-in a check-i -1)
                    a)]
        ; (println p n)
        (if (nil? n)
          new-a
          (recur n new-a))))))

(->> tiles-t3
  parse-array
  find-loop-v2
  mark-i
  ;mark-o
  ; flatten
  ; (filter some?)
  ; (apply max)
  ; println
  )

(println "\n\n\n\n")

(->> tiles
  parse-array
  find-loop-v2
  mark-i
  ; flatten
  ; (filter some?)
  ; (apply max)
  println
  )

[[nil nil nil nil nil nil nil nil nil]
 [nil 0 43 42 41 40 39 38 37]
 [nil 1 16 17 18 19 20 21 36]
 [nil 2 15 nil nil nil nil 22 35]
 [nil 3 14 nil nil nil nil 23 34]
 [nil 4 13 12 11 26 25 24 33]
 [nil 5 nil nil 10 27 nil nil 32]
 [nil 6 7 8 9 28 29 30 31]
 [nil nil nil nil nil nil nil nil nil]]
