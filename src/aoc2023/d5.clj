(ns  aoc2023.d5
  (:require [clojure.string :as str]))


(def almanac (clojure.string/split (slurp "resources/aoc2023/day5_1") #"\r?\n\r?\n"))

(def almanac-t (clojure.string/split (slurp "resources/aoc2023/day5_t") #"\r?\n\r?\n"))

almanac-t


(defn parse-array [s] 
  ;(println s)
  (let [r (clojure.string/split s #" ")]
    (mapv #(Long/parseLong %1) r)))

(parse-array "79 14 55 13")

(defn parse-array-ignore-first-raw [s]
  (let [s (clojure.string/split s #"\r?\n")]
    (mapv parse-array (rest s))))

(defn parse-almanac [[seeds ss sf fw wl lt th hl]] 
  (let [[_ seeds] (re-matches #"seeds: (.*)" seeds)
        seeds (parse-array seeds)]
    {:seeds seeds 
     :ss (parse-array-ignore-first-raw ss)
     :sf (parse-array-ignore-first-raw sf)
     :fw (parse-array-ignore-first-raw fw)
     :wl (parse-array-ignore-first-raw wl)
     :lt (parse-array-ignore-first-raw lt)
     :th (parse-array-ignore-first-raw th)
     :hl (parse-array-ignore-first-raw hl)}))

(vec (rest `("34" "1 2 3")))

(re-matches #"seeds: (.*)" "seeds: 79 14 55 13")

(parse-almanac almanac-t)





["seeds: 79 14 55 13"
 "seed-to-soil map:\r\n50 98 2\r\n52 50 48"
 "soil-to-fertilizer map:\r\n0 15 37\r\n37 52 2\r\n39 0 15"
 "fertilizer-to-water map:\r\n49 53 8\r\n0 11 42\r\n42 0 7\r\n57 7 4"
 "water-to-light map:\r\n88 18 7\r\n18 25 70"
 "light-to-temperature map:\r\n45 77 23\r\n81 45 19\r\n68 64 13"
 "temperature-to-humidity map:\r\n0 69 1\r\n1 0 69"
 "humidity-to-location map:\r\n60 56 37\r\n56 93 4"]

{:seeds [79 14 55 13],
 :ss [[50 98 2] [52 50 48]],
 :sf [[0 15 37] [37 52 2] [39 0 15]],
 :fw [[49 53 8] [0 11 42] [42 0 7] [57 7 4]],
 :wl [[88 18 7] [18 25 70]],
 :lt [[45 77 23] [81 45 19] [68 64 13]],
 :th [[0 69 1] [1 0 69]],
 :hl [[60 56 37] [56 93 4]]}

(partition 2 [79 14 55 13])


(defn mmap [n maps] 
  (loop [m maps]
    (if (empty? m) n
      (let [[d s l] (first m)]
        (if (<= s n (+ s l))
          (+ n (- d s))
          (recur (next m)))))))

(mmap 79 [[50 98 2] [52 50 48]])



(defn processing [{seeds :seeds ss :ss sf :sf fw :fw wl :wl lt :lt th :th hl :hl}]
  (for [s seeds] (-> s
                   (mmap ss)
                   (mmap sf)
                   (mmap fw)
                   (mmap wl)
                   (mmap lt)
                   (mmap th)
                   (mmap hl))))


(assert (= 35 (->> almanac-t
                parse-almanac
                processing
                (apply min)
                )))

(assert (= 178159714 (->> almanac
                       parse-almanac
                       processing
                       (apply min)
                       )))


;; part 2

(defn multi [x l]
  (for [i (range l)] (+ x i)))

(defn parse-almanac-v2 [[seeds ss sf fw wl lt th hl]] 
  (let [[_ seeds] (re-matches #"seeds: (.*)" seeds)
        seeds  (parse-array seeds)]
    {:seeds (mapv (fn [[f s]] [f (+ f s)]) (partition 2 seeds))
     :ss (parse-array-ignore-first-raw ss)
     :sf (parse-array-ignore-first-raw sf)
     :fw (parse-array-ignore-first-raw fw)
     :wl (parse-array-ignore-first-raw wl)
     :lt (parse-array-ignore-first-raw lt)
     :th (parse-array-ignore-first-raw th)
     :hl (parse-array-ignore-first-raw hl)}))

(vec (mapcat (fn [[x l]] (multi x l))))

(multi 10 10)

(->> almanac-t
  parse-almanac-v2)


; (defn mmap-v2 [seed maps] 
;   (let [s (get seed 0)
;         l (get seed 1)] seed))


; (loop [m maps]
;   (if (empty? m) n
;     (let [[d s l] (first m)]
;       (if (<= s n (+ s l))
;         (+ n (- d s))
;         (recur (next m)))))))

; (mmap-v2 [2988689842 2988689842] [[2988689842 2988689842 2] [52 50 48]])


;(+ n (- d s))

(defn mmap-v2 [maps seeds] 
  (loop [seeds seeds m maps r []]
    (cond 
      (empty? seeds) r
      (empty? m) (apply conj r seeds)
      :else (let [[start end] (first seeds)
                  [d s l] (first m)
                  ; _ (println seeds m
                  ;     (and (< start s) (< end (+ s l))) 
                  ;     " "
                  ;     (and (> start s) (> end (+ s l)))
                  ;     " "
                  ;     (and (>= start s) (<= end (+ s l)))
                  ;     " "
                  ;     (and (< start s) (> end (+ s l)))
                  ;     )
                  ]
              (if (or (< end s) (> start (+ s l)))
                (recur seeds (next m) r)
                (cond 
                  (and (< start s) (< end (+ s l))) (recur (conj (next seeds) [start (dec s)]) maps (conj r [d (- (+ end d) s)]))
                  (and (> start s) (> end (+ s l))) (recur (conj (next seeds) [(+ s l 1) end]) maps (conj r [(- (+ start d) s) (+ l d)]))
                  (and (>= start s) (<= end (+ s l))) (recur (next seeds) (next m) (conj r [(- (+ start d) s) (- (+ end d) s)]))
                  (and (< start s) (> end (+ s l))) (recur (conj (next seeds) [start (dec s)] [(+ s l 1) end]) maps (conj r [d (+ l d)]))
                  :else (recur (next seeds) (next m) r)))))))
  


(defn processing-v2 [{seeds :seeds ss :ss sf :sf fw :fw wl :wl lt :lt th :th hl :hl}]

    (->> seeds
      (mmap-v2 ss)
      (mmap-v2 sf)
      (mmap-v2 fw)
      (mmap-v2 wl)
      (mmap-v2 lt)
      (mmap-v2 th)
      (mmap-v2 hl)
      ))

(println "\n\n\n")

(->> almanac-t
  parse-almanac-v2
  processing-v2
  flatten
  (apply min)
  )



(->> almanac
  parse-almanac-v2
  processing-v2
  flatten
  (apply min)  )

824872000