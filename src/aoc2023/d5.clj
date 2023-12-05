(ns  aoc2023.d5
  (:require [clojure.string :as str]))


(def almanac (clojure.string/split (slurp "resources/aoc2023/day5_1") #"\r?\n\r?\n"))

(def almanac-t (clojure.string/split (slurp "resources/aoc2023/day5_t") #"\r?\n\r?\n"))


(defn parse-array [s] 
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

(assert (= ["seeds: 79 14 55 13" "79 14 55 13"] (re-matches #"seeds: (.*)" "seeds: 79 14 55 13")))

(assert (= [[79 14] [55 13]] (partition 2 [79 14 55 13])))


(defn mmap [n maps] 
  (loop [m maps]
    (if (empty? m) n
      (let [[d s l] (first m)]
        (if (<= s n (+ s l))
          (+ n (- d s))
          (recur (next m)))))))

(assert (= 81 (mmap 79 [[50 98 2] [52 50 48]])))



(defn processing [{seeds :seeds ss :ss sf :sf fw :fw wl :wl lt :lt th :th hl :hl}]
  (for [s seeds] 
    (-> s
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

(defn mmap-v2 [maps seeds] 
  (for [s seeds]
    (loop [seeds [s] m maps r []]
      (cond 
        (empty? seeds) r
        (empty? m) (apply conj r seeds)
        :else (let [[start end] (first seeds)
                    [d s l] (first m)]
                (if (or (< end s) (> start (+ s l)))
                  (recur seeds (next m) r)
                  (cond 
                    (and (< start s) (< end (+ s l))) (recur (conj (next seeds) [start (dec s)]) maps (conj r [d (- (+ end d) s)]))
                    (and (> start s) (> end (+ s l))) (recur (conj (next seeds) [(+ s l 1) end]) maps (conj r [(- (+ start d) s) (+ l d)]))
                    (and (>= start s) (<= end (+ s l))) (recur (next seeds) (next m) (conj r [(- (+ start d) s) (- (+ end d) s)]))
                    (and (< start s) (> end (+ s l))) (recur (conj (next seeds) [start (dec s)] [(+ s l 1) end]) maps (conj r [d (+ l d)]))
                    :else (recur (next seeds) (next m) r))))))))
  


(defn processing-v2 [{seeds :seeds ss :ss sf :sf fw :fw wl :wl lt :lt th :th hl :hl}]
  (->> seeds
    (mmap-v2 ss)
    (mapcat #(mmap-v2 sf %1))
    (mapcat #(mmap-v2 fw %1))
    (mapcat #(mmap-v2 wl %1))
    (mapcat #(mmap-v2 lt %1))
    (mapcat #(mmap-v2 th %1))
    (mapcat #(mmap-v2 hl %1))))



(assert (= 46 (->> almanac-t
                parse-almanac-v2
                processing-v2
                flatten
                (apply min))))



(assert (= 100165128 (->> almanac
                       parse-almanac-v2
                       processing-v2
                       flatten
                       (apply min))

