(ns aoc2022.d11
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(def test-input (clojure.string/split (slurp "resources/aoc2022/day12_t") #"\n"))
(def input (clojure.string/split (slurp "resources/aoc2022/day12_1") #"\n"))

(defn parse-input [r] 
  (mapv int r))

(defn find-letter [m l]
  (let [max-j (count m)
        max-i (count (m 0))
        l-code (int l)]
    (loop [i 0 j 0] 
      (cond
        (= l-code (get-in m [j i])) [j i]
        (and (= i max-i) (= j max-j)) nil
        (= i max-i) (recur 0 (inc j))
        :else (recur (inc i) j)))))


(defn update-results [p r n] 
  (let [cv (inc (get-in r p))]
    (loop [n n r r]
      (if (empty? n)
        r
        (recur (next n) (update-in r (first n) #(if (>= % cv) cv %)))))))

(defn find-path [m] 
  (let [max-i (count (m 0))
        max-j (count m)
        s (find-letter m \S)
        e (find-letter m \E)
        r (vec (for [j (range max-j)]
                 (vec (for [i (range max-i)] 1000000000))))
        r (assoc-in r s 0)
        m (assoc-in m s (int \a))
        m (assoc-in m e (int \z))]
    (loop [q (conj [] s) v (conj #{} s) r r]
      (if (empty? q) 
        (get-in r e)
        ; r
        (let [[j i :as p] (first q)
              rest-q (vec (rest q))
              n (for [new-p [[j (inc i)] [(dec j) i] [j (dec i)] [(inc j) i]]
                      :let [[new-j new-i] new-p]
                      :when (and 
                              (>= new-i 0)
                              (>= new-j 0)
                              (< new-i max-i)
                              (< new-j max-j)
                              (not (.contains v new-p))
                              (>= (- (get-in m p) (get-in m new-p)) -1))]
                  [new-j new-i])]
          (if (= p [20 1]) (println "p" p "n" n ) nil)
          (if (empty? n)
            (recur rest-q v r)
            (recur (apply conj rest-q n) (apply conj v n) (update-results p r n))))))))
    

(assert (= [1 2 3 4] (conj [1 2 3] 4)))
(assert (= [4 1 2 3] (conj `(1 2 3) 4)))


(assert (= 31 (->> test-input
                (mapv parse-input)
                find-path)))

(assert (= 420 (->> input
                 (mapv parse-input)
                 find-path)))


(defn find-reverse-path [m] 
  (let [max-i (count (m 0))
        max-j (count m)
        s (find-letter m \S)
        e (find-letter m \E)
        r (vec (for [j (range max-j)]
                 (vec (for [i (range max-i)] 1000000000))))
        r (assoc-in r e 0)
        m (assoc-in m s (int \a))
        m (assoc-in m e (int \z))]
    (loop [q (conj [] e) v (conj #{} e) r r]
      (if (empty? q)
        (reduce min (for [i (range max-i)
                          j (range max-j)
                          :when (= (get-in m [j i]) (int \a))]
                      (get-in r [j i])))
        (let [[j i :as p] (first q)
              rest-q (vec (rest q))
              n (for [new-p [[j (inc i)] [(dec j) i] [j (dec i)] [(inc j) i]]
                      :let [[new-j new-i] new-p]
                      :when (and 
                              (>= new-i 0)
                              (>= new-j 0)
                              (< new-i max-i)
                              (< new-j max-j)
                              (not (.contains v new-p))
                              (>= (- (get-in m new-p) (get-in m p)) -1)
                              )]
                  [new-j new-i])]
          (if (empty? n)
            (recur rest-q v r)
            (recur (apply conj rest-q n) (apply conj v n) (update-results p r n))))))))

(assert (= 29 (->> test-input
                (mapv parse-input)
                find-reverse-path)))


(assert (= 414 (->> input
                 (mapv parse-input)
                 find-reverse-path)))