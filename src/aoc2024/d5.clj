(ns  aoc2024.d5
  (:require [clojure.string :as str]))



(def test-pages (str/split (slurp "resources/aoc2024/d5_t") #"\n\n"))
(def pages (str/split (slurp "resources/aoc2024/d5_1") #"\n\n"))


(defn parse-pages [[rules pages]]
  (let [r (str/split rules #"\n")
        new-r (mapv #(let [[_ x y] (re-matches #"(\d*)\|(\d*)" %)] [(Integer/parseInt x) (Integer/parseInt y)]) r)
        p (str/split pages #"\n")
        p (mapv #(let [t (str/split % #",")] (mapv (fn [x] (Integer/parseInt x)) t)) p)] [(group-by first new-r) p (set r)]))


(defn check-page [rules pages]
  (loop [p pages v #{}]
    (if (empty? p) 
      true
      (let [f (first p)
            previous-numbers (mapv second (get rules f))]
        (if (not (every? #(not (contains? v %)) previous-numbers))
          false
          (recur (next p) (conj v (first p))))))))

(defn middle-value [vect]
  (when-not (empty? vect)
    (vect (quot (count vect) 2))))

(defn calculate [[rules pages]]
  (loop [p pages r 0]
    (cond 
      (empty? p) r
      (check-page rules (first p)) (recur (next p) (+ r (middle-value (first p))))
      :else (recur (next p) r))))

(assert (= 143 (->> test-pages
  parse-pages
  calculate)))

(assert (= 7365 (->> pages
  parse-pages
  calculate)))

(defn calculate-2 [[rules pages raw-rules]]
	(loop [p pages r 0]
	 (cond 
	   (empty? p) r
	   (check-page rules (first p)) (recur (next p) r)
	  	:else (recur (next p) (+ r (middle-value (vec (sort (fn [x1 x2] (cond 
	                                                                  (contains? raw-rules (str/join "|" [x1 x2])) 1
	                                                                  (contains? raw-rules (str/join "|" [x2 x1])) -1
	                                                                  :else 0
	                                                                  )) (first p)))))))))

(assert (=  123 (->> test-pages
  parse-pages
  calculate-2)))

(assert (= 5770 (->> pages
  parse-pages
  calculate-2)))


