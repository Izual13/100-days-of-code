(ns  aoc2024.d9
  (:require [clojure.string :as str]))

(def test-disk-map (vec (slurp "resources/aoc2024/d9_t")))
(def disk-map (vec (slurp "resources/aoc2024/d9_1")))


(defn vec-insert [v i e] (vec (concat (take i v) [e] (drop i v))))
(defn vec-remove [v i] (into (subvec v 0 i) (subvec v (inc i))))

(defn calculate [m]
  (loop [m m s 0 e (count m) i 0 f 0 r 0]
    (cond 
      (> s e) r
      (= 1 (mod s 2)) (recur m (inc s) e i (+ f (get m s)) r)
      (and (> f 0) (= 1 (mod e 2)) (> (get m (dec e)) 0)) (recur (update m (dec e) dec) s e (inc i) (dec f) (+ r (* i (int (/ e 2)))))
      (and (> f 0) (= 1 (mod e 2)) (= (get m (dec e)) 0)) (recur m s (- e 2) i f r)
      (> (get m s) 0) (recur (update m s dec) s e (inc i) f (+ r (* i (int (/ s 2)))))
      :else (recur m (inc s) e i f r))))


(assert (= 1928 (->> test-disk-map
  (mapv #(- (int %) 48))
  calculate)))

(assert (= 6435922584968 (->> disk-map
  (mapv #(- (int %) 48))
  calculate)))


(defn parse-disk-map [m]
  (loop [m m i 0 id 0 r []]
    (cond
      (empty? m) r
      (= 0 (mod i 2)) (recur (next m) (inc i) (inc id) (conj r {:r (first m) :id id}))
      :else (recur (next m) (inc i) id (conj r {:f (first m)})))))



(defn find-free-space [m s]
  (let [c (count m)]
    (loop [i 0]
      (cond 
        (= i c) nil
        (contains? (get m i) :r) (recur (inc i))
        (>= ((get m i) :f) s) i
        :else (recur (inc i))))))

(defn compact-disk [m] 
  (loop [m m r (dec (count m))] 
    (cond 
      (= r 0) m
      (contains? (get m r) :f) (recur m (dec r))
      :else (let [f (get m r)
                  i (find-free-space m (f :r))]
        (if (nil? i)
          (recur m (dec r))
          (let [reminder (- ((get m i) :f) (f :r))]
            (cond 
              (> i r) (recur m (dec r))
              (= reminder 0) (recur (-> m
                                      (assoc r {:f (f :r)})
                                      (vec-remove i)
                                      (vec-insert i {:r (f :r) :id (f :id)})) (dec r))
              :else (recur (-> m
                             (assoc r {:f (f :r)})
                             (vec-remove i)
                     		 (vec-insert i {:f reminder})
                             (vec-insert i {:r (f :r) :id (f :id)})) (dec r)))))))))

(defn calculate-2 [m]
  (let [c (count m)]
    (loop [m m i 0 ir 0 r 0]
      (if (= c i) 
        r
      	(let [f (get m i)]
          (cond 
            (contains? f :f) (recur m (inc i) (+ ir (f :f)) r)
            (= 0 (f :r)) (recur m (inc i) ir r)
            :else (recur (update-in m [i :r] dec) i (inc ir) (+ r (* ir (f :id))))))))))

(assert (= 2858 (->> test-disk-map
  (mapv #(- (int %) 48))
  parse-disk-map
  compact-disk
  calculate-2)))

(assert (= 6469636832766 (->> disk-map
  (mapv #(- (int %) 48))
  parse-disk-map
  compact-disk
  calculate-2)))