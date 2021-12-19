(ns  aoc2021.d14
  (:require [clojure.string :as str]))



(def input-from-file-test
  (let [[polymer pairs] (str/split (slurp "resources/aoc2021/day14_t") #"\r?\n\r?\n")
        parsed-pairs (->> pairs
                          (str/split-lines)
                          (mapv #(let [[_ a b c] (re-matches #"(\w)(\w) -> (\w)" %)] {(str a b) [(str a c) (str c b)]}))
                          (into {}))]
    {:polymer polymer :pairs parsed-pairs}))


(assert (= ["CH" "B"] (let [[_ c p] (re-matches #"(\w+) -> (\w+)" "CH -> B")] [c p])))


(def input-from-file
  (let [[polymer pairs] (str/split (slurp "resources/aoc2021/day14_1") #"\r?\n\r?\n")
        parsed-pairs (->> pairs
                          (str/split-lines)
                          (mapv #(let [[_ a b c] (re-matches #"(\w)(\w) -> (\w)" %)] {(str a b) [(str a c) (str c b)]}))
                          (into {}))]
    {:polymer polymer :pairs parsed-pairs}))

(defn build-map [polymer]
  (->>
   (for [i (range 1 (count polymer))] (str (get polymer (dec i)) (get polymer i)))
   (reduce #(update %1 %2 (fnil inc 0)) {})))

(assert (=  {"NN" 1, "NC" 1, "CB" 1} (build-map "NNCB")))
(assert (=  {"NN" 2, "NC" 1, "CN" 1, "NB" 1} (build-map "NNCNNB")))



(defn step [m i] 
  ;;(println "step!!!")
  (loop [pairs (filter (fn [[_ v]] (> v 0)) m) r m]
    (if (empty? pairs) r
        (let [[k c] (first pairs)
              new-values (get i k)
              new-r (update r k (fn [x] (- x c)))
              new-r (reduce #(update %1 %2 (fnil (fn [x] (+ c x)) 0) ) new-r new-values)
              ]
          ;;(println "k" k "new-r" new-r)
              
          (recur (rest pairs) new-r)))))

(defn part1 [input]
  (let [first-char (first (:polymer input))
        last-char (last (:polymer input))
        _ (println "first-char" first-char "last-char" last-char)
        _ (println input)
        m (build-map (:polymer input))
        new-m (loop [i 0 r m] 
                (if (= i 10) r 
                    (recur (inc i) (step r (:pairs input)))))
        
        char-kv (->> new-m
                 (map (fn [[k v]] [{(first k) v} {(second k) v}]))
                 (flatten)
                 (reduce #(assoc %1 (key (first %2)) (+ (val (first %2)) (get %1 (key (first %2)) 0))) {}))
        
        _ (println "char-kv" char-kv)

        char-kv (-> char-kv
                    (update first-char inc)
                    (update last-char inc))
        
        ;; char-kv (reduce #(update %1 ) {} char-kv)
        _ (println "char-kv" char-kv)
        _ (println "sum char-kv" (apply + (map val char-kv)))
        ] (- (/ (apply max (map val char-kv)) 2) (/ (apply min (map val char-kv)) 2))))



(frequencies "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")


(assert (= 1588 (part1 input-from-file-test)))
(assert (= 2223 (part1 input-from-file)))

(apply + (map val {"C" 22, "H" 14, "B" 38, "N" 18}))

(defn part2 [input]
  (let []))

(assert (= 16 (time (part2 input-from-file-test))))
(assert (= 93 (time (part2 input-from-file))))

(- 5644 1790)
(/ 3854 2)
