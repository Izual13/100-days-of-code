(ns  aoc2021.d13
  (:require [clojure.string :as str]))


(def input-from-file-test
  (let [[paper insrruction] (str/split (slurp "resources/aoc2021/day13_t") #"\n\n")
        parsed-paper (->> paper
                          (str/split-lines)
                          (mapv #(str/split % #","))
                          (mapv #(mapv (fn [x] (Long/parseLong x)) %)))

        parsed-insrructions (->> insrruction
                                 (str/split-lines)
                                 (mapv #(let [[_ c p] (re-matches #"fold along (\w+)=(\d+)" %)] [c (Long/parseLong p)])))]
    {:points parsed-paper :insrructions parsed-insrructions}))


(assert (= ["x" "655"] (let [[_ c p] (re-matches #"fold along (\w+)=(\d+)" "fold along x=655")] [c p])))


(def input-from-file
  (let [[paper insrruction] (str/split (slurp "resources/aoc2021/day13_1") #"\n\n")
        parsed-paper (->> paper
                          (str/split-lines)
                          (mapv #(str/split % #","))
                          (mapv #(mapv (fn [x] (Long/parseLong x)) %)))

        parsed-insrructions (->> insrruction
                                 (str/split-lines)
                                 (mapv #(let [[_ c p] (re-matches #"fold along (\w+)=(\d+)" %)] [c p])))]
    {:points parsed-paper :insrructions parsed-insrructions}))

(defn get-max [m]
  (let [x (apply max (map first m))
        y (apply max (map second m))] [(inc x) (inc y)]))

(assert (= [6 5] (get-max [[1 2] [3 4] [5 0]])))

(defn build-matrix [points]
  (let [[maxx maxy] (get-max points)
        _ (println maxx maxy)
        result (vec (for [_ (range maxy)] (vec (for [_ (range maxx)] false))))
        ;; _ (println "result:" result)
        result (loop [p points r result]
                ;;  (println "p" (first p))
                 (if (empty? p)
                   r
                   (recur (rest p) (assoc-in r (reverse (first p)) true))))]
    {:x maxx :y maxy :matrix result}))

(defn fold-matrix [m [d p]]
  (println d p)
  ;(println "(get-in m [y x])" (get-in m [11 1]))
  (if (= d "y") 
    (let [matrix (:matrix m)
          
          sharps (for [y (range p (:y m)) 
                       x (range 0 (:x m))
                       :when (get-in matrix [y x])]
            [y x])
          _ (println "sharps" sharps)
          ] m) 
    m))

(defn part1 [input]
  (let [matrix (build-matrix (:points input))
        new-matrix (fold-matrix matrix (first (:insrructions input)))
        _ (println (map println (:matrix new-matrix)))]
    matrix))

(assert (= 10 (part1 input-from-file-test)))
(assert (= 4970 (part1 input-from-file)))


(defn part2 [input])

(assert (= 36 (time (part2 input-from-file-test))))
(assert (= 137948 (time (part2 input-from-file))))


