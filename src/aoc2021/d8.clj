(ns  aoc2021.d8
  (:require [clojure.string :as str]))

(def input-from-file-test
  (->> (slurp "resources/aoc2021/day8_t")
       (str/split-lines)
       (mapv #(str/split % #" \| "))))

(def input-from-file
  (->> (slurp "resources/aoc2021/day8_1")
       (str/split-lines)
       (mapv #(str/split % #" \| "))))


(defn part1
  ([input]
   (->> input
        (mapv #(last %))
        (mapv #(str/split % #" "))
        (flatten)
        (filter #(let [c (count %)] (or (= c 2) (= c 3) (= c 4) (= c 7))))
        (count))))


(assert (= 26 (part1 input-from-file-test)))
(assert (= 310 (part1 input-from-file)))

(defn filter-number
  ([p f] (->> p (filter #(= (count %) f))))
  ([p f c-e] (let [result (filter-number p f)
                   result (filter #(every? (fn [x] (contains? (set %) x)) c-e) result)] result)))


(defn find-by-map [m v]
  (str (symbol (first (keep #(when (= (val %) v) (key %)) m)))))

(assert (= "3" (find-by-map {:1 123 :3 345} 345)))


(defn parse-row [input] (let [[patterns output] input
                              patterns (-> patterns (str/split #"\s"))
                              patterns (->> patterns
                                            (mapv #(vec (sort %))))
                              output (-> output (str/split #"\s"))
                              output (->> output
                                          (mapv #(apply str (sort %))))
                              numbers {:1 (first (filter-number patterns 2))
                                       :4 (first (filter-number patterns 4))
                                       :7 (first (filter-number patterns 3))
                                       :8 (first (filter-number patterns 7))}

                              numbers (assoc numbers :3 (first (filter-number patterns 5 (:1 numbers))))
                              numbers (assoc numbers :9 (first (filter-number patterns 6 (:4 numbers))))
                              numbers (assoc numbers :0 (first (filter #(not= % (:9 numbers)) (filter-number patterns 6 (:1 numbers)))))

                              numbers (assoc numbers :6 (first (filter #(and (not= % (:9 numbers)) (not= % (:0 numbers))) (filter-number patterns 6))))

                              numbers (assoc numbers :5 (first (filter #(every? (fn [x] (contains? (set (:6 numbers)) x)) (set %)) (filter-number patterns 5))))
                              numbers (assoc numbers :2 (first (filter #(and (not= % (:5 numbers)) (not= % (:3 numbers))) (filter-number patterns 5))))


                              numbers (loop [n numbers r {}]
                                        (if (empty? n)
                                          r
                                          (recur (rest n) (assoc r (first (first n)) (apply str (second (first n)))))))]


                          (Integer/parseInt (apply str (map #(find-by-map numbers %) output)))))



(defn part2
  ([input] (apply + (map parse-row input))))



(assert (= 61229 (part2 input-from-file-test)))
(assert (= 915941 (part2 input-from-file)))

