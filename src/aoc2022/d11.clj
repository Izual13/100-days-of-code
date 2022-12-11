(ns aoc2022.d11
  (:require 
    [clojure.string :as str]
    [clj-async-profiler.core :as prof]))


(def test-input (clojure.string/split (slurp "resources/aoc2022/day11_t") #"\n\n"))
(def input (clojure.string/split (slurp "resources/aoc2022/day11_1") #"\n\n"))

(defn parse-monkey [input] 
  ; (println input)
  (let [lines (clojure.string/split input #"\n")
        items (loop [m (re-matcher #"\d+" (lines 1)) r []]
                (let [n (re-find m)] 
                  (if (nil? n) 
                    r
                    (recur m (conj r (bigint (Long/parseLong n)))))))
        [_ action n] (re-matches #"  Operation: new = old (.) (\d+)" (lines 2))
        [_ test] (re-matches #"  Test: divisible by (\d+)" (lines 3))
        [_ test-true] (re-matches #"    If true: throw to monkey (\d+)" (lines 4))
        [_ test-false] (re-matches #"    If false: throw to monkey (\d+)" (lines 5))]
    {:items items 
     :action action 
     :n (if (nil? n) nil (bigint (Long/parseLong n)))
     :test (Long/parseLong test) 
     :throw [(Long/parseLong test-true) (Long/parseLong test-false)]
     :counter (count items)}))


(defn proceed-items [i monkeys]
  (let [monkey (monkeys i)
        items (:items monkey)
        action (:action monkey)
        action (case action
                 nil *
                 "*" *
                 "+" +)
        n (:n monkey)
        test (:test monkey)
        [t-true t-false] (:throw monkey)
        ; _ (println monkey)
        ]
    (loop [i items m monkeys]
      (if (empty? i) 
        m
        (let [f (bigint (first i))
              ; _ (println f)
              tmp (action f (if (nil? n) f (bigint n)))
              tmp (bigint (/ (bigint tmp) 3))]
          (if (= 0 (mod tmp test)) 
            (recur (next i) (-> m
                              (assoc-in [t-true :items] (conj (get-in m [t-true :items]) tmp))
                              (update-in [t-true :counter] (fnil inc 0))
                              ))
            (recur (next i) (-> m
                              (assoc-in [t-false :items] (conj (get-in m [t-false :items]) tmp))
                              (update-in [t-false :counter] (fnil inc 0)))))
          ))
      )))


(* 100000000000 789432748934 4372894372894)
; {:items [79 98], :action "*", :n 19, :test 23, :throw [2 3]}

(defn proceed-round [monkeys]
  (loop [i 0 r monkeys]
    ; (println r)
    (if (= i (count monkeys)) 
      r
      (let [new-r (proceed-items i r)
            new-r (assoc-in new-r [i :items] [])]
        (recur (inc i)  new-r)))))

(defn proceed-rounds [n monkeys]
  (loop [i 0 r monkeys]
    ;(println (map #(:counter %) r))
    (if (= i n) 
      r
      (recur (inc i) (proceed-round r)))))

([1 2] 0)

(assoc-in [{:i [1 2 3]}] [0 :i] [])

;;[_ a b c d] (re-matches #"  Operation: new = old (.) (\d+)" s)

;(re-matcher #"\d+" "abc12345def")

(re-find #"\d+" "Starting items: 79, 98")

(->> test-input
  next
  next
  first
  parse-monkey)

(parse-monkey (str "Monkey 0:\n"
                "  Starting items: 79, 98\n"
                "  Operation: new = old * 19\n"
                "  Test: divisible by 23\n"
                "    If true: throw to monkey 2\n"
                "    If false: throw to monkey 3"))

(assert (= 10605 (->> test-input
                   (map parse-monkey)
                   vec
                   (proceed-rounds 20)
                   (map #(- (:counter %) (count (:items %))))
                   sort
                   (take-last 2)
                   (apply *))))


(assert (= 72884 (->> input
                   (map parse-monkey)
                   vec
                   (proceed-rounds 20)
                   (map #(- (:counter %) (count (:items %))))
                   sort
                   (take-last 2)
                   (apply *))))








(defn proceed-items2 [i monkeys]
  (let [monkey (monkeys i)
        items (:items monkey)
        action (:action monkey)
        action (case action
                 nil *
                 "*" *
                 "+" +)
        n (:n monkey)
        test (:test monkey)
        [t-true t-false] (:throw monkey)
        ; _ (println monkey)
        ]
    (loop [i items m monkeys]
      (if (empty? i) 
        m
        (let [f (bigint (first i))
              ; _ (println f)
              tmp (action f (if (nil? n) f (bigint n)))]
          (if (= 0 (mod tmp test)) 
            (recur (next i) (-> m
                              (assoc-in [t-true :items] (conj (get-in m [t-true :items]) tmp))
                              (update-in [t-true :counter] (fnil inc 0))
                              ))
            (recur (next i) (-> m
                              (assoc-in [t-false :items] (conj (get-in m [t-false :items]) tmp))
                              (update-in [t-false :counter] (fnil inc 0)))))
          ))
      )))


(defn proceed-round2 [monkeys]
  (loop [i 0 r monkeys]
    ; (println r)
    (if (= i (count monkeys)) 
      r
      (let [new-r (proceed-items2 i r)
            new-r (assoc-in new-r [i :items] [])]
        (recur (inc i)  new-r)))))

(defn proceed-rounds2 [n monkeys]
  (loop [i 0 r monkeys]
    (println (map #(:counter %) r))
    (if (= i n) 
      r
      (recur (inc i) (proceed-round2 r)))))




(->> test-input
  (map parse-monkey)
  vec
  (proceed-rounds2 1000)
  (map #(- (:counter %) (count (:items %))))
  sort
  (take-last 2)
  (apply *))

(6 10 3 5)
(52 54 6 52)
(537 469 15 544)


(assert (= 10605 (->> test-input
                   (map parse-monkey)
                   vec
                   (proceed-rounds 10000)
                   (map #(- (:counter %) (count (:items %))))
                   sort
                   (take-last 2)
                   (apply *))))
