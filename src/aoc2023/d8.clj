(ns  aoc2023.d8
  (:require [clojure.string :as str]))

(def network (clojure.string/split (slurp "resources/aoc2023/day8_1") #"\r?\n\r?\n"))

(def network-t (clojure.string/split (slurp "resources/aoc2023/day8_t") #"\r?\n\r?\n"))

(def network-t2 (clojure.string/split (slurp "resources/aoc2023/day8_t2") #"\r?\n\r?\n"))

(def network-t3 (clojure.string/split (slurp "resources/aoc2023/day8_t3") #"\r?\n\r?\n"))

network-t

(defn parse-array [s] 
  (let [i (vec (first s))
        m (clojure.string/split (second s) #"\r?\n")
        m (map #(let [[_ a b c] (re-matches #"(\w+) = \((\w+), (\w+)\)" %1)] {a [b c]}) m)
        m (apply merge m)
        ]
    {:instructions i :maps m}))

(assert (= {:instructions [\R \L],
            :maps
            {"AAA" ["BBB" "CCC"],
             "BBB" ["DDD" "EEE"],
             "CCC" ["ZZZ" "GGG"],
             "DDD" ["DDD" "DDD"],
             "EEE" ["EEE" "EEE"],
             "GGG" ["GGG" "GGG"],
             "ZZZ" ["ZZZ" "ZZZ"]}}
          ) (->> 
              network-t
              parse-array))

(assert (= {"AAA" ["BBB" "CCC"]} (let [[_ a b c] (re-matches #"(\w+) = \((\w+), (\w+)\)" "AAA = (BBB, CCC)")] {a [b c]})))

(defn find-path [{instructions :instructions maps :maps}] 
  (loop [i instructions p "AAA" c 0]
    (cond 
      (= p "ZZZ") c
      (empty? i) (recur instructions p c)
      :else (let [f (first i)]
              (if (= f \L)
                (recur (next i) (get-in maps [p 0]) (inc c))
                (recur (next i) (get-in maps [p 1]) (inc c)))))))


(assert (= 2 (->> 
               network-t
               parse-array
               find-path)))

(assert (= 6 (->> 
               network-t2
               parse-array
               find-path)))

(assert (= 19199 (->> 
                   network
                   parse-array
                   find-path)))


{:instructions [\R \L],
 :map
 {"AAA" ["BBB" "CCC"],
  "BBB" ["DDD" "EEE"],
  "CCC" ["ZZZ" "GGG"],
  "DDD" ["DDD" "DDD"],
  "EEE" ["EEE" "EEE"],
  "GGG" ["GGG" "GGG"],
  "ZZZ" ["ZZZ" "ZZZ"]}}

;;; part2

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (if (or (zero? a) (zero? b))
    0
    (/ (Math/abs (* a b)) (gcd a b))))

(defn lcm-of-list [numbers]
  (reduce lcm numbers))

(assert (= 13663968099527 (lcm-of-list [17621 11309 20777 13939 15517 19199])))

(defn find-path-v3 [{instructions :instructions maps :maps}]
  (for [p (filterv #(= \A (last %1)) (keys maps))]
    (loop [i instructions p p c 0]
      (cond 
        (and (empty? i) (= (last p) \Z)) c
        (empty? i) (recur instructions p c)
        :else (let [f (first i)]
                (if (= f \L)
                  (recur (next i) (get-in maps [p 0]) (inc c))
                  (recur (next i) (get-in maps [p 1]) (inc c))))))))

(assert (= 13663968099527 (->> 
                            network
                            parse-array
                            find-path-v3
                            lcm-of-list)))

