(ns aoc2022.d5
  (:require [clojure.string :as str]))

(def test-input (clojure.string/split (slurp "resources/aoc2022/day5_t") #"\n\n"))
(def input (clojure.string/split (slurp "resources/aoc2022/day5_1") #"\n\n"))


(defn parse-moves [s]
  (let [[_ a b c] (re-matches #"move (\d*) from (\d*) to (\d*)" s)]
    [(Integer/parseInt a) (Integer/parseInt b) (Integer/parseInt c)]))

(assert (= [28 23 17] (parse-moves "move 28 from 23 to 17")))

(defn parse-letter [s]
  (let [[_ a] (re-matches #"\[(.)\]\s?" s)]
    a))

(defn parse-stack [s] 
  (loop [s s r []] 
    (if (empty? s) 
      r
      (let [f (parse-letter (subs s 0 (min 4 (count s))))]
        (recur (subs s (min 4 (count s))) (conj r f))))))

(assert (= [nil "D" nil] (parse-stack "    [D]    ")))
(assert (= ["N" "C" nil] (parse-stack "[N] [C]    ")))
(assert (= ["Z" "M" "P"] (parse-stack "[Z] [M] [P]")))


(defn parse-input [[s m]] 
  (let [l (str/split-lines s)
        c (/ (+ 1 (count (first l))) 4)
        stacks (loop [l l r (vec (for [x (range c)] []))]
                 (if (empty? l) 
                   r
                   (let [p (parse-stack (first l))
                         new-r (vec (map-indexed (fn [i v] (if (nil? v) (get r i) (conj (get r i) v))) p))
                         ]
                     (recur (rest l) new-r))))
        ] 
    [stacks (map parse-moves (str/split-lines m))]))

(assert (= [[["N" "Z"] ["D" "C" "M"] ["P"]] `([1 2 1] [3 1 3] [2 2 1] [1 1 2])] (parse-input test-input)))


(defn rearrangement [[stacks moves]] 
  (loop [m moves s stacks] 
    (if (empty? m)
      s
      (let [[c from to] (first m)
            new-s (loop [i 0 s s] 
                    (if (= i c) 
                      s
                      (let [e (first (get s (- from 1)))
                            ]
                        (if (nil? e)
                          s
                          (let [new-s (update s (- from 1) rest)
                                new-s (update new-s (- to 1) #(cons e %))]
                            (recur (inc i) new-s))
                          ))))]
        (recur (rest m) new-s)))))

(assert (= "CMZ" 
          (->> test-input
            (parse-input)
            rearrangement
            (map first)
            (apply str))))

(assert (= "SBPQRSCDF" 
          (->> input
            (parse-input)
            rearrangement
            (map first)
            (apply str))))


(defn rearrangement-2 [[stacks moves]] 
  (loop [m moves s stacks] 
    (if (empty? m)
      s
      (let [[c from to] (first m)
            e (take c (get s (- from 1)))
            new-s (update s (- from 1) #(nthrest % c))
            new-s (update new-s (- to 1) #(reduce (fn [element array] (cons array element)) % (reverse e)))
            ]
        (recur (rest m) new-s)))))

(assert (= ["D" "N" "Z" "P"] (reduce #(cons %2 %1) ["Z" "P"] (reverse ["D" "N"]))))

(assert (= "MCD" 
          (->> test-input
            (parse-input)
            rearrangement-2
            (map first)
            (apply str))))

(assert (= "RGLVRCQSB" 
          (->> input
            (parse-input)
            rearrangement-2
            (map first)
            (apply str)
            )))