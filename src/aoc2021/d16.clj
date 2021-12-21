(ns  aoc2021.d16
  (:require [clojure.string :as str]))

(def input-from-file-test
  (-> (slurp "resources/aoc2021/day16_t")
      (str/trim)))


(def input-from-file
  (-> (slurp "resources/aoc2021/day16_1")
      (str/trim)))


(defn toBin [c] (case c
                  \0  "0000"
                  \1  "0001"
                  \2  "0010"
                  \3  "0011"
                  \4  "0100"
                  \5  "0101"
                  \6  "0110"
                  \7  "0111"
                  \8  "1000"
                  \9  "1001"
                  \A  "1010"
                  \B  "1011"
                  \C  "1100"
                  \D  "1101"
                  \E  "1110"
                  \F  "1111"))


(assert (= "110100101111111000101000" (apply str (map toBin "D2FE28"))))
(assert (= "00111000000000000110111101000101001010010001001000000000" (apply str (map toBin "38006F45291200"))))

(assert (= "100010100000000001001010100000000001101010000000000000101111010001111000" (apply str (map toBin "8A004A801A8002F478"))))




(defn toInt ([s]
            ;;  (println "toInt: " s)
             (if (empty? s)
               0
               (Long/parseLong s 2)))
  ([s & xs] (toInt (apply str s xs))))


(assert (= 6 (toInt "1" "1" "0")))
(assert (= 6 (toInt "110")))

(toInt "10010110001010010011000101100001")
(toInt "000000000011011")


(defn parse-literal [[marker & xs]]
  (loop [m marker o xs r []]
    (if (= m \0)
      (let [_ (comment println (apply str (apply conj r (take 4 o))))] {:value [(toInt (apply str (apply conj r (take 4 o))))] :tail (drop 4 o)})
      (let [f4 (take 4 o)
            new-m (first (drop 4 o))
            new-o (drop 5 o)]
        (recur new-m new-o (apply conj r f4))))))

(assert (= [2021]  (:value (parse-literal "101111111000101000"))))
(assert (= "10111" (subs "101111111000101000" 0 5)))

(parse-literal (subs "11010001010" 6))


(defn parse-operator [[i & xs]]
  ;; (println "I" i "tail" (apply str xs))
  (case i
    \0 (let [l (toInt (apply str (take 15 xs)))

             p (loop [o (take l (drop 15 xs)) r []]
                 (if (empty?  o)
                   r
                   (let [subp (parse-packet o)]
                     (recur (:tail subp) (conj r subp)))))
             ]
         {:children p :tail (drop (+ l 15) xs)})
    \1 (let [l (toInt (apply str (take 11 xs)))
            ;;  _ (println "count packets" l)
             p (loop [o (drop 11 xs) r [] i 0]
                 (if (=  i l)
                   r
                   (let [subp (parse-packet o)]
                     (recur (:tail subp) (conj r subp) (inc i)))))]
         {:children p :tail (:tail (last p))})
    :else (println "!!!!!" i)))

(defn parse-packet [[v1 v2 v3 t1 t2 t3 & xs]]
  (let [v (toInt (str v1 v2 v3))
        t (toInt (str t1 t2 t3))]
    ;; (println "tag t" t "tag v" v)
    (-> (case t
          4 (parse-literal xs)
          (parse-operator xs))
        (assoc :v v))))

(defn part1 [input]
  (let [packets (parse-packet (apply str (map toBin input)))
        sumv (loop [q [packets] r 0]
               (if (empty? q)
                 r
                 (let [f (first q)]
                   (if (nil? (:children f))
                     (recur (rest q) (+ r (:v f)))
                     (recur (apply conj (:children f) (rest q)) (+ r (:v f)))))))] 
    sumv))


(assert (= 16 (part1 "8A004A801A8002F478")))
(assert (= 12 (part1 "620080001611562C8802118E34")))
(assert (= 23 (part1 "C0015000016115A2E0802F182340")))
(assert (= 31 (part1 "A0016C880162017C3686B18A3D4780")))

(assert (= 1002 (time (part1 input-from-file))))



(defn part2 [input]
  (let [packets (parse-packet (toBin input))] packets))

(assert (= 315 (time (part2 input-from-file-test))))
(assert (= 2821 (time (part2 input-from-file))))


