(ns  aoc2020.d4
  (:require [clojure.string :as str]))


(def test-passports (clojure.string/split (slurp "resources/aoc2020/day4_t") #"\n\n"))
(def passports (clojure.string/split (slurp "resources/aoc2020/day4_1") #"\n\n"))

(defn parse-fields [fields]
  (loop [l fields r {}]
    (if (empty? l) 
      r
      (let [f (first l)
          [_ k v] (re-matches #"(\w+):(.*)" f)]
      (recur (next l) (assoc r k v))))))

(assert (= 2 (->> test-passports
  (mapv #(clojure.string/split % #"[\n| ]"))
  (mapv parse-fields)
  (filter #(or (= 8 (count %)) (and (= 7 (count %)) (not (contains? % "cid")))))
  count)))

(assert (= 170 (->> passports
  (mapv #(clojure.string/split % #"[\n| ]"))
  (mapv parse-fields)
  (filter #(or (= 8 (count %)) (and (= 7 (count %)) (not (contains? % "cid")))))
  count)))


(assert (= 103 (->> passports
  (mapv #(clojure.string/split % #"[\n| ]"))
  (mapv parse-fields)
  (filter #(or (= 8 (count %)) (and (= 7 (count %)) (not (contains? % "cid")))))
  (filter #(let [byr (Long/parseLong (% "byr"))] (<= 1920 byr 2002)))
  (filter #(let [iyr (Long/parseLong (% "iyr"))] (<= 2010 iyr 2020)))
  (filter #(let [eyr (Long/parseLong (% "eyr"))] (<= 2020 eyr 2030)))
  (filter #(let [hgt (% "hgt")
                 [_ n r] (re-matches #"(\d+)(.*)" hgt)
                 n (Long/parseLong n)] 
             (or (and (= r "cm") (<= 150 n 193))
                 (and (= r "in") (<= 59 n 76)))))
  (filter #(let [hcl (% "hcl")] (not (str/blank? (re-matches #"#[a-f0-9]{6}" hcl)))))
  (filter #(let [ecl (% "ecl")] (or (= ecl "amb") 
                                  (= ecl "blu")
                                  (= ecl "brn")
                                  (= ecl "gry")
                                  (= ecl "grn")
                                  (= ecl "hzl")
                                  (= ecl "oth"))))
  (filter #(let [pid (% "pid")] (not (str/blank? (re-matches #"[0-9]{9}" pid)))))
  count)))


(assert (= true (not (str/blank? (re-matches #"#[a-f0-9]{6}" "#123abc")))))
(assert (= false (not (str/blank? (re-matches #"#[a-f0-9]{6}" "#123abz")))))
(assert (= false (not (str/blank? (re-matches #"#[a-f0-9]{6}" "123abc")))))

