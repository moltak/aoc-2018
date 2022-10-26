(ns hello-iced.2020.aoc4-1
  (:require [clojure.string :as str]
            [clojure.string :as s]))

(comment 
  " Day 4

  https://adventofcode.com/2020/day/4
  파트 1

  여권이 유효한지 판단하려고 한다. 여권에는 다음과 같은 필드가 있음.

  byr (Birth Year)
  iyr (Issue Year)
  eyr (Expiration Year)
  hgt (Height)
  hcl (Hair Color)
  ecl (Eye Color)
  pid (Passport ID)
  cid (Country ID)

  파트 1에서는 여권의 모든 필드가 존재하는지의 여부를 검사한다. 주어진 입력에서 '유효한' 여권의 숫자를 반환하여라.

  ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
  byr:1937 iyr:2017 cid:147 hgt:183cm

  iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
  hcl:#cfa07d byr:1929

  hcl:#ae17e1 iyr:2013
  eyr:2024
  ecl:brn pid:760753108 byr:1931
  hgt:179cm

  hcl:#cfa07d eyr:2025 pid:166559648
  iyr:2011 ecl:brn hgt:59in

  첫번째는 유효한 여권이다. 8개의 필드가 전부 존재한다.
  두번째는 유효하지 않다. hgt가 없기 때문.
  세번째는 cid가 없지만, ** cid는 없어도 되는 ** 것으로 간주한다. 그래서 유효하다.
  네번째는 cid와 byr이 없다. byr은 반드시 있어야하는 필드이므로 유효하지 않다. ")


(def input4-1 (slurp "resources/2020/aoc4_1.input"))

(def test-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(defn raw-string->개인여권-string
  [raw-string]
  (->> (str/split-lines raw-string)
       (reduce (fn [acc x] 
                 (if (= "" x) 
                   (conj acc [])
                   (assoc acc (dec (count acc)) (conj (last acc) x))))
               [[]])
       (map (fn [x] (str/join " " x)))))

(defn 개인여권-string->여권map
  [x]
  (->> (str/split x #" ")
       (map (fn [x] (str/split x #":")))
       (apply conj {})))

(defn 유효하지-않음
  [x]
  (->> x
       (dissoc x "cid")
       ((comp (partial = 7) count keys))))

(comment
  (->> test-input
       raw-string->개인여권-string
       (map (fn [x] (개인여권-string->여권map x)))
       (map (fn [x] (유효하지-않음 x)))
       (filter true?)
       count)

  (def test-input-1 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm")

  (->> input4-1 
       raw-string->개인여권-string
       (map (fn [x] (개인여권-string->여권map x)))
       (map (fn [x] (유효하지-않음 x)))
       (filter true?)
       count)
)
