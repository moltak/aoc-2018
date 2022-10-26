(ns hello-iced.2020.aoc4-2
  (:require [clojure.string :as str] 
            [clojure.spec.alpha :as s]))

(comment 
  "파트 2

파트1에서는 필드의 유무만을 검사했다면, 파트2에서는 구체적인 범위가 주어진다.

    byr (Birth Year) - 4 자리 숫자; 최소 1920 & 최대 2002.
    iyr (Issue Year) - 4 자리 숫자; 최소 2010 & 최대 2020.
    eyr (Expiration Year) - 4 자리 숫자; 최소 2020 & 최대 2030.
    hgt (Height) - 마지막에 cm 혹은 in이 오는 숫자:
    cm의 경우, 숫자는 최소 150 & 최대 193.
    in의 경우, 숫자는 최소 59 & 최대 76.
    hcl (Hair Color) - #뒤에 오는 정확히 6개의 캐릭터 0-9 혹은 a-f.
    ecl (Eye Color) - 정확히 amb blu brn gry grn hzl oth 중 하나.
    pid (Passport ID) - 처음 0을 포함하는 9자리 숫자.
    cid (Country ID) - 없어도 됨.

아래는 예시들이다.

byr valid:   2002
byr invalid: 2003

hgt valid:   60in
hgt valid:   190cm
hgt invalid: 190in
hgt invalid: 190

hcl valid:   #123abc
hcl invalid: #123abz
hcl invalid: 123abc

ecl valid:   brn
ecl invalid: wat

pid valid:   000000001
pid invalid: 0123456789

모든 필드의 기준에 맞는 여권의 수를 반환하여라.")


(def input4-2 (slurp "resources/2020/aoc4_2.input"))

(def valid-input "ecl:gry pid:060033327 eyr:2020 hcl:#ffffff byr:1920 iyr:2010 cid:147 hgt:150cm

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:060753108 byr:1931
hgt:179cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929 hgt:59in")

(def invalid-input "ecl:gry pid:160033327 eyr:2020 hcl:#ffffff byr:1920 iyr:2010 cid:147 hgt:150cm

hcl:#ae17z1 iyr:2013
eyr:2024
ecl:brn pid:060753108 byr:1931
hgt:179cm cid:1000

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929 hgt:100in")


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
  (let [splitted (str/split x #" ")]
    (->> splitted
         (map #(str/split % #":"))
         (apply conj {}))))
(defn print-interm [x]
  (println x) x)

(defn inch|cm->bool
  [x]
  (let [unit (re-find #"[cm|in]+" (x "hgt"))
        hgt-int (parse-long (re-find #"[0-9]+" (x "hgt")))] 
    (case unit 
      "cm" (<= 150 hgt-int 193)
      "in" (<= 59 hgt-int 76)
      false)))


(defn solve
  [raw-string]
  (let [passports (raw-string->개인여권-string raw-string)]
    (->> passports 
         (map #(개인여권-string->여권map %))
         (filter #(= 7 (count (dissoc % "cid"))))
         (filter #(<= 1920 (parse-long (% "byr")) 2002))
         (filter #(<= 2010 (parse-long (% "iyr")) 2020))
         (filter #(<= 2020 (parse-long (% "eyr")) 2030))
         (filter #(inch|cm->bool %))
         (filter #(re-find #"^#[0-9a-f]{6}$" (% "hcl")))
         (filter #(#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth" } (% "ecl")))
         (filter #(re-find #"^\d{9}$" (% "pid"))))))

(comment
  (solve test-input)
  (solve valid-input)
  (solve invalid-input)
  (solve input4-2)
)

(comment
  (defn my-< [a b]
    (println "(my-<" a b ") returns " (< a b))
    (< a b))
  (type my-<)

  (defn comparr-> [] (< 1 2))
  (type comparr->)

  (defn test-function1 [] (str ""))
  (type test-function1)

  (. my-< (compare 1 2))
  (. my-< (compare 2 1))
  )
