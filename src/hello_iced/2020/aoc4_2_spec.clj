(ns hello-iced.2020.aoc4-2-spec
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(def input4-2 (slurp "resources/2020/aoc4_2.input"))

(def valid-input "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(def invalid-input "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")


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
         (map #(hash-map (keyword (first %)) (second %)))
         (apply merge))))

(defn inch|cm->bool
  [x]
  (let [unit (re-find #"[cm|in]+" x)
        hgt-int (parse-long (re-find #"[0-9]+" x))] 
    (case unit 
      "cm" (<= 150 hgt-int 193)
      "in" (<= 59 hgt-int 76)
      false)))

(s/def :passort/string string?)

(s/def :passport/cid (constantly true))
(s/def :passport/byr (s/and string? #(<= 1920 (parse-long %) 2002)))
(s/def :passport/iyr (s/and string? #(<= 2010 (parse-long %) 2020)))
(s/def :passport/eyr (s/and string? #(<= 2020 (parse-long %) 2030)))
(s/def :passport/hgt (s/and string? #(inch|cm->bool %)))
(s/def :passport/hcl (s/and string? #(re-find #"^#[0-9a-f]{6}$" %)))
(s/def :passport/ecl (s/and string? #(#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth" } %)))
(s/def :passport/pid (s/and string? #(re-matches #"^\d{9}$" %)))
(s/def :passport/person (s/keys :req-un 
                                [:passport/byr :passport/iyr :passport/eyr 
                                 :passport/hgt :passport/hcl :passport/ecl 
                                 :passport/pid]))

(defn solve
  [raw-string]
  (let [passports (raw-string->개인여권-string raw-string)]
    (->> passports 
         (map #(개인여권-string->여권map %))
         (filter #(= 7 (count (dissoc % :cid))))
         (filter #(<= 1920 (parse-long (% :byr)) 2002))
         (filter #(<= 2010 (parse-long (% :iyr)) 2020))
         (filter #(<= 2020 (parse-long (% :eyr)) 2030))
         (filter #(not-empty (% :hgt)))
         (filter #(inch|cm->bool (% :hgt)))
         (filter #(re-find #"^#[0-9a-f]{6}$" (% :hcl)))
         (filter #(#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth" } (% :ecl)))
         (filter #(re-find #"^\d{9}$" (% :pid)))
         count)))

(defn solve2
  "filter -> spec으로 변경"
  [raw-string]
  (let [passports (raw-string->개인여권-string raw-string)]
    (->> passports 
         (map #(개인여권-string->여권map %))
         (map #(s/valid? :passport/person %))
         (filter true?)
         count)))

(comment
  (solve2 test-input)
  (solve2 valid-input)
  (solve2 invalid-input)
  (solve2 input4-2)

)
