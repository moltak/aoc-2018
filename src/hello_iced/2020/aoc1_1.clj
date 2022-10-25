(ns hello-iced.2020.aoc1-1
  (:require
   [clojure.string :as str]))

; 문제 https://adventofcode.com/2018/day/1

; 번역 https://kippie408.tistory.com/55

(comment " Day 1
https://adventofcode.com/2020/day/1

파트 1
더해서 2020이 되는 두 숫자의 곱을 구하시오. (두 숫자는 유일하다고 가정)
예) 1721 979 366 299 675 1456 의 경우 1721 * 299 = 514579 를 출력

파트 2
같은 입력이 주어질 때, 더해서 2020이 되는 세 숫자의 합을 구하시오.
예) 2020 = 979 + 366 + 675, 곱하면 241861950 을 출력 ")

(def input (map (fn [x] (parse-long x)) (str/split-lines (slurp "resources/2020/aoc1_1.input"))))

(def test-input (map (fn [x] (parse-long x)) ["1721" "979" "366" "299" "675" "1456"]))

(comment 
  (->> (for [a test-input b test-input] [a b])
       (map (fn [[x y]] (vector (+ x y) x y)))
       (filter (fn [[sum]] (= 2020 sum)))
       (map (fn [[_ x y]] (* x y)))
       first)

  (->> (for [a input b input] [a b])
       (map (fn [[x y]] (vector (+ x y) x y)))
       (filter (fn [[sum]] (= 2020 sum)))
       (map (fn [[_ x y]] (* x y)))
       first))
