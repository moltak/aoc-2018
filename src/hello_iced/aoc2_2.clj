(ns hello-iced.aoc2-2
  (:require
   [clojure.string :as str]))

;여러 개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
; "abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"
; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.

; 1. 두 문자열을 받아 한글자만 다른 경우 정보 반환 pair 반환, 아니면 nil 반환 
; 2. 두 문자열 출력. 다른 문자는 출력하지 않음.

(def input (str/split-lines (slurp "resources/aoc2-2.input")))

(def sample-array ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])

(defn duplication
  "array의 1, 2번째 문자열에서 같은 부분만 반환"
  [array]
  (reduce 
   (fn [acc i] (if (= (get i 0) (get i 1))
                 (str acc (get i 0))
                 acc))
   ""
   (map vector (get array 0) (get array 1))))

(defn print-answer 
  "정답(한글자만 다른경우) 일 때 정답 반환, 아니면 nil 반환"
  [array]
  (let [res (duplication array)] 
    (if 
     (= (dec (count (get array 1))) (count res)) res nil)))


(defn solve 
  "for 문을 이용해 모든 경우의 수를 탐색"
  [array]
  (filter some? (for [a1 array a2 array]
    (print-answer [a1 a2]))))

;
(comment
  (solve sample-array)
  (solve input)

  ; reduce로 어떻게 바꿀 수 있을까?
  (reduce (fn [acc i] (println i)) [] [["a" "b" "c"] ["a" "b" "c"]])
)

