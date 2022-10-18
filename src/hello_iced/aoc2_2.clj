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

(defn get-pair
  "두 문자열을 받아 한글자만 다른 경우 pair반환, 아니면 nil 반환"
  [x y] 
  (let [diff-map {:count 0}] 
    (for [i (range (count x))
        :let [a (get x i)
              b (get y i)
              diff (abs (compare a b))]
        :while (< (diff-map :count) 2)] 
      (update diff-map :count + diff)) 
    (if (> (diff-map :count) 0)
      [x y]
      nil)))

(defn solve
  "str1='abc' str2='abb' 일 때  
   [[a a] [b b] [c b]로 변경 후 ab만 출력
  "
  [str1 str2]
  (->> (map vector (seq str1) (seq str2))
       (reduce (fn [acc i] (if (= (get i 0) (get i 1)) 
                             (str acc (get i 0)) ;(println (get i 0)) 
                             acc) ""))))

(defn print-pair 
  "두 문자열에서 같은 부분만 반환"
  [x y]
  (str/join 
    "" 
    (for [i (range (count x))
      :let [a (get x i)
            b (get y i)]
      :when (= (compare a b) 0)]
    (get x i))))


;
(comment
  (println (seq "abc") (seq "bbb"))
  (get "abc" 1)
  (print-pair "abc" "abb")
  (print-pair "fghij" "fguij")

  (str "a" "b")
)

