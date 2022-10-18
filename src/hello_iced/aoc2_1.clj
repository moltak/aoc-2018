(ns hello-iced.aoc2-1
  (:require
   [clojure.string :as str]))


;https://github.com/green-labs/bootcamp-aoc/blob/main/translations/aoc2018/day2.md"
;주어진 각각의 문자열에서, 같은 문자가 두 번 혹은 세 번씩 나타난다면 각각을 한 번씩 센다.
;두 번 나타난 문자가 있는 문자열의 수 * 세 번 나타난 문자가 있는 문자열의 수를 반환하시오.

;abcdef 어떤 문자도 두 번 혹은 세 번 나타나지 않음 -> (두 번 나오는 문자 수: 0, 세 번 나오는 문자 수: 0)
;bababc 2개의 a, 3개의 b -> (두 번 나오는 문자 수: 1, 세 번 나오는 문자 수: 1)
;abbcde 2개의 b -> (두 번 나오는 문자 수: 1, 세 번 나오는 문자 수: 0)
;abcccd 3개의 c -> (두 번 나오는 문자 수: 0, 세 번 나오는 문자 수: 1)
;aabcdd 2개의 a, 2개의 d -> (두 번 나오는 문자 수: 2, 세 번 나오는 문자 수: 0)
;abcdee 2개의 e -> (두 번 나오는 문자 수: 1, 세 번 나오는 문자 수: 0)
;ababab 3개의 a, 3개의 b -> (두 번 나오는 문자 수: 0, 세 번 나오는 문자 수: 2)

; 1. 부호화 -> abbcde -> a1b2c3, a2, b3,
; 2. 2 3 을 filter
;   2 -> 1, 1, 0 -> count -> 2
;   3 -> 1, 0, 1 -> count -> 2
; 3. 곱한다.
;   2 * 2 -> 4

(def input (slurp "resources/aoc2-1.input"))

(def sample-array ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])

(map frequencies sample-array)
(defn encode-char-number
  "aabb를 {a 2 b 2}로 변환"
  [text]
  (->> text
       seq
       sort
       (reduce (fn [mapped c] (if (contains? mapped c) 
                               (assoc mapped c (+ (mapped c) 1)) 
                               (assoc mapped c 1))) 
               {})))

(defn some-target-number
  "map과 target-number를 받아서 target-number 유무 반환"
  [target-number mapped]
  (some #(= target-number %) (vals mapped)))

(partial some-target-number 2)
; refactoring
(defn solve [array]
  (* (->> array 
          (map encode-char-number) 
          (map (partial some-target-number 2))
          (filter true?) 
          count)
     (->> array
          (map encode-char-number)
          (map (partial some-target-number 3))
          (filter true?) 
          count)))

(defn solve2 [array]
  (->> array
       (map encode-char-number)))
;
;
(comment
  (println input)
  (encode-char-number "aabb")
  (some-target-number {\a 2} 2)
  (solve2 sample-array)
  (solve (str/split-lines input))
  (reduce (fn [a v] (if (< a 100) (+ a v) (reduced 1000))) (range 20))

  (str/split "aabb" #"")
  (apply list "aabb")
  (seq "aabb")

;;
)
