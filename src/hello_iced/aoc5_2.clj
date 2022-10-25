(ns hello-iced.aoc5-2
  (:require [clojure.string :as str]))
(use 'clojure.set)
;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(def input5-2 (slurp "resources/aoc5_2.input"))
(def test-input "dabAcCaCBAcCcaDA")
(defn char->int [x] (int (.charAt x 0)))
(defn 반응
  [x y]
  (= (abs (- (char->int x) (char->int y))) 32))

(comment
  (- (char->int "a") (char->int "A"))
  (- (char->int "b") (char->int "B"))
  (- (char->int "A") (char->int "a"))
  (반응 "a" "A")
  (반응 "A" "a")
  (반응 "A" "b")
  (seq "text"))

(defn solve
  [text]
  (->> text
       seq
       (reduce (fn [acc x] 
                 (if (empty? acc)
                   (conj acc x)
                   (if (반응 (str (last acc)) (str x))
                     (pop acc)
                     (conj acc x))
                   )) 
               [])))

(defn az->AZ
  []
  (apply map (fn [a b] (str a "|" b)) 
         (let [[l u] 
               [(range (char->int "a") (inc (char->int "z"))) 
                (range (char->int "A") (inc (char->int "Z")))]] 
           [(map #(str (char %)) l) (map #(str (char %)) u)])))

(comment
  (->> test-input
       vector
       cycle
       (take (count (az->AZ)))
       (map (fn [a b] (vector a b)) (az->AZ))
       (map (fn [[a b]] (str/replace b (re-pattern a) "")))
       (map solve)
       (map count)
       sort)

  (->> input5-2
       vector
       cycle
       (take (count (az->AZ))) ; input5-2를 a-z까지의 수와 같게 만듬
       (map (fn [a b] (vector a b)) (az->AZ))
       (map (fn [[a b]] (str/replace b (re-pattern a) ""))) ; a|A b|B 알파벳을 없앰
       (map solve)
       (map count)
       (map dec)
       sort)


  ; test input
  (str/join "" (solve test-input))
  (str/join "" (solve (str/replace test-input #"a|A" "")))
  (str/join "" (solve (str/replace test-input #"b|B" "")))
  (str/join "" (solve (str/replace test-input (re-pattern "c|C") "")))
  (str/join "" (solve (str/replace test-input #"d|D" "")))
  )

