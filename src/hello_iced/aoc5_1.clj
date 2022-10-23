(ns hello-iced.aoc5-1
  (:require [clojure.string :as str]
            [clojure.test :as test]))
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
(defn to-int [x] (int (.charAt x 0)))
(defn 반응
  [x y]
  (= (abs (- (to-int x) (to-int y))) 32))

(comment
  (- (to-int "a") (to-int "A"))
  (- (to-int "b") (to-int "B"))
  (- (to-int "A") (to-int "a"))
  (반응 "a" "A")
  (반응 "A" "a")
  (반응 "A" "b")
  (seq "text")
  )

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

(comment
  (str/join "" (solve "dabAcCaCBAcCcaDA"))
  )
