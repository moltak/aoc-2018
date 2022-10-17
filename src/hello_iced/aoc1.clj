(ns hello-iced.aoc1
  (:require
   [clojure.string :as str]))

; 문제 https://adventofcode.com/2018/day/1
; 번역 https://kippie408.tistory.com/55

(def input (slurp "resources/aoc1.input"))
; input

; example
; +1, +1, +1의 결과는  3
; +1, +1, -2의 결과는  0
; -1, -2, -3의 결과는 -6
; 0부터 파일 끝까지 다 더하면 되는 문제.
; -1 -2 -3 을 (-3 (- 2 (- 1 0))) 으로 변형 할 수 있으면 되겠는데?
; (solve "-2" (solve "+1" (solve "+1" 0))) ;<- 이렇게 만들면 되겠다!

(defn split-operator-and-operand
  "line 에서 연산자와 피연산자를 구별하여, [연산자 피연산자] 형태의 벡터를 반환한다."
  [line]
  (let [operator (subs line 0 1)
        operand (subs line 1 (.length line))]
    [operator operand]))

(defn calc [operator value acc]
  ((resolve (symbol operator)) acc (Integer/parseInt value)))

(defn solve [data acc]
  (let [[op value] (destruct data)]
    (calc op value acc)))

(defn main [opss]
  (loop [[op & rest-ops :as ops] opss
         acc 0]
    (if (empty? ops)
      acc
      ;(recur (rest ops) (solve (first ops) acc)
      (recur rest-ops (+ op acc)))))

(comment
  (split-operator-and-operand "+100")
  (Integer/parseInt "-100")
  (parse-long "-1000")

  ;(main ["+1" "+1" "+1"])
  ;(main ["+1" "+1" "-2"])
  ;(main ["-1" "-1" "-3"])
  ;(main ["-20" "-2" "-3"])
  (main (map parse-long (str/split input #"\n")))
  (reduce + (map parse-long (str/split-lines input)))


  (->> input
       str/split-lines
       (map parse-long)
       (reduce +))

  (-> "1233"
      parse-long
      (- 1000)))

