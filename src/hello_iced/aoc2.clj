(ns hello-iced.aoc2
 (:require
   [clojure.string :as str]))


; https://github.com/green-labs/bootcamp-aoc/blob/main/translations/aoc2018/day1.md
; 주어진 입력의 숫자를 더할 때마다 나오는 숫자 중, 처음으로 두 번 나오는 숫자를 리턴하시오.
; +3, +3, +4, -2, -4
; 위 예에서는 10이 처음으로 두 번 나오는 숫자임. 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

(def array [3, 3, 4, -1, -4])

(def input-file (slurp "resources/aoc1-2.input"))


; 1. java object 사용
; 2. loop, recur 사r

(defn solve [array]
  (loop [res-set #{} sum 0 xs array]
    ;(println res-set " -> " (first xs))
    (if (contains? res-set sum) 
      sum
      (recur
        (conj res-set sum)
        (+ sum (first xs))
        (rest xs)
      )
    )
  )
)

(comment 
  (->> input-file 
       str/split-lines
       (map parse-long) 
       cycle
       solve
  )
  
  (solve (take 20 (cycle array))) ; 10
  (solve (take 20 (cycle [+1 -1]))) ; 10
  (solve (take 20 (cycle [+7, +7, -2, -7, -4]))) ; 10
  (solve (take 20 (cycle [-6, +3, +8, +5, -6]))) ; 10
  (solve (take 20 (cycle array))) ; 10
  (solve array)
  (conj #{1 2 3} 3)
  (#{1 2 3} 4)
  (contains? #{1 2 3} 3)
)
