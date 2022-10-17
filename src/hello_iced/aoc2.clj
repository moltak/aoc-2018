(ns hello-iced.aoc2)

; https://github.com/green-labs/bootcamp-aoc/blob/main/translations/aoc2018/day1.md
; 주어진 입력의 숫자를 더할 때마다 나오는 숫자 중, 처음으로 두 번 나오는 숫자를 리턴하시오.
; +3, +3, +4, -2, -4
; 위 예에서는 10이 처음으로 두 번 나오는 숫자임. 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

(def array [3, 3, 4, -1, -4])

; 1. java object 사용
; 2. loop, recur 사용

(defn solve [array]
  (loop [res-set #{} sum 0 item (first array)]
    (println res-set " -> " item)
    (if (contains? res-set sum) 
      sum
      (recur
        (conj res-set #{sum})
        (+ sum item)
        (first (rest array))
      )
    )
  )
)

(comment 
  ;(solve (take 20 (cycle array))) ; 10
  (solve array)
  (conj #{1 2 3} 3)
  (#{1 2 3} 4)
  (contains? #{1 2 3} 3)
)
