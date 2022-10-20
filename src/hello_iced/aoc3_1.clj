(ns hello-iced.aoc3-1
  (:require [clojure.string :as str]))

(def input (slurp "resources/aoc3_1.input"))

(defn decode-claim
  "claim을 맵으로 변경 
   input: #1 @ 1,3: 4x4"
  [claim]
  (str/split claim #""))


(defn coordinate
  "x y width height를 이용 2차원 좌표 반환"
  [& {x :x y :y width :width height :height}]
  (for [x (seq (range x (+ x width))) y (seq (range y (+ y height)))]
    (seq [x y])))

(defn count-same-area
  "겹치는 구역 수 반환"
  [area1 area2]
  (count (filter some? (for [a area1 b area2]
                  (if (= a b) a nil)))))

(defn solve
  "{:id :x :y :width :hegith를 실행"
  [vector-map]
  (for [a vector-map b vector-map]
    [a b]))

(comment 
  (solve [{:id "0" :x 1 :y 3 :width 4 :height 4}
          {:id "1" :x 3 :y 1 :width 4 :height 4}
          {:id "2" :x 5 :y 5 :width 2 :height 2}])

  (solve [1 2])

  (def ID1 (coordinate {:x 1 :y 3 :width 4 :height 4}))
  (def ID2 (coordinate {:x 3 :y 1 :width 4 :height 4}))
  (def ID3 (coordinate {:x 5 :y 5 :width 2 :height 2}))

  (count-same-area ID1 ID2)
  (count-same-area ID2 ID3)
  (count-same-area ID1 ID3)
)
