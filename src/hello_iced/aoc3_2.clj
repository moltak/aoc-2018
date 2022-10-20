(ns hello-iced.aoc3-1
  (:require [clojure.string :as str]))
(use 'clojure.set)

(comment "
Day 3

https://adventofcode.com/2018/day/3
파트 1

다음과 같은 입력이 주어짐.

#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2

# 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

........
...2222.
...2222.
.11XX22.
.11XX22.
.111133.
.111133.
........

여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4) 
")

(def input3-2 (slurp "resources/aoc3_2.input"))

(defn to-location-code
  "문제에서 정의한 형식의 코드 한 줄을 받아 위치정보로 변환해 리턴합니다."
  [raw-code]
  ;                        id      x     y      width height
  (let [[_ id x y width height] (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" raw-code)]
    {
     :id     (-> id read-string str)
     :x      (-> x read-string str)
     :y      (-> y read-string str)
     :width  (-> width read-string str)
     :height (-> height read-string str)}))

(defn map-coordinate 
  "claim으로 맵 좌표 :id 생성" 
  [& {id :id x :x y :y width :width height :height :as input}]
  (->> 
   (let [xn (parse-long x) yn (parse-long y) widthn (parse-long width) heightn (parse-long height)] 
     (for [x (seq (range xn (+ xn widthn))) 
           y (seq (range yn (+ yn heightn)))]
       ;[(str x y)]
       ;[(select-keys input [:x :y])]
       [{:x x :y y}]))
   flatten
   (map #(hash-map % #{id}))
   (into {})))

(defn join-coordinate
  "겹치는 좌표의 set에 id 추가"
  [coordinates]
  (->> coordinates
       (reduce (fn [acc x] 
                 (merge-with into acc x)
                 #_(update acc (first (keys x)) conj x)
                 ) 
               {})))

(comment 
  #_(->> input3-2
       str/split-lines
       (map to-location-code) 
       (map map-coordinate)
       join-coordinate
       (filter (fn [[k v]] (= (count v) 1)))
       #_(map (fn [[k v]] v))
       #_distinct
       #_(map seq)
       #_flatten
       #_(reduce (fn [acc x] (if (acc x) (update acc x inc) (conj acc {x 1}))) {})
       #_(filter (fn [[k v]] (= v 1)))
       )

  (->> input3-2
       str/split-lines
       (map to-location-code)
       (for [a % b %] [a b]))

)

