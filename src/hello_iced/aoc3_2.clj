(ns hello-iced.aoc3-2
  (:require [clojure.string :as str]))
(use 'clojure.set)

(comment "
Day 3

https://adventofcode.com/2018/day/3
파트 2

입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않으므로 3을 출력.
겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
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
  (def 전체-id 
    (->> input3-2
         str/split-lines
         (map to-location-code)
         (map (fn [x] (select-keys x [:id])))
         (map vals)
         (map first)
         set))

  (def 겹치는좌표-id
    (->> input3-2
         str/split-lines
         (map to-location-code)
         (map map-coordinate) ; id의 2차원 맵을 생성
         join-coordinate ; 2차원 맵을 좌표로 group-by
         ; 좌표에 아이디가 여러개 있음. 그 아이디를 맵에서 전부 삭제함
         (filter (fn [[k v]] (> (count v) 1)))
         vals
         (apply union)))
 
  (difference 전체-id 겹치는좌표-id) ; 297

  (def cor1 [[{:x 0 :y 0 } #{"A"}]])

  (let [[x y] ["A" "B"]] (str x " " y))

  (apply union [#{1 2} #{2 3}]) ; 이렇게 나오게 하고 싶다. [1 2 3]

  (difference #{1 2 3} #{3 4 5})

  (seq #{1 2 3})
  )
