(ns hello-iced.aoc6-2
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

(comment
  "https://adventofcode.com/2018/day/6
파트 1
입력 : 좌표의 쌍이 N개 주어짐
(1, 1) (1, 6) (8, 3) (3, 4) (5, 5) (8, 9)
각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.")

(def input (slurp "resources/aoc6_2.input"))

(def test-input [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]])

(defn print-interm
  [x]
  (println x) x)

(defn 지도-사이즈
  [coords]
  (let [xs (reduce #(conj %1 (first %2)) [] coords)
        ys (reduce #(conj %1 (second %2)) [] coords)]
    {:min {:x (apply min xs) :y (apply min ys)} 
     :max {:x (apply max xs) :y (apply max ys)}}))

(defn 지도-경계
  [map-min-max] 
  (let [x-range (range ((map-min-max :min) :x) (inc ((map-min-max :max) :x))) 
        y-range (range ((map-min-max :min) :y) (inc ((map-min-max :max) :y)))]
    {:x-range x-range :y-range y-range}))

(defn 맨하탄거리 [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn 입력->맨하탄거리-누적
  [distance coords]
  (let [map-size (지도-사이즈 coords)
        map-range (지도-경계 map-size)]
    (for [w (map-range :x-range) 
          h (map-range :y-range)] 
      (->> coords 
           (reduce (fn [acc [x y]] (+ acc (맨하탄거리 [x y] [w h]))) 0)
           (> distance)
           ))))

(comment
  (count (filter true? ((partial 입력->맨하탄거리-누적 32) test-input)))

  (count 
   (filter true? 
           (->> (str/split-lines input)
                (map (fn [x] (str/split x #", ")))
                (map (fn [x] (vec [(parse-long (first x)) (parse-long (second x))])))
                ((partial 입력->맨하탄거리-누적 10000)))))
  )


