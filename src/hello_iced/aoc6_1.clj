(ns hello-iced.aoc6-1
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

(comment
  "https://adventofcode.com/2018/day/6
파트 1
입력 : 좌표의 쌍이 N개 주어짐
(1, 1) (1, 6) (8, 3) (3, 4) (5, 5) (8, 9)
각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.")

(def input (slurp "resources/aoc6_1.input"))

(def test-input [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]])

(defn in? 
  "true if coll contains elm"
  [coll elm]  
  (some #(= elm %) coll))

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

(defn 경계에-닿았다?
  [x y map-range]
  (or (= x (first (map-range :x-range))) 
      (= x (last (map-range :x-range)))
      (= y (first (map-range :y-range)))
      (= y (last (map-range :y-range)))))

(defn 맨하탄거리 [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn 입력->맨하탄거리
  [coords 
   {x-range :x-range y-range :y-range}]
  (for [w x-range h y-range] 
    (->> coords 
         (map-indexed (fn [i [x y]] [(맨하탄거리 [x y] [w h]) i w h]))
         sort)))


(defn 타일-수-세기
  [map-range manhatans]
  (->> manhatans
       (reduce (fn [acc x] (let [pv1 (first (first x))
                                 pv2 (first (second x)) 
                                 pk1 (second (first x))
                                 w (nth (first x) 2)
                                 h (nth (first x) 3)]
                             (if (not= pv1 pv2)
                               (if (contains? acc pk1) 
                                 (into acc {pk1 [(inc (first (acc pk1))) 
                                                 (conj (second (acc pk1)) (경계에-닿았다? w h map-range))]})
                                 (assoc acc pk1 [1 #{}] ))
                               acc))) {})))

(defn solve
  [coords]
  (let [map-size (지도-사이즈 coords)
        map-range (지도-경계 map-size)
        manhatan-map (입력->맨하탄거리 coords map-range)]
    (->> manhatan-map
         (타일-수-세기 map-range)
         (map second)
         (sort-by first >)
         (filter #(= 1 (count (second %))))
    )))

(comment
  (into {:pk1 [0 false]} {:pk1 [(inc 0) true]})
  (def test-map-range (지도-경계 (지도-사이즈 test-input)))
  (경계에-닿았다? 1 100 test-map-range)
  (def abc (입력->맨하탄거리 test-input test-map-range))

  (solve test-input)

  (def inputted 
    (->> (str/split-lines input)
         (map (fn [x] (str/split x #", ")))
         (map (fn [x] (vec [(parse-long (first x)) (parse-long (second x))])))))

  (->> (str/split-lines input)
       (map (fn [x] (str/split x #", ")))
       (map (fn [x] (vec [(parse-long (first x)) (parse-long (second x))])))
       solve)
  )


