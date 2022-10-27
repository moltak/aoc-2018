(ns hello-iced.aoc6-1
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.walk :refer [postwalk]]))

;1.
(comment
  "https://adventofcode.com/2018/day/6
파트 1
입력 : 좌표의 쌍이 N개 주어짐
(1, 1) (1, 6) (8, 3) (3, 4) (5, 5) (8, 9)
각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.")

(def input (slurp "resources/aoc6_1.input"))

(s/def ::maps (s/coll-of (s/keys :req [::name ::coordinates])))
(s/def ::name string?)
(s/def ::coordinates vector?)

(def test-input [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]])

(defn print-interm
  [x]
  (println x) x)

(defn 지도-사이즈
  [coords]
  (let [xs (reduce (fn [acc x] (conj acc (first x))) [] coords)
        ys (reduce (fn [acc x] (conj acc (second x))) [] coords)]
    [(apply max xs) (apply max ys)]))


(defn 맨하탄거리
  [[ax ay] [bx by]]
  (+ (abs (- ax bx)) (abs (- ay by))))

(defn 좌표쌍+전체맵->맨하탄거리
  [c-name [ax ay] [w h]]
  (->> (for [x (range 0 w) y (range 0 h)] [x y])
       (reduce (fn [acc [x y]]
                 (assoc acc (str x y) (vector #{c-name} (맨하탄거리 [ax ay] [x y]))))
               {})))

(defn 좌표+
  [[key1 val1] [key2 val2]]
  (cond
    (= val1 val2) [(set/union key1 key2) val1]
    (< val1 val2) [key1      val1]
    (> val1 val2) [key2      val2]))

(defn 좌표계+
  [coords]
  {:pre [(s/valid? ::coordinates coords)]}
  (->> coords
       (reduce (fn [acc i] (into acc i)) [])
       (reduce (fn [acc i]
                 (let [keyy (first i) vall (second i)]
                   (if (acc keyy)
                     (conj acc [keyy (좌표+ vall (acc keyy))])
                     (conj acc [keyy vall]))))
               {})))

(defn 외곽찾기
  "좌표에 x=0 | y=0 | x=w | y=h 일 경우 해당 idx 모두 삭제"
  [w h coords]
  (->> coords
       (filter (fn [i]
                 (let [x (parse-long (str (first (key i))))
                       y (parse-long (str (second (key i))))]
                   (or (= x 0) (= x (inc w))
                       (= y 0) (= y (inc h))))))))

(defn solve
  [[w h] coords]
  (->> coords
       (map-indexed (fn [idx x] (좌표쌍+전체맵->맨하탄거리 (str idx) x [w h]))) ;좌표 이름을 idx로 사용
       vec ; 모든 입력을 벡터로 변경
       좌표계+ ; 좌표를 모두 더해서 {"xy" [#{1 2} 2]}로 변경. xy 좌표에 1 2번이 2맨하탄 거리에 있다는 의미
       #_(외곽찾기 w h)
       (map #(first (second %))) ; #{1 2} 만 가져옴
       (filter #(= (count %) 1)) ; 거리가 겹치지 않은것만 가져옴
       flatten
       frequencies ; 겹치지 않은 가장 큰 수
       sort))

(comment
  (def test-input1 [[1 1]])
  (지도-사이즈 test-input)
  (solve (지도-사이즈 test-input) test-input)

  (def A (좌표쌍+전체맵->맨하탄거리 "A" [1 1] [10 10]))
  (def B (좌표쌍+전체맵->맨하탄거리 "B" [1 6] [10 10]))

  (->> (str/split-lines input)
       (map (fn [x] (str/split x #", ")))
       (map (fn [x] (vec [(parse-long (first x)) (parse-long (second x))])))
       (solve #(지도-사이즈 %))))

