(ns hello-iced.aoc3-1-test
  (:require [clojure.test :refer [deftest testing is run-tests] :as t]
            [hello-iced.aoc3-1 :as sut]))

(deftest to-location-code-test
  (testing "input을 변경"
    (is (= {:id "1" :x "1" :y "3" :width "4" :height "4"} (sut/to-location-code "#1 @ 1,3: 4x4")))
    (is (= {:id "2" :x "3" :y "1" :width "4" :height "4"} (sut/to-location-code "#2 @ 3,1: 4x4")))
    (is (= {:id "3" :x "5" :y "5" :width "2" :height "2"} (sut/to-location-code "#3 @ 5,5: 2x2"))))) 

(deftest map-coordinate-test
  (testing "claim으로 맵 좌표 :id 생성" 
    (is (= {"00" #{"1"} "01" #{"1"} "10" #{"1"} "11" #{"1"}}
           (sut/map-coordinate {:id "1" :x "0" :y "0" :width "1" :height "1"}) ))))

(deftest join-coordinate-test
  (testing "겹치는 좌표의 set에 item 추가"
    #_(is (= {"00" #{"1" "2"}} (sut/join-coordinate [{"00" #{"1"}} {"00" #{"2"}} ])))
    (is (= {"00" #{"1" "2"}} (sut/join-coordinate [{"00" #{"1"}} {"00" #{"2"}} ])))
    (is (= {"0" #{"0" "1" "2"} "1" #{"1"}}
           (sut/join-coordinate [{"0" #{"0"}} {"0" #{"1"}} {"0" #{"2"}} {"1" #{"1"}}])))
    ))

(deftest overlap-area-test
  (testing "겹친 지역 반환"
    (is (= [{"00" #{"1" "2"}}] (sut/overlap-area 2 [{"00" #{"1" "2"}}])))
    (is (= [] (sut/overlap-area 2 [{"00" #{"1"}}])))
    (is (= [{"00" #{"1" "2"}}
            {"00" #{"1" "2"}}
            {"00" #{"1" "2"}}
            {"00" #{"1" "2"}}] 
           (sut/overlap-area 2 [
                              {"00" #{"1" "2"}}
                              {"00" #{"1" "2"}}
                              {"00" #{"1" "2"}}
                              {"00" #{"1" "2"}}
                              ])))
    ))

(deftest solve3-2-test
  (testing "겹치는 타일이 없는 id 반환"
    (is (= #{"4"}
           (sut/solve3-2 [{"00" #{"1" "2" "3"}}
                      {"00" #{"1" "2"}}
                      {"00" #{"1" "2"}}
                      {"00" #{"2" "3"}}
                      {"00" #{"4"}}
                      {"00" #{"4"}}
                      {"00" #{"4"}}])))))
