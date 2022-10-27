(ns hello-iced.aoc6-1-test
  (:require [clojure.test :refer [deftest testing is run-tests] :as t]
            [hello-iced.aoc6-1 :as sut]))

((deftest 좌표쌍+전체맵->맨하탄거리-test
   (testing "좌표쌍+전체맵->맨하탄거리-test "
     (is (= 
          {"00" [#{"A"} 2] "01" [#{"A"} 1] "10" [#{"A"} 1] "11" [#{"A"} 0]}
          (sut/좌표쌍+전체맵->맨하탄거리 "A" [1 1] [2 2]))))
   ))

(deftest 좌표+test
  (testing "좌표를 더함"
    (is (= [#{"A"} 1] (sut/좌표+ [#{"A"} 1] [#{"B"} 3])))
    (is (= [#{"A" "B"} 1] (sut/좌표+ [#{"A"} 1] [#{"B"} 1])))
    ))

(deftest 좌표계+test
  (testing "좌표계를 더함"
    (is (= {"00" [#{"A"} 0] 
            "01" [#{"A" "B"} 1]
            "10" [#{"A" "B"} 1]
            "11" [#{"B"} 0]}
           (sut/좌표계+ 
            [(sut/좌표쌍+전체맵->맨하탄거리 "A" [0 0] [2 2]) 
             (sut/좌표쌍+전체맵->맨하탄거리 "B" [1 1] [2 2])])))))
