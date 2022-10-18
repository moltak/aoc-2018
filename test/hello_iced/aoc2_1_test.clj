(ns hello-iced.aoc2-1-test
  (:require [clojure.test :refer [deftest testing is run-tests] :as t]
            [hello-iced.aoc2-1 :as sut]))

(deftest public-function-in-namespace-test
  (testing "A description of the test"
    (is (= 2 (+ 1 1)))
    (is (= 10 (* 2 5)))))


(def sample-array ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])
(deftest encode-char-number-test
  (testing "aabb를 a2b2로 변환"
    (is (= (sut/encode-char-number "aabb") {\a 2 \b 2}))
    (is (= (sut/encode-char-number "bababc") {\a 2 \b 3 \c 1}))))

(deftest some-target-number-test
  (testing "key의 값이 2, 3인 것 필터"
    (is (= (sut/some-target-number {\a 1 \b 1} 2) nil))
    (is (= (sut/some-target-number {\a 2 \b 1} 2) true))
    (is (= (sut/some-target-number {\a 2 \b 1} 3) nil))
    (is (= (sut/some-target-number {\a 2 \b 3} 3) true))))

(deftest count-true-test
  (testing "true의 수 반환"
    (is (= (sut/count-true [true true]) 2))
    (is (= (sut/count-true [true false true]) 2))))

;
;
(comment
  (run-tests))

