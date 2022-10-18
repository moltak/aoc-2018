(ns hello-iced.aoc2-2-test
  (:require [clojure.test :refer [deftest testing is run-tests] :as t]
            [hello-iced.aoc2-2 :as sut]))

(deftest get-pair-test
  (testing "두 문자열을 받아 한글자만 다른 경우 pair반환, 아니면 nil 반환"
    (is (= ["abc" "bbc"] (sut/get-pair "abc" "bbc"))) ;
    (is (= ["aaab" "baab"] (sut/get-pair "aaab" "baab"))) ;
    (is (= nil (sut/get-pair "abc" "aba"))) ;
  ))

(deftest print-pair-test
  (testing "두 문자열을 받아 같은 부분만 반환"
    (is (= "ab" (sut/print-pair "abc" "abb")))
    (is (= "cc" (sut/print-pair "abcc" "AAcc")))
    (is (= "fgij" (sut/print-pair "fghij" "fguij")))
))

(deftest solve-test
  (testing "두 문자열을 받아 같은 부분만 반환"
    (is (= "ab" (sut/solve "abc" "abb")))
))
