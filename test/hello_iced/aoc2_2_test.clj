(ns hello-iced.aoc2-2-test
  (:require [clojure.test :refer [deftest testing is run-tests] :as t]
            [hello-iced.aoc2-2 :as sut]))

(deftest get-pair-test
  (testing "두 문자열을 받아 한글자만 다른 경우 pair반환, 아니면 nil 반환"
    (is (= ["abc" "bbc"] (sut/get-pair "abc" "bbc"))) ;
    (is (= ["aaab" "baab"] (sut/get-pair "aaab" "baab"))) ;
    (is (= nil (sut/get-pair "abc" "aba"))) ;
  ))

(deftest duplication-test
  (testing "두 문자열을 받아 같은 부분만 반환"
    (is (= "ab" (sut/duplication ["abc" "abb"])))
    (is (= "cc" (sut/duplication ["abcc" "AAcc"])))
    (is (= "fgij" (sut/duplication ["fghij" "fguij"])))
))

