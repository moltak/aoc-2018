(ns hello-iced.aoc3-1-test
  (:require [clojure.test :refer [deftest testing is run-tests] :as t]
            [hello-iced.aoc3-1 :as sut]))

(deftest decode-claim-test
  (testing "claim을 map으로 변경"
    (is (= {:id "#1" :x 1 :y 3 :width 4 :height 4} 
           (sut/decode-claim "#1 @ 1,3: 4x4"))))
  (let)
  )
