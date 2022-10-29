(ns hello-iced.2020.aoc8-1-test
  (:require [clojure.test :refer [deftest testing is run-tests] :as t]
            [hello-iced.2020.aoc8-1 :as sut]))

(deftest 명령어->레지스터-저장-test
  (testing "default, acc 함수 검증"
    (is (= {:PC 1 :IR "acc" :DR 1 :TR 1 :RAW "acc +1 1"} 
           (sut/명령어->레지스터-저장 {:PC 1} "acc +1")))))


(def TEST-INPUT-1 ["jmp +2" "nop +1" "nop +1"])

(deftest execute-test
  (testing "execute test"
    (is (= {:PC 2 :IR "jmp" :TR 0 :DR 0 :RAW "jmp +2 0" :inst-history ["jmp +2 0"]}
           (sut/execute 
            (sut/명령어->레지스터-저장 
             {:PC 0} 
             "jmp +2"))))
    (is (= {:PC 90 :IR "jmp" :TR 0 :DR 0 :RAW "jmp -10 100" :inst-history ["jmp -10 100"]}
           (sut/execute 
            (sut/명령어->레지스터-저장 
             {:PC 100} 
             "jmp -10"))))
    (is (= {:PC 2 :IR "nop" :TR 0 :DR 0 :RAW "nop +2 1" :inst-history ["nop +2 1"]}
           (sut/execute 
            (sut/명령어->레지스터-저장
             {:PC 1}
             "nop +2"))))
    (is (= {:PC 6 :IR "acc" :TR 0 :DR 0 :AC 102 :RAW "acc +2 5" :inst-history ["acc +2 5"]}
           (sut/execute 
            (sut/명령어->레지스터-저장
             {:PC 5 :AC 100}
             "acc +2"))))
    ))
