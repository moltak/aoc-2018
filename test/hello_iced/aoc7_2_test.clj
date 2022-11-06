(ns hello-iced.aoc7-2-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [hello-iced.aoc7-2 :as sut]))

(def worker1 (merge nil sut/WORKER))

(deftest worker-test
  ; A를 할당했을 때
  (testing (is (= 61 (:remain (sut/occupy-work worker1 "A")))))
  (testing (is (= "WORKING" (:status (sut/occupy-work worker1 "A")))))
  ; 일이 없을 때
  (testing (is (= 0 (:remain (sut/working! worker1)))))
  (testing (is (= "IDLE" (:status (sut/working! worker1)))))

  ; working 이 remain 감소시키는지 확인
  (testing (is (= 60 (:remain (sut/working! (sut/occupy-work worker1 "A")))))))

(def supervior1 (merge nil sut/supervior))
(def next-work (:next-work supervior1))
(def idle-worker (:idle-worker supervior1))
(def resolve-work (:resolve-work supervior1))

(def worker2 (sut/occupy-work (merge nil sut/WORKER) "A"))
(def worker3 (sut/occupy-work (merge nil sut/WORKER) "B"))

(deftest supervior-test
  (testing (is (= ["A" "B"] (map #(:work %) [worker2 worker3])))))
