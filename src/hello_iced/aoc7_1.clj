(ns hello-iced.aoc7-1
  (:require [clojure.string :as str]))

(comment
"Day 7

https://adventofcode.com/2018/day/7
파트 1

스케줄이 주어질 때, 일이 처리되는 순서를 반환하시오. 알파벳 캐릭터 하나로 대표되는 일(work)이 주어지고, 각 일을 처리하기 위해서 선행되어야 하는 일들이 스케줄 형식으로 주어짐.

Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.

위와 같은 입력의 경우 아래 형태의 그래프가 만들어짐.

  -->A--->B--
 /    \\      \\
C      -->D----->E
 \\           /
  ---->F-----

순서는 아래와 같음.

    처음엔 C만 가능함. C에서 시작. 만약 다른 시작점이 존재한다면 알파벳 순서로 진행.
    C 다음으로 A와 F가 가능한데, A가 알파벳 우선순위가 높으므로 A로 감.
    A 를 완료하면 B, D, F가 가능한데, 역시 알파벳 우선순위가 높은 B가 수행됨.
    남은 D와 F중에서 D가 수행됨
    F가 수행됨
    E가 수행됨 (E는 B, D, F 모두가 선행되어야 수행될 수 있음)

결과: CABDFE")

(def input (str/split-lines (slurp "resources/aoc7_1.input")))

(def test-input (str/split-lines "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin."))

(defn print-interm
  [x]
  (println x) x)

(defn 노드추출
  [text] 
  (->> text
       #_(print-interm)
       (map #(let [matcher (re-matcher #"[A-Z] " %)] 
               [(str/trim (re-find matcher)) (str/trim (re-find matcher))]))))

(comment 
  (노드추출 input)
  (노드추출 test-input)
  )
