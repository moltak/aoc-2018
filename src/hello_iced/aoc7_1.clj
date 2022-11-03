(ns hello-iced.aoc7-1
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

(defn raw->works
  [text] 
  (->> text
       #_(print-interm)
       (map #(let [matcher (re-matcher #"[A-Z] " %)] 
               [(str/trim (re-find matcher)) 
                (str/trim (re-find matcher))]))))

(comment
  "1. 의존성 배열
    [A [C]] [F [C]] [B [A]] [D [A]] [E [B D F]] 
  
   2. 루트 작업 찾기
     답: C
     의존성 배열 안에서 모든 작업(ABCDEF) 중 [0]에 없는 작업이 루트임

   3. C를 출력 후 모든 배열에서 C를 삭제한다. 
    [A []] [F []] [B [A]] [D [A]] [E [B D F]] 로 변환

   4. 2번째 배열이 비어있는 작업 중 알파벳 순으로 출력 
     답: A
  
   5. A를 출력 후 모든 배열에서 A를 삭제
    [F []] [B []] [D []] [E [B D F]]

   6. 2번째 배열이 비어있는 작업 중 알파벳 순으로 출력 
     답: B

   7. B를 출력 후 모든 배열에서 B를 삭제
    [F []] [D []] [E [D F]]

   8. 4-5를 반복
  "
  )

(defn intersection-works
  "모든 업무를 set으로 변환. 중복제거하기 위해"
  [works]
  (->> works
       (reduce (fn [acc [a b]] (conj acc a b)) #{})
       vec
       sort))

(defn dependency-works
  "1. 의존성 배열
  [A [C]] [F [C]] [B [A]] [D [A]] [E [B D F]]"
  [works]
  (let [의존성-뒤집기 (map (fn [x] [(second x) #{(first x)}]) 
                     works)
        맵으로-변환 (map (fn [x] (into {} [x])) 
                    의존성-뒤집기)  
        맵-합성 (apply merge-with into {} 맵으로-변환)
        ] (vec 맵-합성)))

(defn generate-root-works
  " 2. 루트 작업 찾기
     답: C
     의존성 배열 안에서 모든 작업(ABCDEF) 중 [0]에 없는 작업이 루트임"
  [works]
  (let [의존성을-갖는-works (set (map #(first %) (dependency-works works)))
        중복제거된-모든-works (set (intersection-works works))]
    (map (fn [x] (vec [x #{}])) (set/difference 중복제거된-모든-works 의존성을-갖는-works))))

(defn remove-work-from-parent
  " 3. C를 출력 후 모든 배열에서 C를 삭제한다. 
    [A []] [F []] [B [A]] [D [A]] [E [B D F]] 로 변환"
  [work dependency-works]
  (map (fn [x] [(first x) (disj (second x) work)]) dependency-works))

(defn remove-work-from-child
  [work dependency-works]
  (filter #(not= (first %) work) dependency-works))

(defn next-work
  "4. 2번째 배열이 비어있는 작업 중 알파벳 순으로 출력 답: A "
  [dependency-works]
  (loop [counts 0]
    (let [filtered (filter (fn [x] (= counts (count (second x)))) dependency-works)]
      (if (= 0 (count filtered))
        (recur (inc counts))
        (->> filtered
             sort
             first
             first)))))

(defn solve
  [input]
  (let [works (concat (dependency-works (raw->works input))
                      (generate-root-works (raw->works input)))]
    (println "works: " works)

    (loop [완료작업 (next-work works) 
           완료된작업들 []
           works (remove-work-from-child 완료작업 works)]
      ;(println " ")
      (println "출력-> " 완료작업)
      (let [부모가-삭제된-작업들 (remove-work-from-parent 완료작업 works)
            다음작업 (next-work 부모가-삭제된-작업들)]
        ;(println "부모가-삭제된-작업들" 부모가-삭제된-작업들)
        ;(println "다음작업:" 다음작업)
        ;(println "works:" works)
        (if (= 0 (count works))
          (str 완료된작업들 완료작업)
          (recur 다음작업 
                 (conj 완료된작업들 완료작업) 
                 (remove-work-from-child 다음작업 부모가-삭제된-작업들)))))))

(comment 
  (intersection-works (raw->works input))
  (dependency-works (raw->works input))
  (generate-root-works (raw->works input))
  (next-work
   (concat (dependency-works (raw->works input))
           (generate-root-works (raw->works input))))
  (remove-work-from-child "C" 
                          (concat 
                           (generate-root-works (raw->works test-input))
                           (dependency-works (raw->works test-input))))

  (solve test-input)
  (solve input))

#_(map 
  (fn [x]
   (if (some? x)
     (println "x:" x)
     (println "x is nil"))

   (let [result (into {} [x])]
     (println "result: " result)
     result)))
