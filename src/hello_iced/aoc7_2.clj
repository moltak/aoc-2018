(ns hello-iced.aoc7-2
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(comment
"Part2
파트 1에서는 일을 워커(worker)가 하나였지만, 파트 2는 5명. 즉, 동시에 5개의 일을 처리할 수 있게 됨.
그리고 각각의 일 (A~Z)은 처리하는데 (60+1~60+26)의 시간이 걸림. B는 62초, D는 64초, etc.

이 때, 주어진 모든 일을 처리하는데 걸리는 시간을 구하시오.

예)

아래는 파트 1의 예시에서 워커가 2명이 된 케이스이다.
Second   Worker 1   Worker 2   Done
   0        C          .        
   1        C          .        
   2        C          .        
   3        A          F       C
   4        B          F       CA
   5        B          F       CA
   6        D          F       CAB
   7        D          F       CAB
   8        D          F       CAB
   9        D          .       CABF
  10        E          .       CABFD
  11        E          .       CABFD
  12        E          .       CABFD
  13        E          .       CABFD
  14        E          .       CABFD
  15        .          .       CABFDE
15초가 걸리므로 답은 15")

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

  ; TODO: 하하하

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
