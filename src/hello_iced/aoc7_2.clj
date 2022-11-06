(ns hello-iced.aoc7-2
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [hello-iced.util :as util]
            [hello-iced.aoc7-1 :as aoc7-1]))

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

(def input (str/split-lines (slurp "resources/aoc7_2.input")))

(def WORKER {:index 0
             :work nil
             :final-work nil
             :remain 0
             :status "IDLE"})

(defn work->int
  "업무를 수로 변환. A->61, B->62"
  [work]
  (+ (- (util/char->int (str work)) (util/char->int "A")) 61))

(defn remove-works-from-list
  "업무를 업무 목록에서 삭제"
  [want-to-delete-works remain-works]
  (reduce (fn [acc work] (aoc7-1/remove-work-from-child 
                          work 
                          (aoc7-1/remove-work-from-parent work acc)))
          remain-works
          want-to-delete-works))

(defn occupy-work 
  "worker에게 일을 할당"
  [worker work] 
  (assoc worker 
         :remain (work->int work) 
         :work work 
         :status "WORKING"))

(defn idle-workers
  [workers] 
  (filter #(= "IDLE" (:status %)) workers))

(defn assign-works
  "next-work를 찾아서 idle-workers에게 할당"
  [acc idle-workers]
  (reduce (fn [acc worker] 
            (let [uncompleted-works (set (map str (aoc7-1/next-works (:remain-works acc)))) ; 완료되지 않은 일
                  processing-works (set (map str (:processing-works acc))) ; 현재 진행중인 일
                  next-work (set/difference uncompleted-works processing-works)]
              (if (> (count next-work) 0)
                (let [first-next-work (str (first next-work))] 
                  (assoc acc 
                         :workers (assoc (vec (:workers acc)) 
                                         (:index worker) 
                                         (occupy-work worker first-next-work))
                         :processing-works (vec (set (conj (:processing-works acc) (str first-next-work))))))

                ; 다음 일이 없을 때, acc 그대로 반환
                acc)))
          ; reduce default value
          acc
          ; iterate workers
          idle-workers))

(defn working! 
  "worker의 남은 시간을 감소시키고 0이 되었을 때, 상태를 idle로 변경함."
  [worker] 
  (if (= 1 (:remain worker)) 
    (assoc worker 
           :final-work (:work worker) 
           :work nil 
           :status "IDLE") 
    (update worker :remain dec)))

(defn tick-tick!
  [acc tick]
  (let [workers (map #(working! %) (:workers acc)) ; 모든 워커 tick 증가
        final-works (filter util/not-nil? (map #(:final-work %) workers))]
    (assoc acc 
           :tick tick
           :workers workers 
           :remain-works (remove-works-from-list final-works (:remain-works acc)) ; 완료된 일을 업무 목록에서 삭제
           :resolved-works (vec (set (conj (set (:resolved-works acc)) (set final-works)))) ; 종료된 작업 추가
           :processing-works (vec (apply disj (set (:processing-works acc)) (set final-works)))))) ;종료된 작업 processing-works 에서 삭제

(defn solve-with-workers
  [input]
  (let [total-works (concat (aoc7-1/dependency-works (aoc7-1/raw->works input))
                            (aoc7-1/generate-root-works (aoc7-1/raw->works input)))
        workers (vec (map #(assoc WORKER :index %) (range 0 5)))]
    (->> (range 0 10000) ; tick을 정해진 수가 아닌 무한으로 변경하기 (0 10000 삭제하면됨)
         (reduce (fn [acc tick] 
                   (let [acc (assoc acc :tick tick)
                         idle-workers (idle-workers (:workers acc))]
                     ; 남은 일이 없을 때, 프로그램 종료
                     (util/print-interm 
                      (if (= 0 (+ (count (:processing-works acc)) (count (:remain-works acc))))
                        (reduced acc)

                        ; 일 할당 후 workers의 tick 증가
                        #_(let [ticked (tick-tick! acc tick)]
                          (assign-works ticked idle-workers))
                        (let [acc+assigned-works (assign-works acc idle-workers)] 
                          (tick-tick! acc+assigned-works tick))
                        ))))
                 ; reduce default value
                 {:tick 0 
                  :workers workers 
                  :total-works total-works 
                  :remain-works total-works
                  :processing-works []
                  :resolved-works []}))))

(comment 
  (solve-with-workers aoc7-1/test-input)
  (solve-with-workers input))
