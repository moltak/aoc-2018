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

(def worker {:index 0
             :work nil
             :final-work nil
             :remain 0
             :status "IDLE"})

(defn work-time
  [work]
  (+ (- (util/char->int work) (util/char->int "A")) 61))

(defn resolve-work
  [work works]
  (aoc7-1/remove-work-from-child work (aoc7-1/remove-work-from-parent work works)))

(defn occupy-work [worker work] 
  (assoc worker 
         :remain (work-time work) 
         :work work 
         :status "WORKING"))

(defn dec-remain [worker] 
  (if (= 0 (:remain worker)) 
    (assoc worker 
           :final-work (:work worker) 
           :work nil 
           :status "IDLE") 
    (update worker :remain dec)))

(defn next-work 
  "다음일을 찾음. "
  [workers remain-works]
  (let [workers-work (map #(:work %) workers)
        next-work (str (aoc7-1/next-work remain-works))
        reamin-works (map #(first %) remain-works)]
    (if (util/in? reamin-works next-work)
      next-work
      nil)))

(defn idle-workers
  [workers] 
  (filter #(= "IDLE" (:status %)) workers))

(defn do-dec-remain
  [acc worker tick]
  {:tick tick
   :workers (assoc (:workers acc) (:index worker) (dec-remain worker))
   :total-works (:total-works acc)
   :remain-works (:remain-works acc)
   :resolve-works (:resolve-works acc)})

(defn solve-with-workers
  [input]
  (let [total-works (concat (aoc7-1/dependency-works (aoc7-1/raw->works input))
                            (aoc7-1/generate-root-works (aoc7-1/raw->works input)))
        workers (vec (map #(assoc worker :index %) (range 0 1)))]
    (->> (range 0 10000) 
         (reduce (fn [acc tick] 
                   (let [next-work (aoc7-1/next-work (:remain-works acc))
                         worker (first (:workers acc))]
                     (if (= 0 (count (:remain-works acc)))
                       (reduced {:tick tick
                                 :workers (:workers acc)
                                 :total-works (:total-works acc)
                                 :remain-works (:remain-works acc)
                                 :resolve-works (:resolve-works acc)})

                       (if (util/not-nil? next-work)
                         (if (util/idle? worker) 
                           {:tick tick
                            :workers (assoc (:workers acc) (:index worker) (occupy-work worker next-work))
                            :total-works (:total-works acc)
                            :remain-works (resolve-work next-work (:remain-works acc)) ; WORKING->IDLE로 바뀐 시점에 호출되야함.
                            :resolve-works (conj (:resolve-works acc) next-work)}
                           (do-dec-remain acc worker tick))
                         (do-dec-remain acc worker tick)))))
                 ; reduce default value
                 {:tick 0 
                  :workers workers 
                  :total-works total-works 
                  :remain-works total-works
                  :resolve-works []}))))

(comment 
  (solve-with-workers aoc7-1/test-input)
  (solve-with-workers input)
  )
