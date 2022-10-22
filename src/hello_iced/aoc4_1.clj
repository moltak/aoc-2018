(ns hello-iced.aoc4-1
  (:require [clojure.string :as str]
            [clojure.test :as test]))
(use 'clojure.set)

(comment "
파트 1
입력:

[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up

키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.


파트 2
주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.")


(def test-input ["[1518-11-01 00:00] Guard #10 begins shift"
                 "[1518-11-01 00:55] wakes up"
                 "[1518-11-01 23:58] Guard #99 begins shift"
                 "[1518-11-02 00:40] falls asleep"
                 "[1518-11-02 00:50] wakes up"
                 "[1518-11-03 00:05] Guard #10 begins shift"
                 "[1518-11-03 00:24] falls asleep"
                 "[1518-11-03 00:29] wakes up"
                 "[1518-11-04 00:02] Guard #99 begins shift"
                 "[1518-11-04 00:36] falls asleep"
                 "[1518-11-04 00:46] wakes up"
                 "[1518-11-05 00:03] Guard #99 begins shift"
                 "[1518-11-01 00:05] falls asleep"
                 "[1518-11-01 00:25] wakes up"
                 "[1518-11-01 00:30] falls asleep"
                 "[1518-11-05 00:45] falls asleep"
                 "[1518-11-05 00:55] wakes up"])


(def input4-1 (str/split-lines (slurp "resources/aoc4_1.input")))

(defn to-timeschedule
  "문제에서 정의한 형식의 코드 한 줄을 받아 스케줄 표로 변환합니다."
  [raw-code]
  (let [[date] (re-find #"(\[[^\[\]]+\])" raw-code)
        [guard] (re-find #"(\#\d+)" raw-code)
        [asleep] (re-find #"(falls asleep)" raw-code)
        [wake-up] (re-find #"(wakes up)" raw-code)
        ]
    {
     :date (str/replace date #"\[|\]" "")
     :guard guard
     :asleep asleep
     :wake_up wake-up
     }))


(comment
  (to-timeschedule (first test-input))
  (->> test-input
       (map to-timeschedule)
       (sort (fn [a b] (compare (a :date) (b :date))))))

(defn for-guard-schedule
  "가드의 시간표로 묶어서 표현합니다.
  [1518-11-01 00:00] Guard #10 begins shift
  [1518-11-01 00:05] falls asleep
  [1518-11-01 00:25] wakes up
  [1518-11-01 00:30] falls asleep
  [1518-11-01 00:55] wakes up

  -> guard: #10, { :'1518-11-01 00:05' asleep  :'time' wakes up  :'time' falls asleep  :'time' wakes up
  "
  [schedules]
  (->> schedules
       (sort (fn [a b] (compare (a :date) (b :date))))
       (reduce (fn [acc x] 
                 (if (x :guard) 
                   (conj acc x) 
                   (conj acc (assoc x :guard ((last acc) :guard))))) 
               [])))

(comment 
  (->> test-input 
       (map to-timeschedule)
       for-guard-schedule
       (group-by :guard)))


(defn 가드가-자는-수
  [by-guard-schedules]
  (->> by-guard-schedules
       (filter (fn [x] (x :asleep)))
       count))

(comment 
  (->> test-input 
       (map to-timeschedule)
       for-guard-schedule
       (group-by :guard)
       (map (fn [x] (가드가-자는-수 (second x))))))
