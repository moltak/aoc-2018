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
        [shift] (re-find #"(begins shift)" raw-code)
        [asleep] (re-find #"(falls asleep)" raw-code)
        [wake-up] (re-find #"(wakes up)" raw-code)
        ]
    {
     :date (str/replace date #"\[|\]" "")
     :guard guard
     :shift shift
     :asleep asleep
     :wake_up wake-up
     }))


(comment
  (to-timeschedule (first test-input))
  (->> test-input
       (map to-timeschedule)
       (sort (fn [a b] (compare (a :date) (b :date))))))

(defn for-guard-schedule
  "가드의 시간표로 묶어서 출력함
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
  (->> input4-1
       (map to-timeschedule)
       for-guard-schedule
       (group-by :guard)))

(defn 가드의-잠든-모든-분을-리스트로-변환
  [schedules]
  (->> schedules
       (map (fn [x] (assoc x :minute (parse-long (subs (re-find #":\d\d" (x :date)) 1))))) ; date에서 minute만 찾아 feild를 long type으로 생성합니다.
       (reduce (fn [acc x] 
                 (if (x :wake_up)
                   (conj acc 
                         (assoc 
                          {:guard (x :guard)} 
                          :minutes (range ((last acc) :minute) (x :minute))))
                   (conj acc x))) [])))

(comment 
  (->> test-input 
       (map to-timeschedule)
       for-guard-schedule
       가드의-잠든-모든-분을-리스트로-변환))

(defn 잠든-모든-분을-가드-리스트로-변환
  [schedules]
  (->> schedules
       (filter (fn [x] (x :minutes))) ; :minutes가 있는 map 만 반환
       (map (fn [x] (assoc x :minutes-count (count (x :minutes))))) ; 그냥 보려고 추가
       ; 이렇게 만들고 싶다 -> {"#10" (5 6 7 30 31 24 25)}
       ; 삽질의 흔적들
       #_(group-by :guard)
       #_(map (fn [x] (map (fn [y] (select-keys y [:minutes])) (second x))))
       #_(map (fn [x] (select-keys x [:minutes])))
       #_(apply (partial merge-with into))

       #_(reduce (fn [acc x] (conj acc x)) [])
       #_(map (fn [x] 
                (reduce (fn [acc y] 
                          (if (acc :guard) 
                            (println y)
                            #_(update acc :minutes (conj (y :minutes)) )
                            (merge acc y))) 
                        {} 
                        (first (second x)))))
       ; 각 나온다........
       (reduce (fn [acc x] (let [target (acc (x :guard))] 
                             (if target 
                               (update acc (x :guard) (comp vec flatten conj) (x :minutes)) 
                               (merge acc {(x :guard) (x :minutes)})))) {})
       #_(map (fn [x] (assoc {} :guard (first x) :count (count (second x)))))
       #_(sort-by :count #(compare %2 %1))
       ))

(comment 
  (->> test-input 
       (map to-timeschedule)
       for-guard-schedule
       가드의-잠든-모든-분을-리스트로-변환
       잠든-모든-분을-가드-리스트로-변환))

(defn 가장-많이-잔-가드-반환
  [by-guard-schedules]
  (->> by-guard-schedules
       (map (fn [x] (assoc {} :guard (first x) :count (count (second x)))))
       (sort-by :count #(compare %2 %1))
       first
       ; map에서 :guard(string)만 어떻게 반환하지?
       ))

(comment
  (->> test-input 
       (map to-timeschedule)
       for-guard-schedule
       가드의-잠든-모든-분을-리스트로-변환
       잠든-모든-분을-가드-리스트로-변환
       가장-많이-잔-가드-반환))

(defn 가장-많이-잠든-시간-반환
  [by-guard-schedules]
  (->> by-guard-schedules
       (map (fn [x] (assoc {} 
                           :guard (first x) 
                           ;:frequencies (frequencies (second x))
                           :most-fre (last (sort-by second (frequencies (second x)))))))))

(comment
  (->> test-input 
       (map to-timeschedule)
       for-guard-schedule
       가드의-잠든-모든-분을-리스트로-변환
       잠든-모든-분을-가드-리스트로-변환
       가장-많이-잠든-시간-반환)

  (def 가장-많이-잔-가드 (->> input4-1
       (map to-timeschedule)
       for-guard-schedule
       가드의-잠든-모든-분을-리스트로-변환
       잠든-모든-분을-가드-리스트로-변환
       가장-많이-잔-가드-반환))

  (def 가장-많이-잠든-시간 (->> input4-1
       (map to-timeschedule)
       for-guard-schedule
       가드의-잠든-모든-분을-리스트로-변환
       잠든-모든-분을-가드-리스트로-변환
       가장-많이-잠든-시간-반환))
  )

