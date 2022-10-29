(ns hello-iced.2020.aoc8-2
  (:require [clojure.string :as str]))

(comment
  "주어진 지시들 중, 정확히 하나의 지시가 잘못된 것을 알게 되었다. 정확히 하나의 jmp가 nop가 되어야하거나, nop가 jmp가 되면 프로그램은 종료된다.

  nop +0  | 1
  acc +1  | 2
  jmp +4  | 3
  acc +3  |
  jmp -3  |
  acc -99 |
  acc +1  | 4
  nop -4  | 5 ;; 여기!
  acc +6  | 6

  위의 예시에서, '여기!' 라고 표기된 곳이 jmp에서 nop로 바뀌면, 지시는 무한히 반복하지 않고 마지막에 6을 반환하며 종료된다. 프로그램이 종료되는 시점의 accumulator의 값을 반환하여라.")

(comment
  "PC(Program Counter) IR(Instruction Register) AC(Accomulator) TR(Temporary Register) DR(Data Register) IH(Instruction History)
  코드 실행 순서
  1. 명령어 fetch -> 해석
  2. 레지스터 세팅
  3. 명령 실행 (AC 변경 + 다음 명령 fetch 해석) 

  예제)
  nop +0 -> PC: 0, IR: nop, AC: 0, TR: 1, DR: 0, IH: [nop +0] -> 실행 -> PC+=TR, AC+=DR -> 다음 명령어로 이동
  acc +1 -> PC: 1, IR: acc, AC: 0, TR: 1, DR: 1, IH: conj IH 'acc +1 PC' -> 실행 -> PC+=TR, AC+=DR
  jmp +4 -> PC: 2, IR: jmp, AC: 1, TR: 4, DR: 0, IH: conj IH 'jmp +4 PC' -> 실행 -> PC+=TR, AC+=DR
  acc +1 -> PC: 6, IR: acc, AC: 1, TR: 1, DR: 1, IH: conj IH 'acc +1 PC' -> 실행 -> PC+=TR, AC+=DR

  종료 시점)
  (in? IH 'acc +1 2') 
  "
  )

(def test-input 
  (str/split-lines 
"nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"))

(def input (str/split-lines (slurp "resources/2020/aoc8_2.input")))

(def REGISTER {:PC 0 :IR "" :AC 0 :TR 0 :DR 0 :inst-history [] :RAW ""})

(defn print-interm
  [x]
  (println x) x)

(defn string-in?
  [s elm]
  (not= nil (str/index-of s elm)))

(defn in? 
  "true if coll contains elm"
  [coll elm]  
  (some #(= elm %) coll))

(defn in-history?
  "명령어가 이미 실행된적 있는지 확인"
  [register instruction]
  (in? (register :inst-history) 
       (str instruction " " (register :PC))))

(defn 명령어->cmd+operand
  [inst]
  (let [splitted (str/split inst #" ")]
    [(first splitted) (parse-long (second splitted))]))

(defn 명령어->레지스터-저장
  [register instruction]
  (let [[command operand] (명령어->cmd+operand instruction)]
    (into register 
          {:IR command
           :DR (if (= command "acc") operand 0)
           :TR (if (= command "jmp") operand 1)
           ; 명령어를 실행 할 때, 실행된 명령어와 PC 를 함께 저장
           :RAW (str instruction " " (register :PC))})))

(def 명령어->함수-map 
  {:default #(into % {:PC (+ (% :PC) (% :TR)) 
                      :TR 0
                      :inst-history (conj (% :inst-history) (% :RAW))})
   :acc #(into % {:PC (+ (% :PC) (% :TR)) 
                  :AC (+ (% :AC) (% :DR))
                  :TR 0
                  :DR 0
                  :inst-history (conj (% :inst-history) (% :RAW))})})

(defn execute
  "명령어 실행"
  [register]
  (cond
    (= (register :IR) "acc") ((명령어->함수-map :acc) register)
    :else ((명령어->함수-map :default) register)))

(defn solve 
  [instructions]
  (loop [register (into {} REGISTER) 
         instruction (first instructions)]
    (cond 
      (in-history? register instruction) {:result false}
      (= (register :PC) (dec (count instructions))) {:register (dissoc register :inst-history) :result true}
      :else (let [reg-res (execute (명령어->레지스터-저장 register instruction))]
              (recur reg-res (nth instructions (reg-res :PC)))))))

(defn nop->jmp|jmp->nop
  [instruction]
  (cond 
    (string-in? instruction "nop") (str/replace instruction #"nop" "jmp")
    (string-in? instruction "jmp") (str/replace instruction #"jmp" "nop")
    :else instruction))

(defn solve2
  [instructions]
  (for [i (range 0 (count instructions))
        :let [res (->> i
                       (nth instructions)
                       (nop->jmp|jmp->nop)
                       (assoc instructions i)
                       (solve))]
        :when (true? (res :result))]
    res))

(comment
  (solve2 test-input)
  (solve2 input)
  )

