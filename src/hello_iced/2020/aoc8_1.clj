(ns hello-iced.2020.aoc8-1
  (:require [clojure.string :as str]))

(comment
  "Day 8
  https://adventofcode.com/2020/day/8
  파트 1
  일련의 지시가 입력으로 주어진다.
  acc는 전역 변수를 증가/감소 시키는 역할을 한다. acc +7은 accumulator를 7 증가 시킨다. accumulator는 0에서 시작한다.
  jmp는 현재 위치에 기반하여 새로운 지시로 넘어간다. jmp +1은 바로 다음의 지시로 넘어가는 것이고, jmp +2는 바로 다음의 지시는 건너뛰고 그 다음의 지시를 실행하는 것이다.
  nop 는 아무것도 하지 않는다. 아래는 예시이다. ")

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

(def input (str/split-lines (slurp "resources/2020/aoc8_1.input")))

(def REGISTER {:PC 0 :IR "" :AC 0 :TR 0 :DR 0 :inst-history [] :RAW ""})

(defn print-interm
  [x]
  (println x) x)

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
    (if (or (in-history? register instruction) 
            (= (register :PC) (dec (count instructions))))
      register
      (let [reg-res (execute (명령어->레지스터-저장 register instruction))]
        (recur reg-res (nth instructions (reg-res :PC)))))
    ))

(comment
  (명령어->cmd+operand (first test-input))
  (solve test-input)

  (in? (conj ["nop +0 0"] "acc +1 1") "nop +0 0")
  (in? ["nop +0 0" "acc +1 1"] "nop +0 0")

  (def ACC_REG (명령어->레지스터-저장 REGISTER "acc +1"))
  (def JMP_REG (명령어->레지스터-저장 REGISTER "jmp -4"))
  (def NOP_REG (명령어->레지스터-저장 REGISTER "nop -4"))

  (in-history? (into REGISTER {:inst-history ["acc +1 0"]}) "acc +1")

  (println ACC_REG)
  ((명령어->함수-map :acc) ACC_REG)
  ((명령어->함수-map :default) JMP_REG)
  ((명령어->함수-map :default) NOP_REG)
  )
