(ns advent.day02)


;; read day02/invput.txt
(defn input []
  (mapv read-string (clojure.string/split (slurp "resources/day02/input.txt") #",")))

;; ----------------------------------------------------------------------------------------------------
;; Part 1

;; Optcode 1 - Add
;; Read the following three values
;; The first two are the indices of the values to read
;; Add these
;; Store the result in the third indices position



;; Optcode 2 - Multiply
;; Read the following three values
;; The first two are the indices of the values to read
;; Multiply these
;; Store the result in the third indices position


;; Optcode 99 - Halt

(defn halt [lst idx]
  (= 99 (nth lst idx)))

(defn process [idx lst]
  (let [[optcode a b result-idx] (take 4 (nthrest lst idx))
        v1 (nth lst a)
        v2 (nth lst b)
        fnc (if (= optcode 1) + *)
        value (fnc v1 v2)]
    [(+ idx 4)
     (assoc lst result-idx value)]))

(defn intcode [lst]
  (loop [idx 0
         lst lst]
    (if (halt lst idx)
      lst    ;; (first lst)
      (let [[idx lst] (process idx lst)]
        (recur idx lst)))))


(defn pre-process [lst]
  ;; To do this, before running the program, replace position 1 with the value 12 and replace position 2 with the value 2.
  (assoc (assoc lst 1 12) 2 2))

(defn part1 []
  (first (intcode (pre-process (input)))))


(comment
  [1,0,0,0,99] => 2,0,0,0,99 (1 + 1 = 2)
  [2,3,0,3,99] => 2,3,0,6,99 (3 * 2 = 6)
  [2,4,4,5,99,0] => 2,4,4,5,99,9801 (99 * 99 = 9801)
  [1,1,1,4,99,5,6,0,99] => 30,1,1,4,2,5,6,0,99
  
  advent.day02> (intcode [1,0,0,0,99])
  [2 0 0 0 99]
  
  advent.day02> (intcode [2 3 0 3 99])
  [2 3 0 6 99]
  
  advent.day02> (intcode [2 4 4 5 99 0])
  [2 4 4 5 99 9801]
  
  advent.day02> (intcode [1 1 1 4 99 5 6 0 99])
  [30 1 1 4 2 5 6 0 99]
)

(comment
  (part1)
  3716293
)



;; ----------------------------------------------------------------------------------------------------
;; Part 2

;; Accept the preproccing inputs and return result
;;
;; What combination of noun verb values produces 19690720

;; https://adventofcode.com/2019/day/2#part2



;; Tried and found the answer through trial and error
;; advent.day02> (first (part2 (input) 100 0))
;; 30 749 891
;; advent.day02> (first (part2 (input) 50 0))
;; 15 389 891
;; advent.day02> (first (part2 (input) 80 0))
;; 24605891
;; advent.day02> (first (part2 (input) 60 0))
;; 18461891
;; advent.day02> (first (part2 (input) 70 0))
;; 21533891
;; advent.day02> (first (part2 (input) 64 29))
;; 19690720
;; advent.day02> (first (part2 (input) 64 5))
;; 19690696
;; advent.day02> (first (part2 (input) 64 12))
;; 19690703
;; advent.day02> (first (part2 (input) 64 24))
;; 19690715
;; advent.day02> (first (part2 (input) 64 27))
;; 19690718
;; advent.day02> (first (part2 (input) 64 29))
;; 19690720



;; Strategy
;; Increment noun values while leaving verb at 0 
;; Til you go past the desired value
;; Then using the previous noun, increment the verb until the value is found


(defn prepare [lst noun verb]
  (assoc (assoc lst 1 noun) 2 verb))

(defn process2 [lst noun verb]
  (let [lst (prepare lst noun verb)]
    (first (intcode lst))))


(defn part2 [lst goal]
  (loop [noun 0
         verb 0
         noun-loop true    ;; working on nouns, false for verbs
         ]
    (let [result (process2 lst noun verb)]
      (if (= result goal)
        [noun
         verb]
        (let [[new-noun new-verb loop-param] (if (and noun-loop)
                                              (if (> result goal)
                                                [(dec noun) (inc verb) false]
                                                [(inc noun) verb true])
                                                [noun (inc verb) false])]
          (recur new-noun new-verb loop-param))))))


;; Find the input noun and verb that cause the program to produce the output 19690720. 
;; What is 100 * noun + verb? (For example, if noun=12 and verb=2, the answer would be 1202.)

(defn solve []
  (let [[noun verb] (part2 (input) 19690720)]
    (+ (* 100 noun) verb)))



(comment
  ;; advent.day02> (solve)
  [64 29]

  ;; advent.day02> (solve)
  6429
)
