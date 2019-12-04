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
