(ns advent.day05-02
  (:require [clojure.pprint :as pp]))

;; ----------------------------------------------------------------------------------------------------
;; Add to the Day2 Intcode computer
;;
;; Optcode 1
;; 1 a b c
;; Add the values at positions a and b and store at position index c
;;
;; Optcode 2
;; 2 a b c
;; Multiply the values at positions a and b and store at position index c
;;
;; Optcode 99
;; Halts the program
;;
;; Optcode 3
;; 3 a
;; Takes input and saves it to the position given as a parameter
;;
;; Optcode 4
;; 4 a
;; Outputs the value of it's parameter
;;
;; Opcode 5 
;; jump-if-true
;; if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
;; 5 a b
;;
;; Opcode 6
;; jump-if-false
;; if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
;; 6 a b

;; Opcode 7
;; is less than
;; if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
;; 7 a b c

;; Opcode 8
;; is equals
;;  if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
;; 8 a b c

;; ----------------------------------------------------------------------------------------------------

;; PArameter Modes
;;
;; Optcodes are 4-5 digits long
;; The lower two digits are the optcode
;; Paramter modes are in the high bits
;; Leading 0s are not present
;;
;; 0 == Position - interpret the value as a position
;; 1 == Immediate - value is interpreted as a value
;;
;; ABCDE
;; 11101
;;
;; A - position mode of the 3rd parameter
;; B - position mode of the 2nd parameter
;; C - position mode of the 1st parameter
;; DE - optcode
;; ----------------------------------------------------------------------------------------------------

(defn input []
  (mapv read-string (clojure.string/split (slurp "resources/day05/input.txt") #",")))

(def input-atom (atom 0))

(defn read-input []
  @input-atom)

(def output-atom (atom 0))

(defn write-output [v]
  (reset! output-atom v))


(defn prepend [s length]
  ;; prepend 0s
  (apply str (concat (take (- length (count s)) (repeat "0")) s)))

(defn get-parameter-modes [val]
  ;; remove last two digits (optcode)
  ;; get A B C modes
  (let [val (str val)
        s (prepend val 5)
        [a b c] (mapv (fn [c] (if (= \space c)
                                0
                                (Integer/parseInt (str c)))) (first (split-at (- (count s) 2) s)))]
    [a b c]))

(defn opt1 [idx lst]
  (let [[optcode a b result] (take 4 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        param1 (if (= m1 1) a (nth lst a))
        param2 (if (= m2 1) b (nth lst b))
        value (+ param1 param2)]
    [(+ idx 4)
     (if (= m3 1)
       (pp/pprint "Opt1 with immediate mode for result")
       (assoc lst result value))]))

(defn opt2 [idx lst]
  (let [[optcode a b result] (take 4 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        param1 (if (= m1 1) a (nth lst a))
        param2 (if (= m2 1) b (nth lst b))
        value (* param1 param2)]
    [(+ idx 4)
     (if (= m3 1)
       (pp/pprint "Opt2 with immediate mode for result")
       (assoc lst result value))]))

(defn opt3 
  "Read the input and store location of parameter"
  [idx lst]
  ;; read input
  (let [[optcode a] (take 2 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        ]
    [(+ idx 2)
     (if (= m1 1)
       (pp/pprint "Opt3 with immediate mode for result")
       (assoc lst a (read-input)))]))

(defn opt4
  "Output the value at or of the parameter"
  [idx lst]
  ;; write output
  ;; (pp/pprint (format "opt4 - %s %s" idx lst))
  (let [[optcode a] (take 2 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        val (if (= m1 1) a (nth lst a))
        ]
    [(+ idx 2)
     (if (= m2 1)
       (pp/pprint "Opt3 with immediate mode for result")
       (do
         ;; (pp/pprint (format "Writing %s to output" val))
         (write-output val)
         lst))]))

(defn opt5
  ;; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
  [idx lst]
  (let [[optcode a b] (take 3 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        param1 (if (= m1 1) a (nth lst a))
        param2 (if (= m2 1) b (nth lst b))
        idx2 (if (not= param1 0) param2 (+ idx 3))]
    [idx2
     lst]))

(defn opt6
  ;; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
  [idx lst]
  (let [[optcode a b] (take 3 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        param1 (if (= m1 1) a (nth lst a))
        param2 (if (= m2 1) b (nth lst b))
        idx2 (if (= param1 0) param2 (+ idx 3))]
    [idx2
     lst]))

(defn opt7
  ;; Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
  [idx lst]
  (let [[optcode a b c] (take 4 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        param1 (if (= m1 1) a (nth lst a))
        param2 (if (= m2 1) b (nth lst b))
        param3 c]
    [(+ idx 4)
     (if (< param1 param2) 
       (assoc lst param3 1)
       (assoc lst param3 0))]))

(defn opt8
  ;; Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
  [idx lst]
  (let [[optcode a b c] (take 4 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        param1 (if (= m1 1) a (nth lst a))
        param2 (if (= m2 1) b (nth lst b))
        param3 c]
    [(+ idx 4)
     (if (= param1 param2) 
       (assoc lst param3 1)
       (assoc lst param3 0))]))

(defn opt99 [idx lst]
  [-1 lst])

(defn get-optcode [val]
  ;; get last two digits as integer
  (let [s (str val)
        length (count s)]
    (Integer/parseInt (apply str (take 2 (nthrest s (- length 2)))))))

(defn intcode [lst]
  (loop [idx 0
         lst lst]
    ;; (pp/pprint (format "%s - %s" idx lst))
    (let [optcode (get-optcode (nth lst idx))]
      (let [[idx lst] (case optcode
                        1 (opt1 idx lst)
                        2 (opt2 idx lst)
                        3 (opt3 idx lst)
                        4 (opt4 idx lst)
                        5 (opt5 idx lst)
                        6 (opt6 idx lst)
                        7 (opt7 idx lst)
                        8 (opt8 idx lst)
                        99 (opt99 idx lst))]
        ;; (pp/pprint (format "%s - %s" idx lst))
        ;; (pp/pprint "--")
        (if (= -1 idx)
          ;; @output-atom
          lst
          (recur idx lst))))))


(defn run [input lst]
  (do
    (reset! input-atom input)
    (intcode lst)
    @output-atom))

(comment
  ;; Equal 8
  (run 8 [3,9,8,9,10,9,4,9,99,-1,8])

  ;; less than 8
  (run 7 [3,9,7,9,10,9,4,9,99,-1,8])
  ;; => 1
  ;; Equal 8
  (run 8, [3,3,1108,-1,8,3,4,3,99])
  ;; => 1
  (run 7, [3,3,1108,-1,8,3,4,3,99])
  ;; => 0

  ;; Less than 8
  (run 7, [3,3,1107,-1,8,3,4,3,99 ])
  ;; => 1
  (run 9, [3,3,1107,-1,8,3,4,3,99 ])
  ;; => 0


  (run 0 [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9])
  ;;=> 0
  (run 1 [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9])
  ;; => 1


  ;; This one is broken
  (run 7, [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])
  ;; => 999

  (run 8, [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])
  ;;=> 1000

  (run 9, [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])
  ;; => 1001

)

(comment 
;; | 0    | 2            | 6          | 9           | 13         | 16        | 19     | 22             | 26   | 28        | 31      | 33        | 36             | 40   | 42        | 45 | 46 |
;; | 3 21 | 1008 21 8 20 | 1005 20 22 | 107 8 21 20 | 1006 20 31 | 1106 0 36 | 98 0 0 | 1002 21 125 20 | 4 20 | 1105 1 46 | 104 999 | 1105 1 46 | 1101 1000 1 20 | 4 20 | 1105 1 46 | 98 | 99 |
;; |------+--------------+------------+-------------+------------+-----------+--------+----------------+------+-----------+---------+-----------+----------------+------+-----------+----+----|
;; | 3 21 | 1008 21 8 20 | 1005 20 22 | 107 8 21 20 | 1006 20 31 | 1106 0 36 | 98 0 7 | 1002 21 125 20 | 4 20 | 1105 1 46 | 104 999 | 1105 1 46 | 1101 1000 1 20 | 4 20 | 1105 1 46 | 98 | 99 |

)

(defn part2 []
  (run 5 (input)))


(comment
  (part2)
  ;; => 9386583
)
