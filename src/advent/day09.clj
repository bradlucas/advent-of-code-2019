(ns advent.day09
  (:require [clojure.pprint :as pp]))


;; ----------------------------------------------------------------------------------------------------
;; BOOST program - Basic Operation Of System Test
;; (your program input is the BOOST program)

;; Existing Intcode computer needs support for parameters in relative mode
;; See Day 5 for more recent Intcode computer

;; ----------------------------------------------------------------------------------------------------


(defn input []
  (mapv read-string (clojure.string/split (slurp "resources/day09/input.txt") #",")))


;; ----------------------------------------------------------------------------------------------------
;; Input/Output Queue
;; ----------------------------------------------------------------------------------------------------

(def q (atom (clojure.lang.PersistentQueue/EMPTY)))

(defn push-queue [v]
  (reset! q (conj @q v)))

(defn set-input [v]
  (reset! q (clojure.lang.PersistentQueue/EMPTY))
  (push-queue v))

(defn pop-queue []
  (let [v (peek @q)]
    (reset! q (pop @q))
    v))

(defn read-input []
  (let [v (pop-queue)]
    v))

(defn write-output [v]
  (pp/pprint (format "output => %s" v))
  (push-queue v))

;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------


;; ----------------------------------------------------------------------------------------------------
;; 'Memory' routines

(def base (atom 0))

(defn update-base [v]
  (reset! base (+ @base v)))

(defn pad-right [lst pos]
  (vec (concat lst (take (inc (- pos (count lst))) (repeat 0)))))

(defn extend-if-neccessary [lst pos]
  (if (>= pos (count lst))
    (pad-right lst pos)
    lst))

(defn put-assoc [l pos v]
  ;; (assoc lst result value)
  ;; (pp/pprint (format "put-assoc %s %s %s" l pos v))
  (let [l (extend-if-neccessary l pos)]
    (assoc l pos v)))

(defn put-val
  [lst pos val mode]
  (case mode
    0 (put-assoc lst pos val)
    1 (pp/pprint "Error: Mode 1 on put")
    2 (put-assoc lst (+ @base pos) val)))

(defn get-nth
  "Get from lst while extending if necessary. Return the requested value along with the lst (expecially if updated)."
  [lst pos]
  (let [lst (extend-if-neccessary lst pos)]
    [(nth lst pos)
     lst]))

(defn get-val 
  "Get val per the parameter mode

0 = position mode  - the value is a position in memory [0-based over lst]
1 = immediate mode - value is interpreted as a value
2 = relative mode  - like position mode but count from the relative base
"
  [lst mode val]
  (case mode
    0 (get-nth lst val)
    1 [val lst]
    2 (get-nth lst (+ @base val))))


;; ----------------------------------------------------------------------------------------------------
;; Intcode OptCodes


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
  (let [[optcode a b c] (take 4 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        [param1 lst] (get-val lst m1 a)
        [param2 lst] (get-val lst m2 b)
        val (+ param1 param2)]
    [(+ idx 4)
     (put-val lst c val m3)]))

;; opt1 tests

;; 0001

(= (second (opt1 0 [1 1 1 4 0]))
   [1 1 1 4 2])

;; 2221
(= (second (opt1 0 [22201 1 1 4 0]))
   [22201 1 1 4 2])


(defn opt2 [idx lst]
  (let [[optcode a b c] (take 4 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        [param1 lst] (get-val lst m1 a)
        [param2 lst] (get-val lst m2 b)
        val (* param1 param2)]
    [(+ idx 4)
     (put-val lst c val m3)]))

(= (second (opt2 0 [1 1 1 4 0]))
   [1 1 1 4 1])

(= (second (opt2 0 [22201 1 1 4 0]))
   [22201 1 1 4 1])


(defn opt3 
  "Read the input and store location of parameter"
  [idx lst]
  (let [[optcode a] (take 2 (nthrest lst idx))
        [_ _ m1] (get-parameter-modes optcode)
        val (read-input)]
    [(+ idx 2)
     (put-val lst a val m1)]))

(defn opt4
  "Output the value at or of the parameter"
  [idx lst]
  (let [[optcode a] (take 2 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        [val lst] (get-val lst m1 a)]
    [(+ idx 2)
     (if (= m2 1)
       (pp/pprint "Opt4 with immediate mode for result")
       (do
         (write-output val)
         lst))]))

(defn opt5
  "Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing."
  [idx lst]
  (let [[optcode a b] (take 3 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        [param1 lst] (get-val lst m1 a)
        [param2 lst] (get-val lst m2 b)]
    (let [idx2 (if (not= param1 0) param2 (+ idx 3))]
      [idx2
       lst])))

(defn opt6
  "Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing."
  [idx lst]
  (let [[optcode a b] (take 3 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        [param1 lst] (get-val lst m1 a)
        [param2 lst] (get-val lst m2 b)
        idx2 (if (= param1 0) param2 (+ idx 3))]
    [idx2
     lst]))

(defn opt7
  "Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0."
  [idx lst]
  (let [[optcode a b c] (take 4 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        [param1 lst] (get-val lst m1 a)
        [param2 lst] (get-val lst m2 b)
        param3 c]
    [(+ idx 4)
     (if (< param1 param2) 
       (put-val lst param3 1 m3)
       (put-val lst param3 0 m3))]))

(defn opt8
  "Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0."
  [idx lst]
  (let [[optcode a b c] (take 4 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        [param1 lst] (get-val lst m1 a)
        [param2 lst] (get-val lst m2 b)
        param3 c]
    [(+ idx 4)
     (if (= param1 param2) 
       (put-val lst param3 1 m3)
       (put-val lst param3 0 m3))]))

(defn opt9
  "Adjusts the relative base by the value of its only parameter"
  [idx lst]
    (let[[optcode a] (take 2 (nthrest lst idx))
       [m3 m2 m1] (get-parameter-modes optcode)
       [val lst] (get-val lst m1 a)]
    (update-base val)
    [(+ idx 2)
     lst]))

(defn opt99 [idx lst]
  [-1 lst])


;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------

(defn get-optcode [val]
  ;; get last two digits as integer
  ;; (pp/pprint (format "get-optcode %s" val))
  (let [s (str val)
        length (count s)]
    (Integer/parseInt (apply str (take 2 (nthrest s (- length 2)))))))

(defn intcode [lst]
  ;;(pp/pprint (format "intcode %s" lst))
  (loop [idx 0
         lst lst]
    (let [optcode (get-optcode (first (get-nth lst idx)))]
      (let [[idx lst] (case optcode
                        1 (opt1 idx lst)
                        2 (opt2 idx lst)
                        3 (opt3 idx lst)
                        4 (opt4 idx lst)
                        5 (opt5 idx lst)
                        6 (opt6 idx lst)
                        7 (opt7 idx lst)
                        8 (opt8 idx lst)
                        9 (opt9 idx lst)
                        99 (opt99 idx lst))]
        (if (not (= optcode 99))
          (recur idx lst))))))



;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------



;; [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] takes no input and produces a copy of itself as output.
(defn example1 []  (intcode [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]))

;; [1102,34915192,34915192,7,4,7,99,0] should output a 16-digit number.
(defn example2 [] (intcode [1102,34915192,34915192,7,4,7,99,0]))

;; [104,1125899906842624,99] should output the large number in the middle.
(defn example3 [] (intcode [104,1125899906842624,99]))

(defn run-test1 []
  (set-input 5)
  (reset! base 0)
  (opt3 0 [003 3 99 3]))

(defn run-test2 []
  (set-input 5)
  (reset! base 0)
  (opt3 0 [203 3 99 3]))



(defn part1 []
  (set-input 1)
  (reset! base 0)
  (intcode (input)))

(comment
  (part1)
  ;; "output => 2465411646"
  )



;; ----------------------------------------------------------------------------------------------------
;; Part2

;; run in sensor boost mode by providing the input of 2

(defn part2 []
  (set-input 2)
  (reset! base 0)
  (intcode (input)))


(comment
  (part2)
  ;; "output => 69781"
  )
