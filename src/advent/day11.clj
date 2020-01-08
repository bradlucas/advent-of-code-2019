(ns advent.day11
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

;; input is an intcode program

;; build a hull painting robot
;; the input program will be the brain
;; input instructions are used to access the robot's camera
;; provide 0 if the robot is over a black panel
;; provide 1 if the robot is over a white panel

;; output will be:
;; value to indicate the color to paint the panel; 0 => paint it black, 1 => paint it white
;; value to indicate the direction to turn; 0 => left 90 degress, 1 => right 90 degress

;; after the robot turns it should move forward exactly 1 panel

;; the robot starts facing up



;; find the number of panels the robot will paint at lease once

;; ----------------------------------------------------------------------------------------------------

(defn input []
  (mapv read-string (str/split (slurp "resources/day11/input.txt") #",")))


;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------
;; Starting with Day09 Intcode

;; ----------------------------------------------------------------------------------------------------
;; Input/Output Queue
;; ----------------------------------------------------------------------------------------------------

;; modify to use two queues
(def in (atom (clojure.lang.PersistentQueue/EMPTY)))
(def out (atom (clojure.lang.PersistentQueue/EMPTY)))

(defn reset-queues []
  (reset! in (clojure.lang.PersistentQueue/EMPTY))
  (reset! out (clojure.lang.PersistentQueue/EMPTY)))

(defn set-input [v]
  (reset! in (clojure.lang.PersistentQueue/EMPTY))
  (reset! in (conj @in v)))

(defn read-input []
  (let [v (peek @in)]
    (reset! in (pop @in))
    v))

(defn read-output []
  (let [v (peek @out)]
    (reset! out (pop @out))
    v))

(defn write-output [v]
  (reset! out (conj @out v)))

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

;; Modified to return without incrementing the idx pointer if there is no input
(defn opt3 
  "Read the input and store location of parameter"
  [idx lst]
  (let [[optcode a] (take 2 (nthrest lst idx))
        [_ _ m1] (get-parameter-modes optcode)
        ;; val (read-input)
        ]
    (if (empty? @in)
      ;; waiting for input
      (do
        ;;(pp/pprint "waiting for input")
        [idx lst])
      (let [val (read-input)]
        ;;(pp/pprint "reading input")
        [(+ idx 2)
         (put-val lst a val m1)]))))

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


;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------



;; Modified to return if waiting for input, two items are in the output queue or the halt statement
(defn intcode [lst idx]
  ;; Run program until it needs more input or halts
  ;;(pp/pprint (format "intcode %s" lst))
  (loop [idx idx
         lst lst]
    (let [optcode (get-optcode (first (get-nth lst idx)))]
      ;;(pp/pprint optcode)
      (let [empty-input (empty? @in)
            [idx lst] (case optcode
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
        (if (or (= optcode 99) 
                (and (= optcode 3) empty-input)
                (= 2 (count @out)))
          [idx lst]
          (recur idx lst))))))

;; Some ideas from reviewing https://github.com/Average-user/adventofcode-clj-2019/blob/master/src/adventofcode_clj_2019/day11.clj

(defn run-until-out-or-end [lst idx]
  (intcode lst idx))

(def turn-right (into {} (map vec (partition 2 1 [:north :east :south :west :north]))))
(def turn-left (comp turn-right turn-right turn-right))

(defn move [[x y] direction]
  (case direction
    :north [x (inc y)]
    :south [x (dec y)]
    :east [(inc x) y]
    :west [(dec x) y]))

(defn robot [lst grid]
  (loop [[x y] [0 0]
         grid grid
         dir :north
         lst lst
         idx 0
         step 0]
    (let [color (get grid [x y] 0)]
      (set-input color)
      (let [[idx lst] (run-until-out-or-end lst idx)]
        (if (or (= -1 idx) (= step 9571))
          ;;grid
          {:grid grid
           :ids idx
           :lst lst
           :step step}
          ;; else
          (do
            (let [color (read-output)
                  turn (read-output)
                  new-dir (case turn
                            0 (turn-left dir)
                            1 (turn-right dir))
                  new-pos (move [x y] new-dir)
                  new-grid (conj grid [[x y] color])]
              (recur new-pos new-grid new-dir lst idx (inc step)))))))))


;; ----------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------


(defn part1 []
  (reset! base 0)
  (reset-queues)
  (count (:grid (robot (input) {}))))


(comment
  (part1)
  ;; => 2041
)


