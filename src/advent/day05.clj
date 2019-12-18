(ns advent.day05
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
    ;;(pp/pprint [a b c])
    [a b c]))

(defn opt1 [idx lst]
  ;; (pp/pprint "opt1")
  ;; (pp/pprint idx)
  ;; (pp/pprint lst)
  (let [[optcode a b result] (take 4 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        param1 (if (= m1 1) a (nth lst a))
        param2 (if (= m2 1) b (nth lst b))
        value (+ param1 param2)]
    [(+ idx 4)
     (if (= m3 1)
       (do
         (pp/pprint "Opt1 with immediate mode for result")
         ;; (pp/pprint optcode)
         ;; (pp/pprint a)
         ;; (pp/pprint b)
         ;; (pp/pprint result)
         ;; (pp/pprint m3)
         ;; (pp/pprint m2)
         ;; (pp/pprint m1)
         )
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


(def input-atom (atom 0))

(defn read-input []
  @input-atom)

(def output-atom (atom 0))

(defn write-output [v]
  (reset! output-atom v))


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
       (assoc lst a (read-input))
       )]))

(defn opt4
  "Output the value at or of the parameter"
  [idx lst]
  ;; write output
  (let [[optcode a] (take 2 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        ]
    [(+ idx 2)
     (if (= m2 1)
       (pp/pprint "Opt3 with immediate mode for result")

       (do
         (write-output (get lst a))
         lst))]))

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
    (let [optcode (get-optcode (nth lst idx))]
      ;; (pp/pprint optcode)
      (let [[idx lst] (case optcode
                        1 (opt1 idx lst)
                        2 (opt2 idx lst)
                        3 (opt3 idx lst)
                        4 (opt4 idx lst)
                        99 (opt99 idx lst))]
        (if (= -1 idx)
          @output-atom
          (recur idx lst))))))


(defn part1 []
  (let [program-input 1]
    (reset! input-atom program-input)
    (intcode (input))))


(comment
  ;; advent.day05> (part1)
  ;; 16489636

)
