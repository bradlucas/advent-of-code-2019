(ns advent.day07
  (:require [clojure.pprint :as pp]))


;; ----------------------------------------------------------------------------------------------------
;; Five Amps in sequence
;; 
;; Each Amp run the Incode computer developed in Day2/day5
;;
;; Each Amp starts with a phase setting which is an integer from 0 to 4


;; Start the copy of the amplifier controller software that will run on
;; amplifier A. At its first input instruction, provide it the
;; amplifier's phase setting, 3. At its second input instruction, provide
;; it the input signal, 0.


;; This means we need to modify Day5's version of intcode which reads a single input
;; Need to have two input values that are read at different times by subsequent Opt3 instances



;; Part 1
;; What is the combination of phase settings what produces the largest output



;; All the combinations of 1-4 for 5 places

;; Run the Incode computer in series five times

;; ----------------------------------------------------------------------------------------------------
;; Inserted Day05-2 version of intcode then modified the read-input to support multiple input values
;;


(def input-atom (atom []))

(defn set-input-values [l]
  (reset! input-atom (into (clojure.lang.PersistentQueue/EMPTY) l)))

(defn read-input []
  (let [v (peek @input-atom)]
    (reset! input-atom (pop @input-atom))
    v))


;; --------------------------------------------------
;; Start Day05-2 routines

(def output-atom (atom 0))

(defn write-output [v]
  (reset! output-atom v))

(defn input []
  (mapv read-string (clojure.string/split (slurp "resources/day07/input.txt") #",")))

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

;; End Day05-2 code
;; ----------------------------------------------------------------------------------------------------

(defn run-amplifier [phase input program]
  (set-input-values [phase input])
  (intcode program)
  @output-atom)

(defn run-intcode [program phases initial-input] 
  (loop [phases phases
         output initial-input]
    (if (empty? phases)
      output
      (recur (rest phases) (run-amplifier (first phases) output program)))))

(def example1 [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])

(def example2 [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0])

(def example3 [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])


(defn run-tests []
  ;; Inputs are first the phase number and second the program input
  ;; 
  (and (= (run-intcode example1 [4 3 2 1 0] 0)
          43210)
       
       (= (run-intcode example2 [0,1,2,3,4] 0)
          54321)
       
       (= (run-intcode example3 [1,0,4,3,2] 0)
          65210)))


;; Try all combinations of phase values
;; [ 0-4, 0-4, 0-4, 0-4, 0-4 ]
;; But,
;; Each phase setting is used exactly once, but the Elves can't remember which amplifier needs which phase setting.

;; @see https://stackoverflow.com/a/26076537/406220
(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    ;; For each item in the collection
    ;; Cons it with the permutations of the rest of the items (with the choosen item removed)
    (for [head colls
          tail (permutations (disj (set colls) head))]   ;; disj set key =? return new set without key
      (cons head tail))))

;; @see https://stackoverflow.com/a/31909140/406220
(defn permutations2 [xs]
  (if (= (count xs) 1)
    (list xs)
    (for [x xs
          y (permutations2 (disj (set xs) x))]
      (map concat y))))


(defn find-phase-settings [program]
  (let [program (input)]
    (apply max (map #(run-intcode program % 0) (permutations (range 0 (inc 4)))))))

(defn part1 []
  (find-phase-settings (input)))

(comment
  (part1)
  ;; => 101490
)
