(ns advent.day07-2
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
;; ----------------------------------------------------------------------------------------------------

(defn log [s]
  ;;(pp/pprint s)
  )

;; --------------------------------------------------

;; Part 2 - Input and Output share a queue

(def q (atom (clojure.lang.PersistentQueue/EMPTY)))

(defn push-queue [v]
  (reset! q (conj @q v)))

(defn pop-queue []
  (let [v (peek @q)]
    (reset! q (pop @q))
    v))

(defn set-input-values [l]
  (log (format "set-input-values %s" l))
  (doseq [v l] (push-queue v)))

(defn read-input []
  (let [v (pop-queue)]
    (do
      (log (format "read-input %s" v))
      v)))

(defn write-output [v]
  (log (format "write-output %s" v))
  (push-queue v))


(defn prepend-queue
  "Put the value at the head of the line"
  [v]
  (let [q1 (clojure.lang.PersistentQueue/EMPTY)]
    (loop [q1 (conj q1 v)
           v (pop-queue)]
      (if (nil? v)
        (reset! q q1)
        (recur (conj q1 v) (pop-queue))))))

;; Start Day05-2 routines


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
    (log (format "%s %s %s %s" optcode a b result))
    (log (format "%s %s" param1 param2))
    [(+ idx 4)
     (if (= m3 1)
       (log "Opt1 with immediate mode for result")
       (assoc lst result value))]))

(defn opt2 [idx lst]
  (log (format "opt2 %s %s" idx lst))
  (let [[optcode a b result] (take 4 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        param1 (if (= m1 1) a (nth lst a))
        param2 (if (= m2 1) b (nth lst b))
        value (* param1 param2)]
    (log (format "%s %s %s %s" optcode a b result))
    (log (format "%s %s" param1 param2))
    [(+ idx 4)
     (if (= m3 1)
       (log "Opt2 with immediate mode for result")
       (assoc lst result value))]))

(defn opt3 
  "Read the input and store location of parameter"
  [idx lst]
  ;; read input
  (let [[optcode a] (take 2 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        ]
    (log (format "%s %s" optcode a))
    ;; (log (format "%s" m2))
    [(+ idx 2)
     (if (= m1 1)
       (log "Opt3 with immediate mode for result")
       (do
         ;; (log (format "opt3 idx = %s" idx))
         (assoc lst a (read-input))))]))

(defn opt4
  "Output the value at or of the parameter"
  [idx lst]
  ;; write output
  ;; (log (format "opt4 - %s %s" idx lst))
  (let [[optcode a] (take 2 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        val (if (= m1 1) a (nth lst a))
        ]
    (log (format "%s %s" optcode a))
    ;;(log (format "%s" m1))
    [(+ idx 2)
     (if (= m2 1)
       (log "Opt3 with immediate mode for result")
       (do
         ;; (log (format "Writing %s to output" val))
         (write-output val)
         lst))]))

(defn opt5
  ;; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
  [idx lst]
  (log (format "opt5 - %s %s" idx lst))
  (let [[optcode a b] (take 3 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        param1 (if (= m1 1) a (nth lst a))
        param2 (if (= m2 1) b (nth lst b))
        ]
    (log param1)
    (log param2)
    (log (format "%s %s %s" optcode a b))
    (log (format "%s %s" param1 param2))

    (let [idx2 (if (not= param1 0) param2 (+ idx 3))]
      [idx2
       lst])))

(defn opt6
  ;; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
  [idx lst]
  (let [[optcode a b] (take 3 (nthrest lst idx))
        [m3 m2 m1] (get-parameter-modes optcode)
        param1 (if (= m1 1) a (nth lst a))
        param2 (if (= m2 1) b (nth lst b))
        idx2 (if (= param1 0) param2 (+ idx 3))]
    (log (format "%s %s %s" optcode a b))
    ;;(log (format "%s %s" param1 param2))
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
    (log (format "%s %s %s %s" optcode a b c))
    ;;(log (format "%s %s 5s" param1 param2 param3))
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
    (log (format "%s %s %s %s" optcode a b c))
    ;;(log (format "%s %s %s" param1 param2 param3))
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

;; Part 1 version
;; (defn intcode [lst]
;;   (loop [idx 0
;;          lst lst]
;;     (log (format "intcode - %s - %s" idx lst))
;;     (let [optcode (get-optcode (nth lst idx))]
;;       (log (format "intcode - optcode %s - %s" optcode lst))
;;       (let [[idx lst] (case optcode
;;                         1 (opt1 idx lst)
;;                         2 (opt2 idx lst)
;;                         3 (opt3 idx lst)
;;                         4 (opt4 idx lst)
;;                         5 (opt5 idx lst)
;;                         6 (opt6 idx lst)
;;                         7 (opt7 idx lst)
;;                         8 (opt8 idx lst)
;;                         99 (opt99 idx lst))]
;;         ;; Part 2
;;         ;; Pause after processing opt4 output statement
;;         (if (not (= optcode 4))
;;           (recur idx lst)
;;           )))))

;; End Day05-2 code
;; ----------------------------------------------------------------------------------------------------


;; ----------------------------------------------------------------------------------------------------
;; Part 2

(def example4 [[3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] [9,8,7,6,5]])

(def example5 [[3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10] [9,7,8,5,6]])


;; @see https://www.reddit.com/r/adventofcode/comments/e7aqcb/2019_day_7_part_2_confused_with_the_question/
;; Each amplififier needs to maintain state (it's instruction pointer)
;; When an amplifier reachs optcod4 it outputs then halts (after incrementing it's ip pointer)
;; The whole cycle ends when Amp E hits it's 99 statments
;; Looks like you set the phase only once


;; Each amplifier needs it's own state which is it's instruction pointer
;; The amplifier will read it's phase only on the first run
;; It will pause after processing the output statement
;; The halt statement will end the processing
;; Send input to Amp A only once


;; ----------------------------------------------------------------------------------------------------
;; example4
;;
;; | 0 |  1 |    2 |  3 |  4 |  5 | 6 |  7 |    8 |  9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 |   18 | 19 | 20 | 21 |   22 | 23 | 24 | 25 | 26 | 27 | 28 |
;; |---+----+------+----+----+----+---+----+------+----+----+----+----+----+----+----+----+----+------+----+----+----+------+----+----+----+----+----+----+
;; | 3 | 26 | 1001 | 26 | -4 | 26 | 3 | 27 | 1002 | 27 |  2 | 27 |  1 | 27 | 26 | 27 |  4 | 27 | 1001 | 28 | -1 | 28 | 1005 | 28 |  6 | 99 |  0 |  0 |  5 |
;; |   |    |      |    |    |    |   |    |      |    |    |    |    |    |    |    |    |    |      |    |    |    |      |    |    |    |    |    |    |
;;
;;
;;
;; |------+----+--------------------------------------------------------------------------------------------|
;; |    3 |  0 | Read input and store in position 26                                                        |
;; |   26 |    |                                                                                            |
;; |------+----+--------------------------------------------------------------------------------------------|
;; | 1001 |  2 | Subtract 4 from the value in 26 and store back in 26                                       |
;; |   26 |    |                                                                                            |
;; |   -4 |    |                                                                                            |
;; |   26 |    |                                                                                            |
;; |------+----+--------------------------------------------------------------------------------------------|
;; |    3 |  6 | Read input and store in 27                                                                 |
;; |   27 |    |                                                                                            |
;; |------+----+--------------------------------------------------------------------------------------------|
;; | 1002 |  8 | Multiple value at 27 by 2 and store in position 27                                         |
;; |   27 |    |                                                                                            |
;; |    2 |    |                                                                                            |
;; |   27 |    |                                                                                            |
;; |------+----+--------------------------------------------------------------------------------------------|
;; |    1 | 12 | Add the values at positions 26 and 27 and store in position 27                             |
;; |   27 |    |                                                                                            |
;; |   26 |    |                                                                                            |
;; |   27 |    |                                                                                            |
;; |------+----+--------------------------------------------------------------------------------------------|
;; |    4 | 16 | Write value at 27 to output                                                                |
;; |   27 |    |                                                                                            |
;; |------+----+--------------------------------------------------------------------------------------------|
;; | 1001 | 18 | Subtract 1 from the value at 28 and store in position 28                                   |
;; |   28 |    |                                                                                            |
;; |   -1 |    |                                                                                            |
;; |   28 |    |                                                                                            |
;; |------+----+--------------------------------------------------------------------------------------------|
;; | 1005 | 22 | If the value at 28 is positive set the instruction pointer to the paramter. In this case 6 |
;; |   28 |    |                                                                                            |
;; |    6 |    |                                                                                            |
;; |------+----+--------------------------------------------------------------------------------------------|
;; |   99 |    | Halt program                                                                               |
;; |------+----+--------------------------------------------------------------------------------------------|
;; |    0 | 26 | Phase value                                                                                |
;; |    0 | 27 | Input                                                                                      |
;; |    5 | 28 | Loop counter                                                                               |
;; |      |    |                                                                                            |
;; |------+----+--------------------------------------------------------------------------------------------|



(def pipeline (atom nil))


(defn amp [program phase name]
  {:program program
   :phase phase
   :idx 0
   :run-count 0
   :halt false
   :name name
   })

(defn create-pipeline [[program phases]]
  (reset! pipeline {:a (amp program (nth phases 0) :a)
                    :b (amp program (nth phases 1) :b)
                    :c (amp program (nth phases 2) :c)
                    :d (amp program (nth phases 3) :d)
                    :e (amp program (nth phases 4) :e)}))

(defn update-pipeline [name value]
  (log (format "update-pipeline %s - %s" name value))
  (reset! pipeline (assoc @pipeline name value)))

(defn intcode [{:keys [run-count idx program] :as m}]
  (log (format "intcode amplifier %s, %s" (:name m) m))
  ;; The first time an amplifier runs, you need to put the phase on the input queue
  (if (= run-count 0)
    (do
      (log (format "Running node %s for the first time, adding a phase of %s" (:name m) (:phase m)))
      (prepend-queue (:phase m))
      ;; If we are on the first run of the first Amplifier, add 0 (the input) to the input queue
      (if (= (:name m) :a)
        (do
          (log (format "Adding initial input to queue for node %s" (:name m)))
          (push-queue 0)))
      ))
  (log "--------------------------------------------------")
  (loop [idx idx
         lst program]
    ;; (log (format "%s - %s" idx lst))
    (let [optcode (get-optcode (nth lst idx))]
      (log (format "optcode %s - %s" optcode lst))
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
        ;; opt4 is the output statement
        ;; pause by returning
        ;; opt99 is the halt statement
        ;; end
        (if (or (= optcode 4) (= optcode 99))
          (do
            (log (format "Amplifier %s - optcode 4 idx = %s" (:name m) idx))
            (assoc m :run-count (inc run-count) :program lst :idx idx :halt (= optcode 99)))
          (recur idx lst))))))

(defn run [example]
  (create-pipeline example)
  (loop [names (cycle [:a :b :c :d :e])]
    (let [name (first names)
          amp (get @pipeline name)]
      (let [r (intcode amp)]
        (log r)
        (if (:halt r)
          (pop-queue)
          (do
            (update-pipeline name r)
            (recur (next names))))))))
 


;; @see https://stackoverflow.com/a/26076537/406220
(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    ;; For each item in the collection
    ;; Cons it with the permutations of the rest of the items (with the choosen item removed)
    (for [head colls
          tail (permutations (disj (set colls) head))]   ;; disj set key =? return new set without key
      (cons head tail))))

(defn find-phase-settings [program]
  (let [program (input)]
    (apply max (map #(run [program %]) (permutations (range 5 (inc 9)))))))

(defn part2 []
  (find-phase-settings (input)))


(comment
  (part2)
  ;; => 61019896
)
