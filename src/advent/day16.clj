(ns advent.day16
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))



;; Input list of numbers is the single
;; Each phase, generates a new list with the same length as the input list
;; The new list is used as the input for the next phase



;; Each element is built by multiplying every valuye int he input list by a value in a repeating pattern
;; Then adding up the results



;; input ->    9 8 7 6 5
;;  pattern -> 1 2 3

;; (+ (* 9 1)  (* 8 2) (* 7 3) (* 6 1) (* 5 2)) => 62 => 2


;; Base pattern => 0, 1, 0, -1
;; Repeat each value by the number of the position which is being calculated
;; For second position => 0, 0, 1, 1, 0, 0, -1, -1
;; When appying the pattern, skip the first value once (drop only the very first, allow pattern to repeat normally)


(def base [0 1 0 -1])

(def test-input-str "12345678")

(defn input []
  (str/trim (slurp "resources/day16/input.txt")))


(defn pattern [len pos]
  (drop 1 (take (inc len) (flatten (cycle (map #(repeat pos %) base))))))

(defn abs [n]
  (if (< n 0) (* -1 n) n))

(defn apply-pattern [l pattern]
  ;; for each position
  ;; multiply each position by the other in the same position
  ;; add all the results
  ;; remove negative
  ;; drop all but the singles digit
  (abs (rem (apply + (map * l pattern)) 10)))

(defn calc-pos [l pos]
  (let [p (pattern (count l) pos)]
    ;; apply pattern p to l
    (apply-pattern l p)))

(defn run [l]
  (let [cnt (count l)
        positions (range 1 (inc cnt))]
    (map #(calc-pos l %) positions)))


(defn num-str-to-list [num-str]
  (map #(Integer/parseInt (str %)) num-str))

(defn list-to-num-str [lst]
  (str/join "" (map str lst)))


;; Testing
;; (= "01029498"
;;    (list-to-num (run (run (run (run (num-to-list 12345678)))))))


(defn run-phases [input-num-str phases]
  (loop [phases phases
         num (num-str-to-list input-num-str)]
    (if (zero? phases)
      (list-to-num-str (take 8 num))
      (recur (dec phases) (run num)))))




(defn part1 []
  (run-phases (input) 100))


(comment
  (part1)
  ;; => "50053207"
  )
