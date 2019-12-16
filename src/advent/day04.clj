(ns advent.day04
  (:require [clojure.pprint :as pp]))


;; Rules
;;
;; Six-digit number
;; Value ranges between the given range
;; Two adjacent digits are the same
;; Going from left to right, the digits never decrease

;; In other words
;; The digits always increase when moving left to right
;; except when a number repeats
;; Also, there needs to be at least one pair of matching numbers

;; 111111 is OK
;; 112345 is OK
;; 223450 is not OK




;; Your puzzle input is 128392-643281.

;; 128392 643281
;; ()

(defn increasing-list [l]
  (loop [l l]
    (if (= 1 (count l))
      true
      (if (not (< (first l) (second l)))
        false
        (recur (next l))))))

(defn de-pair [a b]
  (if (= (last a) b)
    a
    (conj a b)))


(defn is-password [num]
  ;; convert n to a series of integers
  (let [num (str num)
        before (count num)
        l (reduce de-pair [] (mapv #(Integer/parseInt (str %)) num))
        after (count l)]
    ;; (pp/pprint num)
    ;; (pp/pprint before)
    ;; (pp/pprint l)
    ;; (pp/pprint after)
    ;; it needs to be shorter other return false
    (if (= before after)
      false                   ;; no duplicate
      ;; ensure all integers increase
      (increasing-list l))))

(defn puzzle-input-range []
  (range 128392 643281))


(defn part1 []
  (count (filter is-password (puzzle-input-range))))


(comment
  ;; advent.day04> (part1)
  ;; 2050
)
