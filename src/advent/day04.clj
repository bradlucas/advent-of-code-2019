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

(defn remove-matching-ex [a b]
  (if (= (last a) b)
    a
    (conj a b)))


(defn remove-matching [l]
  (reduce remove-matching-ex [] l))

(defn is-password [num]
  ;; convert n to a series of integers
  (let [num (str num)
        before (count num)
        num-list (mapv #(Integer/parseInt (str %)) num)
        l (remove-matching num-list)
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



;; Part 2 
;; Matching pair can not be part of a larger goup of matching digits


;; ie 22 is oke
;; but 222 is not unless there is another pair
;; there can be larger than 2 pairs but there must be one pair


(defn increasing-list2 [l]
  (loop [l l]
    (if (= 1 (count l))
      true
      (if (not (<= (first l) (second l)))
        false
        (recur (next l))))))

(defn has-single-pair [pairs]
  (some true? (map (fn [[k v]] (= v 1)) pairs)))

(defn count-occurances [pairs]
  (frequencies pairs))

(defn filter-pairs [parts]
  (filter (fn [[a b]] (= a b)) parts))

(defn partitions [num-list]
  (concat (partition 2 num-list) (partition 2 (rest num-list))))

(defn num-to-list [num]
  (mapv #(Integer/parseInt (str %)) (str num)))

(defn is-password2 [num]
  (let [num-list (num-to-list num)]
    (if (has-single-pair (count-occurances (filter-pairs (partitions num-list))))
      (increasing-list2 num-list))))

(defn part2 []
  (count (filter is-password2 (puzzle-input-range))))



(comment
  ;; advent.day04> (part2)
  ;; 1390
  )
