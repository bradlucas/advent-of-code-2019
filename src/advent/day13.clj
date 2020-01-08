(ns advent.day13
  (:require [clojure.pprint :as pp]
            [advent.day09 :as intcode]
            ))



(defn input []
  (mapv read-string (clojure.string/split (slurp "resources/day13/input.txt") #",")))


;; (intcode/intcode (input))
;; (count (filter #(= 2 %) (map #(nth % 2) (partition 3 @intcode/q))))
;; => 228


(defn part1 []
  (reset! intcode/base 0)
  (reset! intcode/q (clojure.lang.PersistentQueue/EMPTY))
  (intcode/intcode (input))
  (count (filter #(= 2 %) (map #(nth % 2) (partition 3 @intcode/q)))))


(comment
  (part1)
  ;; => 228
  )
