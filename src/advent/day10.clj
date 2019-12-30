(ns advent.day10
  (:require [clojure.pprint :as pp]))


;; Hint: find slope to each #
;; If a second # has the same slope it can't be seen

;; For each asteroid calculate the number of asteroids it can 'see'
;; Return the one with the highest number

;; read input

;; build data structure

;; calculate number seen for each

;; return the one with the max

;; ----------------------------------------------------------------------------------------------------

(defn input []
  (let [parse (fn [s] (mapv (fn [c] (if (= c \.) 0 1)) s))]
    (mapv parse (clojure.string/split (slurp "resources/day10/input.txt") #"\n"))))


(defn positions
  "Build sparse matrix of asteroid locations

([0 4] [0 8] [0 9] [0 10] [0 11] [0 13] [0 15] [0 27] [1 0] [1 1] ...)
"
  [m]
  (let [get-rows-cols (fn [m] {:rows (count m)
                               :cols (count (first m))})
        {:keys [rows cols]} (get-rows-cols m)]
    (for [row (range rows)
          col (range cols)
          :when (= 1 (get (get m row) col))]
        [row col])))

(defn build-map
  "Build a map of the positions where the position is the key.
"
  [positions]
  (loop [pos positions
         acc {}]
    (if (empty? pos)
      acc
      (recur (rest pos) (into acc {(first pos) 0})))))

;; For each position calculate the line of site to each other position
;; Store the number of positions that are visible (not hidden by another position) 
;; Return the position with the highest number of visible positions

(defn calc-slope [[r1 c1] [r2 c2]]
  ;; (/ (- r2 r1) (- c2 c1)
  (Math/atan2 (- r2 r1) (- c2 c1))
  )

(defn calc-positions [pos m]
  (let [others (remove #{pos} (keys m))]
    ;; find the slope to reach from pos to each of the others
    ;; slope = (y2 - y1) / (x2 -1)

    (let [slopes (into #{} (map #(calc-slope pos %) others))]

    ;; count the number of unique slopes
    ;; update m's pos value with this value and return m
    (assoc m pos (count slopes)))))

(defn process [m]
  (let [lst (keys m)]
    (loop [pos lst
           acc m]
      (if (empty? pos)
        acc
        (recur (rest pos) (calc-positions (first pos) acc))))))


(defn part1 []
  (->> (input)
       positions
       build-map
       process
       (apply max-key val)
       val))


(comment
  (part1)
  ;;=> 329
  )

