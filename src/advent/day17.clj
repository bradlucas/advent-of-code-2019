(ns advent.day17
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [advent.day13-2 :as intcode]))


(defn input []
  (mapv read-string (clojure.string/split (slurp "resources/day17/input.txt") #",")))

;; -------------
;; Scaffold is #

;; ..#..........
;; ..#..........
;; #######...###
;; #.#...#...#.#
;; #############
;; ..#...#...#..
;; ..#####...^..


;; ------------------
;; Find intersections

;; ..#..........
;; ..#..........
;; ##O####...###
;; #.#...#...#.#
;; ##0###O###O##
;; ..#...#...#..
;; ..#####...^..


;; (into #{} @intcode/out)
;; #{46 35 10 94}

;; 35 => #
;; 46 => .
;; 10 => \n
;; 

(defn run-program []
  (advent.day11/reset-queues)
  (let [[code idx] (intcode/intcode (input) 0)]
    @advent.day11/out))

(defn build-string-map [r]
  (str/join (map #(char %) r)))

;; @see https://github.com/fdlk/advent-2019/blob/master/src/advent_2019/day17.clj
(defn build-map [s]
  (into {} (map-indexed (fn [y row]
                          ;; process row
                          (reduce
                           (fn [m [pos value]]
                             (assoc m pos value))
                           {}
                           (map-indexed (fn [x value] [[x y] value]) row)))
                        ;; rows
                        (str/split s #"\n"))))

(defn neighbors [[x1 y1]]
  (map (fn [[x y]] [(+ x x1) (+ y y1)]) [[0 1] [0 -1] [1 0] [-1 0]]))

(defn is-intersection [m p]
  ;; pos should have a \#
  (if (= \# (get m p))
    ;; and all it's neighors should be \#
    (let [n (neighbors p)
          vals (map #(get m %) n)]
      (every? #(= \# %) vals))))

(defn find-intersections [map]
  (filter #(is-intersection map %) (keys map)))



;; (reduce + (map #(apply * %) (find-intersections (part1))))


(defn part1 []
  (->> (run-program)
      build-string-map
      build-map
      find-intersections
      (map #(apply * %))
      (reduce +)))


(comment
  (part1)
  ;; => 6520
  )




