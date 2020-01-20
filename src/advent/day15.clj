(ns advent.day15
  (:require [clojure.pprint :as pp]
            [advent.day13-2 :as intcode]
            ))


;; ----------------------------------------------------------------------------------------------------
;; Accept a movement commant via n input instruction
;; Send the movement command to the repair droid
;; Wait for the repair droid to finish the movement operation
;; Report on the status or the repair droid via an output instruction

;; ----------------------------------------------------------------------------------------------------
;; Movement commands 
;; North => 1
;; South => 2
;; West => 3
;; East => 4

;; ----------------------------------------------------------------------------------------------------
;; Status Codes
;; 0 => Droid hit a wall. Its position has not changed
;; 1 => Droid moved one step in the requested direction
;; 2 => Droid moved one step in the requested direction; its new position is the location of the oxygen system

;; Find shortest path to the Oxygen system
;; Breadth First Search till you find the Oxygen

(defn input []
  (mapv read-string (clojure.string/split (slurp "resources/day15/input.txt") #",")))


(defn run
  "Run the intcode program from day13 part 2. Accept the current program state (program and idx) and input.
Return the resulting program state (program and idx) and output.
  
program state is accepted as:
  {:code code
   :idx idx}"
  [{:keys [code idx] :as state} input]
  ;; (intcode/run program idx input)  => program idx state/output
  (let [[code idx output] (intcode/run code idx input)]
    {:code code
      :idx idx
      :output (first output)}))

(defn directions
  "The cells which surround the given position.

  North => 1
  South => 2
  West => 3
  East => 4

  +x is to the right
  +y is down
"
  [[x y]]
  [[1 [x (- y 1)]] ;; north
   [2 [x (+ y 1)]] ;; south
   [3 [(- x 1) y]] ;; west
   [4 [(+ x 1) y]] ;; east
   ])

(comment
  (directions [0 0])
  [[1 [0 -1]] [2 [0 1]] [3 [-1 0]] [4 [1 0]]]
)


(defn next-steps
  "Try all directions from the current position and return the ones which allow moves.
"
  [{:keys [code idx pos output] :as state}]
  (filter #(not= 0 (:output %)) (map (fn [[command pos]] (let [{:keys [code idx output]} (run state command)]
                                                           {:code code
                                                            :idx idx
                                                            :pos pos
                                                            :output output})) (directions pos))))


;; @see https://github.com/fdlk/advent-2019/blob/master/src/advent_2019/day15.clj
(defn breadth-first-search [initial-state]
  (loop [visited #{}
         max-depth 0
         todo (list [0 initial-state])]
    (if (empty? todo)
      [max-depth visited]
      (let [[depth state] (first todo)
            pos (:pos state)
            new-visited (conj visited pos)]
        (if (= 2 (:output state))
          [depth state]
          (if (visited pos)
            (recur new-visited max-depth (rest todo))
            (recur new-visited (max max-depth depth) (concat (rest todo) (map (fn [x] [(inc depth) x]) (next-steps state))))))))))


;; :pos [0 0]
;;               

(def initial-state {:code (input)
                    :idx 0
                    :pos [0 0]
                    :output 0
                    })



(defn part1 []
  (first (breadth-first-search initial-state)))


(comment
  (part1)
  ;; => 204
  )



;; ----------------------------------------------------------------------------------------------------
;; Part 2


;; Find the oxygen system like in part1. Then each minute the oxygen spreads in each direction to adjacent locations.
;; How many minutes will it take to fill the available region with oxygen

;; Start
;;  ##   
;; #..## 
;; #.#..#
;; #.O.# 
;;  ###  

;; End
;;  ##   
;; #OO## 
;; #O#OO#
;; #OOO# 
;;  ###  

;; Build grid of available positions
;; Start at the final (oxygen system) point
;; Count steps to fill region

;; Breadth First Search starting at the final state from Part 1
;; Done when you run out of nodes

;; @see https://github.com/fdlk/advent-2019/blob/master/src/advent_2019/day15.clj

(defn breadth-first-search-2 [initial-state]
  (loop [visited #{}
         max-depth 0
         todo (list [0 initial-state])]
    (if (empty? todo)
      [max-depth visited]
      (let [[depth state] (first todo)
            pos (:pos state)
            new-visited (conj visited pos)]
          (if (visited pos)
            (recur new-visited max-depth (rest todo))
            (recur new-visited (max max-depth depth) (concat (rest todo) (map (fn [x] [(inc depth) x]) (next-steps state)))))))))


(defn part2 []
  (let [[_ state] (breadth-first-search initial-state)]
    (first (breadth-first-search-2 state))))


(comment
  (part2)
  ;; => 
  )
