(ns advent.day13-2
  (:require [clojure.pprint :as pp]
            [advent.day11 :as intcode]))


(defn input []
  (mapv read-string (clojure.string/split (slurp "resources/day13/input.txt") #",")))


;; ----------------------------------------------------------------------------------------------------
;; Part 2

;; Output
;; Three digits
;; X (distance from left), Y (distance from top), Tile Id
;;
;; Tile Ids:
;; 0 is an empty tile. No game object appears in this tile.
;; 1 is a wall tile. Walls are indestructible barriers.
;; 2 is a block tile. Blocks can be broken by the ball.
;; 3 is a horizontal paddle tile. The paddle is indestructible.
;; 4 is a ball tile. The ball moves diagonally and bounces off objects.

;; Memory Address 0 represents quarters to play
;; Set it to 2 to play for free


;; Joystick
;; Left or right provided by input commands
;;  0 => Neutral
;; -1 => Turn left
;; +1 => Turn right


;; Segment Display
;; X=-1, Y=0, Segment Value

;; ----------------------------------------------------------------------------------------------------


;; @see https://github.com/jkoenig134/AdventOfCode-2019/blob/develop/clojure/Day13.clj
(defn assoc-tile [tiles tile-output]
  (let [x (nth tile-output 0)
        y (nth tile-output 1)
        type (nth tile-output 2)]
    (assoc tiles [x y] type)))

(defn update-tiles [tiles output]
  (reduce assoc-tile {} (partition 3 output)))

(def tile-type
  {:empty 0
   :wall 1
   :block 2
   :paddle 3
   :ball 4})

(defn find-tiles [tiles type]
  ;; 0 => empty
  ;; 1 => wall
  ;; 2 => block
  ;; 3 => paddle
  ;; 4 => ball
  (filter #(= (tile-type type) (second %)) tiles))

(defn find-tile [tiles type]
  (first (first (find-tiles tiles type))))

(defn joystick-direction [ball paddle]
  ;; tilt up or down, or stay neutral?
  (let [r (compare (first ball) (first paddle))]
    r))


;; Run intcode till it needs more input or halts
;; Had to remove Day11's bit where it exits when there are two items in the output queue
;; Also, refactored the parameter order to math other routines below
(defn intcode [lst idx]
  ;; Run program until it needs more input or halts
  (loop [lst lst
         idx idx]
    (let [optcode (intcode/get-optcode (first (intcode/get-nth lst idx)))]
      (let [empty-input (empty? @intcode/in)
            [idx lst] (case optcode
                        1 (intcode/opt1 idx lst)
                        2 (intcode/opt2 idx lst)
                        3 (intcode/opt3 idx lst)
                        4 (intcode/opt4 idx lst)
                        5 (intcode/opt5 idx lst)
                        6 (intcode/opt6 idx lst)
                        7 (intcode/opt7 idx lst)
                        8 (intcode/opt8 idx lst)
                        9 (intcode/opt9 idx lst)
                        99 (intcode/opt99 idx lst))]
        (if (or (= optcode 99) 
                (and (= optcode 3) empty-input))
          [lst idx]
          (recur lst idx))))))

;; Save idx and program between runs
;; Extract output queue for tiles
(defn run [program idx input]
  (if (not (nil? input))
    (intcode/set-input input))

  (let [[program idx] (intcode program idx)
        state (vec @intcode/out)]
    (reset! intcode/out (clojure.lang.PersistentQueue/EMPTY))
    [program idx state]))


(defn play [program]
  (let [program (assoc program 0 2)]   ;; set mem 0 to 2 so we play for free
  (loop [[program idx state] (run program 0 nil)
         tiles (update-tiles {} state)]
    (if (= idx -1)
      (tiles [-1 0])
      ;; Where is the ball?
      ;; Where it the paddle?
      ;; How should the joy stick be titled
      ;; Run with next state
      (let [ball-pos (find-tile tiles :ball)
            paddle-pos (find-tile tiles :paddle)
            joystick (joystick-direction ball-pos paddle-pos)
            [program idx next-state] (run program idx joystick)]
      (recur [program idx next-state] (update-tiles tiles next-state)))))))


(defn part2 []
  (reset! intcode/base 0)
  (reset! intcode/in (clojure.lang.PersistentQueue/EMPTY))
  (reset! intcode/out (clojure.lang.PersistentQueue/EMPTY))
  (play (input)))


(comment
  (part2)
  ;; => 10776
  )

