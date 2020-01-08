(ns advent.day12
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

;; h/t @see https://github.com/jkoenig134/AdventOfCode-2019/blob/develop/clojure/Day12.clj


;; <x=-4, y=3, z=15>
;; <x=-11, y=-10, z=13>
;; <x=2, y=2, z=18>
;; <x=7, y=-1, z=0>


(defn input []
  (let [parse (fn [s] (mapv read-string (re-seq #"-?\d+" s)))]
    (mapv parse (clojure.string/split (slurp "resources/day12/input.txt") #"\n"))))

(defn moons [in]
  (map (fn [p] {:position p :velocity [0 0 0]}) in))


;; Simulate the motion of the moons in time steps.
;; For each step, 
;; Update the velocity of every moon by applying gravity
;; Then update the position of each moon by applying velocity

;; Gravity
;; Consider every pair of moons

(defn add-vector [& vectors]
  (apply map + vectors))

(defn update-position [moon]
  (update moon :position add-vector (:velocity moon)))

(defn apply-gravity [moon1 moon2]
  (update moon1 :velocity add-vector (map compare (:position moon2) (:position moon1))))

(defn step [moons]
  ;; apply-gravity for each moon with each of the other moons
  (let [moons' (map #(reduce apply-gravity % moons) moons)]
    (doall (map update-position moons'))))

(defn simulate [moons]
  (iterate step moons))

(defn total-energy-of-moon [moon]
  (let [potential (apply + (map #(Math/abs %) (:position moon)))  ;; potential-energy abs of x, y, z
        kinetic (apply + (map #(Math/abs %) (:velocity moon))) ;; kinetic-energy abs of velocity valuesxs
        ]
    (* potential kinetic)))


;; ----------------------------------------------------------------------------------------------------
;; Part 1

;; What is the total energy in the system after simulating the moons igven your scan for 1000 steps

(defn total-energy-in-system []
  (let [moons (nth (simulate (moons (input))) 1000)]
    (apply + (map total-energy-of-moon moons))))


(defn part1 []
  (total-energy-in-system))


(comment
  (part1)
  ;; => 7722
  )


