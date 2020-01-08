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




;; ----------------------------------------------------------------------------------------------------
;; Part 2

;; How many steps till a state is repeated


;; Brute force, doesn't return
(defn repeat-til-duplicate [moons]
  (loop [seen #{}
         cnt 0
         moons moons]
    (if (seen moons)
      cnt
      (recur (conj seen moons) (inc cnt) (step moons)))))

;; h/t @see https://github.com/Average-user/adventofcode-clj-2019/blob/master/src/adventofcode_clj_2019/day12.clj
;; h/t @see https://github.com/stubillwhite/advent-of-code-2019/blob/master/src/advent_of_code_2019/day_12.clj
;; h/t @see  https://github.com/ackerleytng/2019-advent-of-code/blob/master/12-nbody/core.clj

;; Find the steps for a cycle for each axis
;; then calculate the lcm for the three

(defn- gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))
 
(defn- lcm [a b]
  (/ (* a b) (gcd a b)))

(defn xs [m]
  (map (fn [{:keys [position velocity]}] [(first position) (first velocity)]) m))

(defn ys [m]
  (map (fn [{:keys [position velocity]}] [(second position) (second velocity)]) m))

(defn zs [m]
  (map (fn [{:keys [position velocity]}] [(nth position 2) (nth velocity 2)]) m))

(defn repeat-til-duplicate-axis [moons axis-fn]
  (loop [seen #{}
         cnt 0
         moons moons]
    (let [v (axis-fn moons)]
      (if (seen v)
      cnt
      (recur (conj seen v) (inc cnt) (step moons))))))


;; advent.day12> (repeat-til-duplicate-axis xs m)
;; 268296
;; advent.day12> (repeat-til-duplicate-axis ys m)
;; 113028
;; advent.day12> (repeat-til-duplicate-axis zs m)
;; 231614

;; advent.day12> (reduce lcm [*1 *2 *3])
;; 292653556339368

(defn part2 []
  (let [m (moons (input))]
    (reduce lcm (map (partial repeat-til-duplicate-axis m) [xs ys zs]))))



(comment
  (part2)
  ;; => 292653556339368
)



