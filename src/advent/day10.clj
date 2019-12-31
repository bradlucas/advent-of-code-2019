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
  (let [get-height-width (fn [m] {:height (count m)
                                  :width (count (first m))})
        {:keys [height width]} (get-height-width m)]
    (for [y (range height)
          x (range width)
          :when (= 1 (get (get m y) x))]
        [x y])))

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

(defn calc-slope [[x1 y1] [x2 y2]]
  ;; (/ (- r2 r1) (- c2 c1)
  (Math/atan2 (- x2 x1) (- y2 y1))
  )

(defn calc-slopes [pos m]
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
        (recur (rest pos) (calc-slopes (first pos) acc))))))


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



;; ----------------------------------------------------------------------------------------------------
;; Part 1 Examples

;; NOTE: Explaination uses cols, rows or X, Y when describint points
;; Lest' change to match it
;; Also, 0,0 is in the upper left

;; The asteroids can be described with X,Y coordinates where X is the
;; distance from the left edge and Y is the distance from the top edge
;; (so the top-left corner is 0,0 and the position immediately to its
;; right is 1,0).


;; So, what I was calling rows is Ys and cols are Xs

(def ex1
".#..#
.....
#####
....#
...##")

(def ex2
"......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")

(def ex3
"#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")

(def ex4
".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")

(def ex5
".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

(defn load-example [ex]
  (let [parse (fn [s] (mapv (fn [c] (if (= c \.) 0 1)) s))]
    (mapv parse (clojure.string/split ex #"\n"))))



(defn test-examples []
  (and
     (= (first (->> ex1 load-example positions build-map process (apply max-key val)))
     [3 4])
  
  (= (first (->> ex2 load-example positions build-map process (apply max-key val)))
     [5 8])
  
  (= (first (->> ex3 load-example positions build-map process (apply max-key val)))
     [1 2])
  
  (= (first (->> ex4 load-example positions build-map process (apply max-key val)))
     [6 3])
  
  (= (first (->> ex5 load-example positions build-map process (apply max-key val)))
     [11 13])
  )
)

;; ----------------------------------------------------------------------------------------------------
;; Part 2


;; IDEA:
;; Find the best position (part1)
;; Find the distance to each of the other positions as well as the slopes
;; Sort the slopes
;; Remove each in turn removing the closes if there are multiple positions on the same slope
;; Iterate till done
;; Which one will be the 200th position to delete?
;;
;; Reviewed:
;; @see https://github.com/transducer/adventofcode/blob/master/src/adventofcode/2019/day10.clj
;; @see https://github.com/erdos/advent-of-code-2019/blob/master/day10.clj


(def ex6
".#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....X...###..
..#.#.....#....##")


(comment
  (= (first (->> ex6 load-example positions build-map process (apply max-key val)))
     [8 3])
)


;; (get-positions (load-example ex5))
;; (get-positions (input))
(defn get-positions [input-data]
  (->> input-data
       positions
       build-map
       process))

(defn get-best-position [m]
  (->> m
       (apply max-key val)
       first))

(defn other-positions [pos m]
  (remove #{pos} (keys m)))

(defn angle [[x1  y1] [xx yy]]
  (-> (Math/atan2  (- xx x1) (- yy y1))
      (-)
      (+ Math/PI)
      (rem (* 2 Math/PI))))

(defn dist [[x1 y1] [x y]]
  (+ (Math/pow (- x x1) 2) (Math/pow (- y y1) 2)))


(defn part2 []
  (let [positions (get-positions (input))
        pos (get-best-position positions)
        positions (other-positions pos positions)]
    (->> positions
         (sort-by (partial dist pos))
         (group-by (partial angle pos))
         (sort-by key)
         (map val)
         (iterate (partial keep next))
         (map (partial keep first))
         (apply concat)
         (#(nth % (dec 200)))
         (#(+ (* 100 (first %)) (second %))))))

