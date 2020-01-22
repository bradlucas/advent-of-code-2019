>(ns advent.day17
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




;; ----------------------------------------------------------------------------------------------------
;; Part 2

(defn display-map []
  (str/split (->> (run-program)
                  build-string-map) #"\n"))


(comment
  ["................................#####.............."
   "................................#...#.............."
   "................................#...#.............."
   "................................#...#.............."
   "................................#...#.............."
   "................................#...#.............."
   "..........................###########.............."
   "..........................#.....#.................."
   "..........................#.....#.................."
   "..........................#.....#.................."
   "..........................#.....#.................."
   "..........................#.....#.................."
   "......................###########.................."
   "......................#...#........................"
   "......................#...#........................"
   "......................#...#........................"
   "......................#...#########................"
   "......................#...........#................"
   "..........#...........#...........#................"
   "..........#...........#...........#................"
   "..........#...........#####.......#................"
   "..........#...............#.......#................"
   "..........#...............#.......#................"
   "..........#...............#.......#................"
   "..........#...........#######.....#................"
   "..........#...........#...#.#.....#................"
   "..........#.........#############.#...############^"
   "..........#.........#.#...#.#...#.#...#............"
   "..........#####.....#.#############...#............"
   "..............#.....#.....#.#...#.....#............"
   "..............#.....#.....#######.....#............"
   "..............#.....#.......#.........#............"
   "..............#.....#.......#.........#............"
   "..............#.....#.......#.........#............"
   "..............#.....#.......###########............"
   "..............#.....#.............................."
   "....###########.....#.............................."
   "....#...............#.............................."
   "....#.....#########.#########......................"
   "....#.....#.......#.........#......................"
   "....#.....#.......#.........#......................"
   "....#.....#.......#.........#......................"
   "###########.......#.........#......................"
   "#...#.............#.........#......................"
   "#...#.............#.........#......................"
   "#...#.............#.........#......................"
   "#...#.............#.........#......................"
   "#...#.............#.........#......................"
   "#####.............###########......................"]
)

(comment
;; Figure out the path manually

A      L 12, L 8, R 10, R 10,
B      L 6, L 4, L 12,
A      L 12, L 8, R 10, R 10,
B      L 6, L 4, L 12,
C      R 10, L 8, L 4, R 10,
B      L 6, L 4, L 12,
A      L 12, L 8, R 10, R 10,
C      R 10, L 8, L 4, R 10,
B      L 6, L 4, L 12,
C      R 10, L 8, L 4, R 10

"A,B,A,B,C,B,A,C,B,C\n,L,12,L,8,R,10,R,10\nL,6,L,4,L,12\nR,10,L,8,L,4,R,10\n"
)

(def solution (map int "A,B,A,B,C,B,A,C,B,C\nL,12,L,8,R,10,R,10\nL,6,L,4,L,12\nR,10,L,8,L,4,R,10\nn\n"))

(defn part2 []
  (advent.day11/reset-queues)
  (reset! advent.day11/in (reduce conj clojure.lang.PersistentQueue/EMPTY solution))
  (let [[code idx] (intcode/intcode (assoc (input) 0 2) 0)]
    (last @advent.day11/out)))


(comment
  (part2)
  ;; => 1071369
  )
