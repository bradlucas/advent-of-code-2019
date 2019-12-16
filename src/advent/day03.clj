(ns advent.day03
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

;; read day03/invput.txt
(defn input []
  (clojure.string/split (slurp "resources/day03/input.txt") #"\n"))

;; Build list of positions
;; Find the intersection point closest to the central port
;; What is the Manhattan distrance from the central port to the closet intersection
;; Central port is the starting point [0, 0]
;; Manhattan distance
;; The sum of the horizontal and vertical distances between points on a grid

(defn right [node]
  (let [[x y] node]
    [(inc x) y]))

(defn left [node]
  (let [[x y] node]
    [(dec x) y]))

(defn up [node]
  (let [[x y] node]
    [x (inc y)]))

(defn down [node]
  (let [[x y] node]
    [x (dec y)]))

(defn parse-command [c]
  (case (keyword c)
    :R right
    :L left
    :U up
    :D down))

(defn parse [c] 
  (let [dir (parse-command (str (first c)))
        value (Integer/parseInt (apply str (rest c)))]
    {:direction dir
     :value value}))

(defn parse-commands [s]
  (let [cmds (str/split s #",")
        parse-command (fn [c]
                        (case (keyword c)
                          :R right
                          :L left
                          :U up
                          :D down))
        parse (fn [c] 
                (let [dir (parse-command (str (first c)))
                      value (Integer/parseInt (apply str (rest c)))]
                  {:direction dir
                   :value value}))]
        (map parse cmds)))


(defn move
  "(move [[0 0]] right 3)
[[0 0] [1 0] [2 0] [3 0]]
"
  [positions direction cnt]
  (loop [cnt cnt
         acc positions]
    (if (zero? cnt)
      acc
      (recur (dec cnt) (conj acc (direction (last acc)))))))

(defn apply-command
  "(apply-command [[0 0]] {:direction right :value 3})
[[0 0] [1 0] [2 0] [3 0]]
"
  [positions {:keys [direction value] :as cmd}]
  (move positions direction value))

(defn apply-commands

  [initial-pos commands]
  (loop [commands commands
         positions [initial-pos]]
    (if (empty? commands)
      positions
      (recur (next commands) (apply-command positions (first commands))))))

(defn build-path
  [instructions]
  (let [cmds (parse-commands instructions)]
    ;; note: remove [0 0] from returned set
    (into #{} (rest (apply-commands [0 0] cmds)))))


(defn build-paths [input-strings]
  (loop [instructions input-strings
         paths []]
    (if (empty? instructions)
      paths
      (recur (rest instructions) (conj paths (build-path (first instructions)))))))

(defn find-intersections [[a b]]
  (clojure.set/intersection  a b))


;; Find shortest Manhattan distance
(defn abs [x]
  (Math/abs x))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(def s #{[0 0] [155 11] [146 46] [155 4] [158 -12]})

;; Remove 00
(defn find-min-of-distances [l]
    (map #(manhattan-distance [0 0] %) l))


(defn part1 [input]
  (let [paths (build-paths input)]
    (first (sort (find-min-of-distances (find-intersections paths))))))


(def test-str "R3,U3,L3
U3,R3") 

(defn test-input []
  (str/split test-str #"\n"))

(defn example1 []
  (let [s "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"]
    (str/split s #"\n"))) 

(defn example2 []
  (let [s "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
    (str/split s #"\n")))


(comment
 (part1 (example1))
 ;; => 159

 (part1 (example2))
 ;; =? 155
)


(comment
  ;; Part1
  (time (part1 (input)))
  ;; "Elapsed time: 619439.425533 msecs"
  ;; 870

)



;; ----------------------------------------------------------------------------------------------------
;; Part 1


;; Minimize the number of stesp

;; Calculate the number of steps each wire takes to reach each intersection
;; Choose the intersection where the sum of both wires's steps is the lowest


;; Keep the paths
;; Find the intersections
;; For each calculate the number of steps to each intersection
;; Return the minimum


(defn build-path2
  [instructions]
  (let [cmds (parse-commands instructions)]
    ;; ignore the starting node
    (rest (apply-commands [0 0] cmds))))

(defn build-paths2 [input-strings]
  (loop [instructions input-strings
         paths []]
    (if (empty? instructions)
      paths
      (recur (rest instructions) (conj paths (build-path2 (first instructions)))))))


(defn steps-til-node 
  " Count the number nodes it will take to find node

  (steps-til-node [[0 0] [0 1] [0 2] [0 3]] [0 3])
  => 3
"
  [path node]
  (loop [path path
         cnt 1]  ;; because we ignored the starting node earlier
    (if (or (empty? path) (= (first path) node))
      cnt
      (recur (next path) (inc cnt)))))

(defn node-steps [node [p1 p2]]
  (let [len1 (steps-til-node p1 node)
        len2 (steps-til-node p2 node)
        total (+ len1 len2)]
    {:node node
     :len1 len1
     :len2 len2
     :total total}))


(defn part2 [input]
  (let [paths (build-paths2 input)
        intersections (find-intersections [(into #{} (first paths)) (into #{} (second paths))])]
    (:total (apply min-key :total (map #(node-steps % paths) intersections)))))



(comment
  (part2 (example1))
  ;; => {:node [158 -12], :len1 206, :len2 404, :total 610}
  ;; => 610

  (part2 (example2))
  ;; => {:node [107 47], :len1 154, :len2 256, :total 410}
  ;; =< 410


  (time (part2 (input)))
  ;; "Elapsed time: 610393.163737 msecs"
  ;; 13698
)
