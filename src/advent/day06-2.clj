(ns advent.day06-2
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))


;; ----------------------------------------------------------------------------------------------------
;; Question input
;; 2TJ)S3Z
;; MJD)YP8
;; 9K5)7Q4
;; HJK)ZKW
;; ...

(defn input []
  (-> "resources/day06/input.txt"
      slurp
      (str/split #"\n")))

(def test-string "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN"
)

(defn test-input []
  (-> test-string
      (str/split  #"\n")))

(defn build-nodes
  "Return each line parted into a vector with two keywords

B)C => [:B C]

[[:COM :B]
 [:B :C]
 [:C :D]
 [:D :E]
 [:E :F]
 [:B :G]
 [:G :H]
 [:D :I]
 [:E :J]
 [:J :K]
 [:K :L]
 [:K :YOU]
 [:I :SAN]]
"
  [strs]
  (mapv #(mapv keyword (str/split % #"\)")) strs))

(defn add-child [m k v]
  (assoc m k (conj (get m k) v)))

(defn insert-node [m pair]
  (let [k (first pair)
        v (second pair)]
    (if (contains? m k)
      (add-child m k v)
      (assoc m k [v]))))

(defn build-tree
  "Read the string into nodes using read-input then convert to our node tree.

Create tree represented as an adjacency list.

  {:I [:SAN],
  :D [:E :I],
  :B [:C :G],
  :J [:K],
  :C [:D],
  :COM [:B],
  :E [:F :J],
  :G [:H],
  :K [:L :YOU]}
"
  [nodes]
  (reduce insert-node {} nodes))

(comment
  (def tree 
    (->> (test-input)
         build-nodes
         build-tree))
)

;;         G - H       J - K - L
;;        /           /
;; COM - B - C - D - E - F
;;                \
;;                 I



;; ----------------------------------------------------------------------------------------------------
;; Part2


;; Find the common parent above YOU and SAN
;; Count the number of paths from the parent to YOU and the parent and SAN
;; Return the total of these two numbers


;; @see https://codereview.stackexchange.com/a/16010
;; Modified by removing keys. Here we have a list of child nodes
(defn- dfs
  [graph goal]
  (fn search
    [path visited]
    (let [current (peek path)]
      (if (= goal current)
        [path]
        (->> current graph
             (remove visited)
             (mapcat #(search (conj path %) (conj visited %))))))))

(defn findpath
  "Returns a lazy sequence of all directed paths from start to goal
  within graph."
  [graph start goal]
  ((dfs graph goal) [start] #{start}))


(comment
  (findpath tree :COM :YOU)
  ;; => ([:COM :B :C :D :E :J :K :YOU])
  (findpath tree :COM :SAN)
  ;; => (findpath tree :COM :SAN)
)

(defn paths-between0
  "Find paths to :YOU AND :SAN. Then figure how many paths are between the two end nodes.
Remove the path to the common parent and count the resulting paths.
"
  [tree]
  (let [p1 (drop-last (first (findpath tree :COM :YOU)))
        p2 (drop-last (first (findpath tree :COM :SAN)))]
    ;; combine two lists while ignoring values in both
    (loop [p1 p1
           p2 p2
           acc []]
      (if (and (empty? p1) (empty? p2))
        acc
        (if (empty? p1)
          (concat acc p2)
          (if (empty? p2)
            (concat acc p1)
            (let [a (first p1)
                  b (first p2)]
              (if (= a b)
                (recur (rest p1) (rest p2) acc)
                (recur (rest p1) (rest p2) (conj acc a b))))))))))


(defn paths-between
  "Find paths to :YOU AND :SAN. Then figure how many paths are between the two end nodes.
Remove the path to the common parent and count the resulting paths.
"
  [tree]
  (let [p1 (drop-last (first (findpath tree :COM :YOU)))
        p2 (drop-last (first (findpath tree :COM :SAN)))]
    (loop [p1 p1
           p2 p2
           acc []]
      (if (or (empty? p1) (empty? p2))
        (concat acc p1 p2)
            (let [a (first p1)
                  b (first p2)]
              (if (= a b)
                (recur (rest p1) (rest p2) acc)
                (recur (rest p1) (rest p2) (conj acc a b))))))))



(defn part2 []
  (let [tree (->> (input)
                  build-nodes
                  build-tree)]
    (count (paths-between tree))))


(comment
  (part2)
  ;; => 361
  )
