(ns advent.day06
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
K)L")

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
 [:K :L]]
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

  {:COM [:B],
   :B [:C :G],
   :C [:D],
   :D [:E :I],
   :E [:F :J],
   :G [:H],
   :J [:K],
   :K [:L]}

"
  [nodes]
  (reduce insert-node {} nodes))


(def tree (build-tree (build-nodes (test-input))))

;;         G - H       J - K - L
;;        /           /
;; COM - B - C - D - E - F
;;                \
;;                 I



;; Get all the nodes in the tree
;; For each do a search and count the number of steps to reach the node


(defn tree-nodes [tree]
  (flatten (vals tree)))


(defn children
  "Returns the sequence of neighbors for the given node"
  [v coll]
  (get coll v))

(defn graph-bfs
  "Traverses a graph in Breadth First Search (BFS)."
  [graph v]
  (loop [queue   (conj clojure.lang.PersistentQueue/EMPTY [v 0]) ;; Use a queue to store the nodes we need to explore
         visited []                                          ;; A vector to store the sequence of visited nodes
         ]                                         
    (if (empty? queue)
      visited                               ;; Base case - return visited nodes if the queue is empty
        (let [[v level]   (peek queue)
              children    (children v graph)
              new-queue   (apply conj (pop queue) (mapv (fn [v] [v (inc level)]) children))]
          (recur new-queue (conj visited [v level]))))))



(defn part1 []
  (apply + (map second (graph-bfs (build-tree (build-nodes (input))) :COM))))

(comment
  (part1)
  ;;=> 295834
)
