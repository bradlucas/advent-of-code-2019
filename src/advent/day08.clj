(ns advent.day08
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))


(def example "123456789012")

(defn input []
  (str/trim (slurp "resources/day08/input.txt")))

(defn convert-string-to-digits
  "Take a string of numbers and return a list of those numbers

(convert-string-to-digits \"12345\")
=> [1 2 3 4 5]
"
  [s]
  (mapv #(read-string (str %)) s))

(defn partition-input
  "Partition the input into width number of pixels by height number of pixels.

For example, given an image 3 pixels wide and 2 pixels tall, the image data 123456789012 corresponds to the following image layers:

Layer 1: 123
         456

Layer 2: 789
         012

(partition-input \"123456789012\" 3 2)
=> (((1 2 3) (4 5 6)) ((7 8 9) (0 1 2)))

"
  [s width height]
  (partition height (partition width (convert-string-to-digits s))))


;; ----------------------------------------------------------------------------------------------------
;; Find the layer with the fewest 0 digits
;; on that layer what is the number of 1 digits multiple by the number of 2 digits

(comment
;; Example layer from input
((2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 0 2 2)
 (2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 0 2 2 2 2 2 2 2 2)
 (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 0 0 2 0 2 2 0)
 (2 1 1 0 2 2 2 0 2 2 1 2 2 0 0 2 0 0 2 2 2 2 0 2 0)
 (2 2 2 2 2 2 0 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2)
 (1 2 2 0 2 2 0 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2))
)

(defn process
  "Flatten the layer to a list and count the occurances of each digit.

(process '((2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 0 2 2)
 (2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 0 2 2 2 2 2 2 2 2)
 (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 0 0 2 0 2 2 0)
 (2 1 1 0 2 2 2 0 2 2 1 2 2 0 0 2 0 0 2 2 2 2 0 2 0)
 (2 2 2 2 2 2 0 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2)
 (1 2 2 0 2 2 0 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)))
=> {2 125, 1 8, 0 17}
"
  [layer]
  (frequencies (flatten layer)))

(defn min-map [a b]
  (if (> (get a 0) (get b 0))
    b
    a))

(defn part1
  "Read the input into layers, process them to get digit counts.
Then find the layer with the smallest number of 0s and for that layer
return the number of 1s multiplied by the number of 2s."
  []
  (let [lst (map process (partition-input (input) 25 6))]
    (let [m (reduce min-map lst)]
      (* (get m 1) (get m 2)))))



;; ----------------------------------------------------------------------------------------------------
;; Part 2



(defn get-data-layers []
  (partition-input (input) 25 6))

(defn get-pos [layer row col]
  (nth (nth layer row) col))

(defn get-pixel-data [data row col]
  (map #(get-pos % row col) data))

(defn get-value [pixel-data]
  ;; 0 is black and 1 is white while 2 is transparent
  ;; return the first 0 or 1 value found else return 2
  (let [found (filter (fn [v] (or (= v 0) (= v 1))) pixel-data)]
    (if (not (empty? found))
      (first found)
      2)))

(defn build-image []
  (let [layers (get-data-layers)
        rows (count (first layers))
        cols (count (first (first layers)))]

    (map (fn [row] 
           (map (fn [col] 
                  ;; (get-value (get-pixel-data data row col))
                  ;; [row col]
                  {:row row
                   :col col
                   :value (get-value (get-pixel-data layers row col))}
                  ) (range cols))) (range rows))
    ))

(defn print-image [i]
  (map #(map :value %) i))


(defn part2 []
  (print-image (build-image)))


(comment

((1 1 1 0 0 1 0 0 1 0 1 1 1 0 0 1 1 1 1 0 1 0 0 1 0)
 (1 0 0 1 0 1 0 0 1 0 1 0 0 1 0 1 0 0 0 0 1 0 0 1 0)
 (1 0 0 1 0 1 1 1 1 0 1 0 0 1 0 1 1 1 0 0 1 0 0 1 0)
 (1 1 1 0 0 1 0 0 1 0 1 1 1 0 0 1 0 0 0 0 1 0 0 1 0)
 (1 0 0 0 0 1 0 0 1 0 1 0 0 0 0 1 0 0 0 0 1 0 0 1 0)
 (1 0 0 0 0 1 0 0 1 0 1 0 0 0 0 1 1 1 1 0 0 1 1 0 0))


((1 1 1     1     1   1 1 1     1 1 1 1   1     1  )
 (1     1   1     1   1     1   1         1     1  )
 (1     1   1 1 1 1   1     1   1 1 1     1     1  )
 (1 1 1     1     1   1 1 1     1         1     1  )
 (1         1     1   1         1         1     1  )
 (1         1     1   1         1 1 1 1     1 1    ))


=> PHPEU
)
