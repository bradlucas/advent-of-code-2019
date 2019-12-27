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
  (let [lst (map process (partition-input (input) 6 25))]
    (let [m (reduce min-map lst)]
      (* (get m 1) (get m 2)))))
