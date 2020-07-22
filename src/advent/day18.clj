(ns advent.day18
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))


;; Lower case letters are keys and are required to have in your possession to enter doors which are the upper case of the same
;; # are walls
;; Initial position (entrance) is @
;; Open passages are marked with .


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

(defn input []
  (->> (slurp "resources/day18/input.txt")
       build-map))


;; Find entrance
(defn entrance [m]
  (first (filter #(= \@ (second %)) m)))

(defn neighbors [m [x1 y1]]
  (map (fn [[x y]] [(+ x x1) (+ y y1)]) [[0 1] [0 -1] [1 0] [-1 0]]))

;; (defn not-wall? [m pos]
;;   (not= \# (get m pos)))


(defn open-positions [m]
  (let [not-wall? (fn [pos] (not= \# (get m pos)))]
    (filter #(not-wall? %) (keys m))))



;; Find keys
(defn find-keys [m]
  (let [lower-case-letter? (fn [c] (let [c (int c)]
                                     (and (>= c (int \a))
                                          (<= c (int \z)))))]
    (filter #(lower-case-letter? %) (vals m))))




;; Start at entrance
;; Get list of keys
;; Find each of the keys

