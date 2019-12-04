(ns advent.day01)


;; read day01/invput.txt
(defn input []
  (mapv read-string (clojure.string/split (slurp "resources/day01/input.txt") #"\n")))

;; ----------------------------------------------------------------------------------------------------
;; Part 1

;; for each mass calculate the total fuel requirement
;; For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
;; For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
;; For a mass of 1969, the fuel required is 654.
;; For a mass of 100756, the fuel required is 33583.

(defn round-down [n]
  (int (Math/floor n)))

(defn fuel [mass]
  (- (round-down (/ mass 3)) 2))

(defn part1 []
  (apply + (map fuel (input))))

(comment
  (part1) => 3393938
)


;; ----------------------------------------------------------------------------------------------------
;; Part 2

;; calculate fuel-requirements on mass
;; then repeat on the result until 0 or negative

(defn fuel2 [mass]
  (loop [mass mass
        acc 0]
    (let [r (fuel mass)]
      (if (<= r 0)
        acc
        (recur r (+ acc r))))))

(defn part2 []
  (apply + (map fuel2 (input))))



(comment
  (part2) => 5088037
)

