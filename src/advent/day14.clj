(ns advent.day14
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

;; ----------------------------------------------------------------------------------------------------
;; Used the following:
;; @see https://github.com/transducer/adventofcode/blob/master/src/adventofcode/2019/day14.clj
;; @see https://gitlab.com/dmarjenburgh/adventofcode/blob/master/src/adventofcode/year_2019.clj


(def input-str (slurp "resources/day14/input.txt"))


;; Parse input data reversing reactions

(defn- parse-unit [s]
  (let [[amount name] (str/split s #" ")
        amount (Integer/parseInt amount)]
    {(keyword name)
     amount}))

(defn- parse-reaction [s]
  (let [[input output] (map str/trim (str/split s #"=>"))
        reactants (into {} (map parse-unit (map str/trim (str/split input #","))))
        [amount name] (str/split output #" ")]
    {(keyword name)
     {:amount (Integer/parseInt amount)
      :reactants reactants}}))

(defn load-reactions [s]
  (into {} (map parse-reaction (str/split s #"\n"))))



;; ----------------------------------------------------------------------------------------------------

(defn scale-reaction
  "For a given reaction scale the amount and it's reactants amounts"
  [reactions name multiplier]
  (update reactions name (fn [m]
                           {:amount (* (:amount m) multiplier)
                            :reactants (reduce-kv (fn [acc k v]
                                                    (assoc acc k (* v multiplier)))
                                                  {}
                                                  (:reactants m))})))


(defn find-ore-amount [reactions amount-of-fuel]
  (let [reactions (scale-reaction reactions :FUEL amount-of-fuel)]


    (loop [reactants (get-in reactions [:FUEL :reactants])
           stash {}
           ore 0]

      (if (seq reactants)
        ;; reactants
        (let [[name value] (first reactants)]
          ;; Does name have reactants?
          (if (reactions name)
            (let [amount-required (get-in reactions [name :amount])
                  next-reactants (get-in reactions [name :reactants])
                  num-required (max (long (Math/ceil (/ (- value (stash name 0)) amount-required))) 0)
                  amount-created (* num-required amount-required)]
              (recur (reduce-kv (fn [acc k v]
                                  (update acc k (fnil + 0) (* num-required v)))
                                (dissoc reactants name)
                                next-reactants)
                     (update stash name (fnil + 0) (- amount-created value))
                     ore))
            ;; else, we have an :ORE
            (recur (dissoc reactants name) stash (+ ore value)))
          )
        ;; else, no more reactants
        ore
        ))))



;; ----------------------------------------------------------------------------------------------------
;; Tests


(def test-input-str1 "10 ORE => 10 A
7 A => 1 FUEL")

(def test-input-str2 "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

(def test-input-str3 "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
1 E, A 7 => 1 FUEL")

(def test-input-str4 "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")

(def test-input-str5 "9 ORE => 2 A
8 ORE => 3 B
3 A, 4 B => 1 AB
2 AB => 1 FUEL")


(comment 
  ;; test-input-str2
  ;; (reactions test-input-str2)
  {:A {:amount 10, :reactants {:ORE 10}},
   :B {:amount 1, :reactants {:ORE 1}},
   :C {:amount 1, :reactants {:A 7, :B 1}},
   :D {:amount 1, :reactants {:A 7, :C 1}},
   :E {:amount 1, :reactants {:A 7, :D 1}},
   :FUEL {:amount 1, :reactants {:A 7, :E 1}}}
  )

(defn tests []
  (and

     (= 31 (find-ore-amount (load-reactions "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL
") 1))

     (= 165 (find-ore-amount (load-reactions "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL") 1))
     
     (= 13312 (find-ore-amount (load-reactions "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT") 1))

     (= 180697 (find-ore-amount (load-reactions "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF") 1))

     (= 2210736 (find-ore-amount (load-reactions "171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX") 1))

))


;; ----------------------------------------------------------------------------------------------------
;; Part 1

(defn part1 []
  (find-ore-amount (load-reactions input-str) 1))

(comment
  (part1
   ;; => 202617
   ))




;; ----------------------------------------------------------------------------------------------------
;; Part 2

;; You have a trillion (1000000000000) units of ORE how much FUEL can you produce
;; 


(defn find-ore-amount-2 [num]
  (find-ore-amount (load-reactions input-str) num))


(defn bi-sect [fnc target]
  (loop [a 1
         b 1.0e9]
    ;; find middle (bi-sect) between a and b
    (if (< a b)
      (let [num (long (+ (/ (- b a) 2) a))]
        (if (> (fnc num) target)
          (recur a num)
          (recur (inc num) b)))
      (dec a))))


(defn part2 []
  (let [trillion 1000000000000]
    (bi-sect find-ore-amount-2 trillion)))


(comment
  (part2)
  ;; => 7863863
  )
