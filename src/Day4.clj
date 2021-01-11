(ns Day4)

(require '[aoc])
(require '[clojure.string :as str])
(require '[clojure.core.reducers :as r])
;;(require '[clojure.set :as s])


(defn chk-asc [[b x] y]
  (if (and b (>= y x))
    [b y]
    [false 10]))

(defn is-dupe [[b x] y]
  (if (or b (= x y))
    [true y]
    [false y]))

(defn check-num [[cnt] x]
  (let [xx (aoc/int-to-vec (str x))]
    (if (and (first (r/reduce chk-asc [true 1] xx)) (first (r/reduce is-dupe [false 0] xx)))
      [(inc cnt)]
      [cnt])
    ))

;;part-one
(r/reduce check-num [0] (range 147998 691423))

;;part-two

(defn is-dupe2 [x]
  (= 2 (some #{2} (map count (aoc/group  x)))))

(defn check-num2 [[cnt] x]
  (let [xx (aoc/int-to-vec (str x))]
    (if (and (first (r/reduce chk-asc [true 1] xx)) (is-dupe2 xx))
      [(inc cnt)]
      [cnt])))

(r/reduce check-num2 [0] (range 147998 691423))



  
