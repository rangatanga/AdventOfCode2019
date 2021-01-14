(ns Day10)

(require '[aoc])
(require '[clojure.string :as str])
(require '[clojure.core.reducers :as r])


(defn chart []
  (vec (aoc/read-file-lines-split "resources/day10.txt" #"")))

(-> (chart) (nth ,,, 0) (nth ,,, 1))

(defn is-asteroid [[l c] [x y]]
  (if (= "#" (get (get c y) x))
    [(conj l [x y]) c]
    [l c]))

(defn get-grad [[x y] [u v]]
  (let [grad (cond
               (= u x) (if (< y v) -99 99)
               (and (< x u) (< y v)) (+ -99 (/ (- u x) (- v y)))
               (= v y) (if (< x u) 0 999)
               (and (< x u) (> y v)) (/ (- u x) (- v y))
               (and (> x u) (> y v)) (+ 99 (/ (- u x) (- v y)))
               (and (> x u) (< y v)) (+ 999 (/ (- u x) (- v y)))
               :else (/ (- u x) (- v y)))]
    grad
    ))

(defn get-grads [coords [x y]]
  [[x y] (count (distinct (map #(get-grad [x y] %) (disj coords [x y]))))])


(defn get-grad-dist [[x y] [u v]]
  (let [grad (cond
               (= u x) (if (> y v) 0 999)
               (and (< x u) (> y v)) (/ (- u x) (- y v))
               (= v y) (if (< x u) 99 9999)
               (and (< x u) (< y v)) (+ 99 (/ (- v y) (- u x)))
               (and (> x u) (< y v)) (+ 999 (/ (- x u) (- v y)))
               (and (> x u) (> y v)) (+ 9999 (/ (- y v) (- x u)))
               )]
    {:ast [u v] :grad grad :dist (aoc/manhat-dist [(- x u) (- y v)])}))

;;part-one
(let [ch (chart)
      coords (for [x (range (count (first ch))) y (range (count ch))] [x y])
      ast-coords (first (r/reduce is-asteroid [#{} ch] coords))]
  (reverse (sort-by last (map #(get-grads ast-coords %) ast-coords)  )))


;;part-two
(let [ch (chart) 
      coords (for [x (range (count (first ch))) y (range (count ch))] [x y])
      ast-coords (first (r/reduce is-asteroid [#{} ch] coords))
      x 37 ;;11
      y 25 ;;13
      data (into [] (sort-by (juxt :grad :dist) (map #(get-grad-dist [x y] %) (disj ast-coords [x y]))))
      ]
  (loop [cnt 0
         i 0
         cur {}
         s #{}]
    (let [j (if (= i (count data)) 0 i)]
      ;;(println cnt i cur)
      (if (= 200 cnt)
        cur
        (if (or (= (:grad cur -888) (:grad (data j))) ;;if grad of next item is same as current move to next
                (contains? s (:ast (data j))))         ;;or if we've already seen the asterisk
          (recur cnt (inc i) cur s )
          (recur (inc cnt) (inc j) (data j) (conj s (:ast (data j)))))))))

