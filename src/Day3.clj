(ns Day3)
(require '[aoc])
(require '[clojure.string :as str])
(require '[clojure.core.reducers :as r])
(require '[clojure.set :as s])


(defn instructions []
  (aoc/read-file-lines-split "resources/day3.txt" #",")
)

(defn gen-coords [x y d instr]
  (let [dir (subs instr 0 1)
        val (-> (subs instr 1) (Integer.) (+ 1))]
    (case dir
      "L" (for [i (range 1 val)]
            [[(- x i) y] (+ d i)])
      "R" (for [i (range 1 val)]
            [[(+ x i) y] (+ d i)])
      "U" (for [i (range 1 val)]
            [[x (+ y i)] (+ d i)])
      "D" (for [i (range 1 val)]
            [[x (- y i)] (+ d i)])
      )
  )
)

(defn add-coords [[x y d s] i]
  (let [coords (gen-coords x y d i)
        finalPos (last coords)]
    [(-> finalPos first first) (-> finalPos first last) (last finalPos) (into s coords)])
)

(defn part-one []
  (let [route1 (r/reduce add-coords [0 0 0 #{}] (-> (instructions) first))
        route2 (r/reduce add-coords [0 0 0 #{}] (-> (instructions) rest first))
        commonCoords (s/intersection (->> (last route1) (map first) set) (->> (last route2) (map first) set))]
    (->> (map aoc/manhat-dist commonCoords) (apply min))
  )
)

(part-one)


(defn combined-path [r1 r2 c]
  (let [r1d (-> (filter #(= (first %) c) r1) last)
        r2d (-> (filter #(= (first %) c) r2) last)]
    (if (or (empty? r1d) (empty? r2d))
      0
      (+ (last r1d) (last r2d))
      )
    )
)


(defn part-two []
  (let [route1 (r/reduce add-coords [0 0 0 #{}] (-> (instructions) first))
        route2 (r/reduce add-coords [0 0 0 #{}] (-> (instructions) rest first))
        commonCoords (s/intersection (->> (last route1) (map first) set) (->> (last route2) (map first) set))]
    (->> (map #(combined-path (last route1) (last route2) %) commonCoords) (apply min))
    ))

(part-two)
