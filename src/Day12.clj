(ns Day12)

(require '[aoc])
(require '[clojure.string :as str])


(defn get-vel [x1 x2]
  (cond
    (< x1 x2) 1
    (> x1 x2) -1
    :else 0))

(defn get-vels [u v]
  (let [x (get-vel (:x u) (:x v))
        y (get-vel (:y u) (:y v))
        z (get-vel (:z u) (:z v))
        ]
    {:id (:id u) :x x :y y :z z}))


(defn sum-vect [u v]
  {:id (:id u) :x (+ (:x u) (:x v)) :y (+ (:y u) (:y v)) :z (+ (:z u) (:z v))}
  )

(defn calc-vels [ps moon]
  (reduce sum-vect {:id (:id moon) :x 0 :y 0 :z 0} (map #(get-vels moon %) ps)))

(defn g [vs1 v2]
  (sum-vect v2 (first (filter #(= (:id v2) (:id %)) vs1)))) 

(defn sum-vects [vs1 vs2]
  (map #(g vs1 %) vs2))

(defn abs-vect [v]
  {:id (:id v) :e (+ (aoc/abs (:x v)) (aoc/abs (:y v)) (aoc/abs (:z v)))})

(defn mult-energy [vs u]
  (* (:e u) (:e (first (filter #(= (:id u) (:id %)) vs)))))

;;part-one
(loop [positions [{:id 1 :x -1 :y 7 :z 3}
                  {:id 2 :x 12 :y 2 :z -13}
                  {:id 3 :x 14 :y 18 :z -8}
                  {:id 4 :x 17 :y 4 :z -4}]
       vels [{:id 1 :x 0 :y 0 :z 0}
             {:id 2 :x 0 :y 0 :z 0}
             {:id 3 :x 0 :y 0 :z 0}
             {:id 4 :x 0 :y 0 :z 0}]
       cnt 0]
  (if (= cnt 1000)
    (let [pe (map #(abs-vect %) positions)
          ke (map #(abs-vect %) vels)
          es (map #(mult-energy pe %) ke)]
      (apply + es))
    (let [newvels (sum-vects vels (map #(calc-vels positions %) positions))
          newpos (sum-vects newvels positions)]
      (recur newpos newvels (inc cnt)))))


(defn comp-vect [vs x]
  (println x)
  (println (first (filter #(= (:id x) (:id %)) vs)))
  (= x (first (filter #(= (:id x) (:id %)) vs)))
  )

{:pos [-1 2 4 3] :vel [0 0 0 0]}




(defn calc-grav [ps p]
  (apply + (map get-vel (repeat 4 p) ps)))


(defn part-two [v]
  (loop [pos v
         vel [0 0 0 0]
         cnt 1]
    (let [grav (map #(calc-grav pos %) pos)
          newvel (map + vel grav)
          newpos (map + pos newvel)]
      ;;(println pos vel)
      (if (and (> cnt 1) (= pos v))
        (println cnt)
        (recur newpos newvel (inc cnt))))))


(part-two [-1 12 14 17]) 
(part-two [7 2 18 4])
(part-two [3 -13 -8 -4]) 

(aoc/lcmv 231614 96236 144624)

