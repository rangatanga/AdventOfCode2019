(ns Day8)

(require '[aoc])
(require '[clojure.string :as str])


(def width 25)
(def height 6)
(def wxh (* width height))

(defn gen-layer [data]
  ;;(map vec (partition width data))
  (vec data)
  )

(defn get-layers [layers data]
  (if (empty? data)
    layers
    (get-layers (conj layers (gen-layer (take wxh data))) (drop wxh data))
    ))

(defn cnt [layer]
  (let [zeroes (-> (filter #(= 0 %) layer) count)
        ones (-> (filter #(= 1 %) layer) count)
        twos (-> (filter #(= 2 %) layer) count)
        ]
    [zeroes (* ones twos)]))

(defn layer-eval [[x y] [u v]]
  (if (< u x)
    [u v]
    [x y]))

;;(defn part-one []
  (let [data (-> (aoc/read-file "resources/day8.txt") aoc/int-to-vec)
        layers (get-layers #{} data)]
    (reduce  layer-eval [100 0] (map cnt layers))
  
    )
;;)

;;part-two
(defn f [[x data] i]
  (let [v (->> (map #(nth % i) data) (drop-while #(= 2 %)))]
    [(conj x (first v)) data]
  )
)


(let [data (-> (aoc/read-file "resources/day8.txt") aoc/int-to-vec)
      layers (get-layers [] data)]
  (partition width (first (reduce f [[] layers] (range (- wxh 0)))))
  ;;(count (first (reduce f [[] layers] (range (- wxh 0)))))
)
