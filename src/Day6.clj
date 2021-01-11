(ns Day6)

(require `[aoc])
(require '[clojure.string :as str])
(require '[clojure.core.reducers :as r])
(require '[clojure.set :as s])

(defn instructions []
  (aoc/read-file-lines-split "resources/day6.txt" #"\)"))

(defn test-instructions []
  (aoc/read-file-lines-split "resources/day6_test.txt" #"\)"))


(defn add-to-map [m [x y]]
  (assoc m y x))

(defn walk-map [m cnt child]
  (let [parent (get m child "")]
  (if (= parent "")
    cnt
    (walk-map m (inc cnt) parent))))

(defn part-one []
  (let [mp (r/reduce add-to-map {} (instructions))
        children (keys mp)]
   (map #(walk-map mp 0 %) children)))

(reduce + (part-one))

(defn walk-map-to-set [m s child]
  (let [parent (get m child "")]
    (if (= parent "")
      s
      (walk-map-to-set m (conj s parent) parent))))

(defn walk-map-until [m s child cnt]
  (let [parent (get m child "")]
    (if (contains? s parent)
      [parent cnt]
      (walk-map-until m s parent (inc cnt)))))

(defn part-two [instr]
  (let [mp (r/reduce add-to-map {} instr)
        sanToCom (walk-map-to-set mp #{} "SAN")
        firstCommonPlanet (walk-map-until mp sanToCom "YOU" 0)
        sanTofirstCommonPlanet (walk-map-until mp (conj #{} (first firstCommonPlanet)) "SAN" 0)
        ]
    (reduce + (map second [firstCommonPlanet sanTofirstCommonPlanet]))))

(part-two (instructions))



