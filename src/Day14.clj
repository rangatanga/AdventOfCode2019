(ns Day14
  (:require [clojure.string :as str])
  (:require [aoc]))


(defn fmt2 [x]
  (let [tmp (str/split x #" ")]
    [(Integer. (first tmp)) (last tmp)])
)

(defn fmt [[x y]]
  [(fmt2 y) (map fmt2 (str/split x #", "))])


(defn instructions [s]
  (aoc/read-file-lines-split s #" =. "))


(defn h [[stack mult] [qty chem]]
  [(assoc stack chem (+ (get stack chem 0) (* mult qty))) mult])

(defn push [stack mult reagents]
  ;;(println stack)
  (first (reduce h [stack mult] reagents)))


(def formula (map fmt (instructions "resources/day14.txt")))


(defn part-one [f]
(loop [stack {"FUEL" f}
       extra {}]
  (let [chem (first (filter #(not= "ORE" %) (keys stack)))]
    (if (nil? chem)
      (get stack "ORE")
      (let [need (get stack chem)
            available (get extra chem 0)
            reagents (first (filter #(= chem (second (first %))) formula))
            reagQty (first (first reagents))]
            (if (>= available need)
              (recur (dissoc stack chem) (assoc extra chem (- available need)))
              (let [need2 (- need available)
                    mult (bigint (Math/ceil (/ need2 reagQty)))
                    excess (- (* mult reagQty) need2)
                    stack2 (push (dissoc stack chem) mult (second reagents))]
                (recur stack2 (assoc extra chem excess)))))))))

;;part-one
(part-one 1)

;;part-two
(loop [i 7659700]
  (if (<= (part-one i) 1000000000000)
    (recur (inc i))
    i))


(part-one 7659732)

