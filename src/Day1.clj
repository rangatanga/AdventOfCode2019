(ns aoc)

(require `[aoc])

(defn fuel [mass]
  (-> (/ mass 3) Math/floor int (- ,,, 2)))

(defn calc-fuel [fileName f]
  (->> (aoc/read-file-lines-ints fileName) (map #(f %)) (reduce +) ))

(defn fuel-r [mass]
  (let [result (-> (quot mass 3) (- ,,, 2))]
  (if (<= result 0)
    0
    (+ result (fuel-r result)))))

(calc-fuel "resources/day1.txt" fuel)

(calc-fuel "resources/day1.txt" fuel-r)


