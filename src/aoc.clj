(ns aoc)

(defn strings-to-ints [fileName]
  (map #(Integer. %) (-> fileName slurp clojure.string/split-lines)))
