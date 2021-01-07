(ns aoc)
(require `[clojure.string])

(defn read-file [fileName]
  (-> fileName slurp clojure.string/trim-newline))

(defn read-file-lines [fileName]
  (-> fileName slurp clojure.string/split-lines))

(defn read-file-lines-ints [fileName]
  (map #(Integer. %) (aoc/read-file-lines fileName)))

(defn read-file-split [fileName delim]
  (-> (aoc/read-file fileName) (clojure.string/split ,,, delim)))

(defn read-file-lines-split [fileName delim]
  (map #(clojure.string/split % delim) (aoc/read-file-lines fileName)))


(-> (aoc/read-file "resources/day2.txt") (clojure.string/split ,,, #","))


(defn abs [n] (max n (-' n)))

(defn manhat-dist [[x y]]
  (+ (aoc/abs x) (aoc/abs y)))
