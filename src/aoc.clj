(ns aoc)
(require '[clojure.string :as str])
(require '[clojure.core.reducers :as r])

(defn read-file [fileName]
  (-> fileName slurp str/trim-newline))

(defn read-file-lines [fileName]
  (-> fileName slurp clojure.string/split-lines))

(defn read-file-lines-ints [fileName]
  (map #(Integer. %) (aoc/read-file-lines fileName)))

(defn read-file-split [fileName delim]
  (-> (aoc/read-file fileName) (str/split ,,, delim)))

(defn read-file-lines-split [fileName delim]
  (map #(clojure.string/split % delim) (aoc/read-file-lines fileName)))


(defn abs [n] (max n (-' n)))

(defn manhat-dist [[x y]]
  (+ (aoc/abs x) (aoc/abs y)))

(defn group [xs]
  (defn add-or-append [v x]
    (let [dlv (vec (drop-last v))
          clv (vec (conj (last v) x))]
      (if (= x (first (last v)))
        (vec (conj dlv clv))
        (conj v [x]))))
  (r/reduce add-or-append [] xs))

(defn int-to-vec [x]
  (let [vx  (str/split (str x) #"")]
    (vec (map #(Integer. %) vx))))

