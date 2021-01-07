(ns Day2)

(require `[aoc])

(defn instructions []
  (zipmap (range) (map #(Integer. %) (aoc/read-file-split "resources/day2.txt" #","))))

(defn get-val [m i]
  (get m i))

(defn add-instr [m i] 
  (+ (get m (get-val m (+ i 1))) (get m (get-val m (+ i 2)))))

(defn mult-instr [m i]
  (* (get m (get-val m (+ i 1))) (get m (get-val m (+ i 2)))))


(defn apply-instruction [m i]
  (let [curVal (get m i)]
    (if (= curVal 99)
      (get m 0)
      (case (get m i)
        1 (apply-instruction (assoc m (get-val m (+ i 3)) (add-instr m i)) (+ i 4))
        2 (apply-instruction (assoc m (get-val m (+ i 3)) (mult-instr m i)) (+ i 4))
    ))))


(apply-instruction (zipmap (range) [2,4,4,5,99,0]) 0)

(apply-instruction (assoc (assoc (instructions) 1 12) 2 2) 0)


(defn upd-instr [noun verb]
  (assoc (assoc (instructions) 1 noun) 2 verb)
)

(defn part-two []
  (loop [n 0
         v 0]
    (if (= 19690720 (apply-instruction (upd-instr n v) 0))
      (+ v (* 100 n))
      ;;(vector n v (get (upd-instr n v) 0) (get (upd-instr n v) 1) (get (upd-instr n v) 2))
      (if (= v 99)
        (recur (inc n) 0)
        (recur n (inc v)))))
  )

(part-two)

(apply-instruction (upd-instr 70 14) 0)