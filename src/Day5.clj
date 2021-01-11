(ns Day5)
(require `[aoc])
(require '[clojure.string :as str])

(defn instructions []
  (zipmap (range) (map #(Integer. %) (aoc/read-file-split "resources/day5.txt" #","))))

(defn test-instructions [x]
  (zipmap (range) (map #(Integer. %) (str/split x #","))))

(defn get-val [m i]
  (get m (get m i)))

(defn get-val-imm [m i]
  (get m i))

(defn get-val-by-mode [m i mode]
  (if (= mode 0)
    (get-val m i)
    (get-val-imm m i)))

(defn add-instr [m i p1 p2]
  (+ (get-val-by-mode m (+ i 1) p1) (get-val-by-mode m (+ i 2) p2)))

(defn mult-instr [m i p1 p2]
  (* (get-val-by-mode m (+ i 1) p1) (get-val-by-mode m (+ i 2) p2)))

(defn apply-instruction [m i]
  (let [curVal (-> (get m i) aoc/int-to-vec)
        p2 (if (= (count curVal) 3)
             0
             (first curVal))
        p1 (if (= (count curVal) 3)
             (first curVal)
             (first (rest curVal)))
        instr (if (= (count curVal) 1)
                (last curVal)
                (* 10 (last curVal)))]
    (do (println curVal " " i)
        (case instr
          3 (apply-instruction (assoc m (get-val-imm m (+ i 1)) 5) (+ 2 i))
          4 (do
            ;;(println i)
              (println (get m (get-val-imm m (+ i 1))))
              (if (= 0 (get m (get-val-imm m (+ i 1))))
                (apply-instruction m (+ 2 i))
                (apply-instruction m (+ 2 i))))
          1 (apply-instruction (assoc m (get-val-imm m (+ i 3)) (add-instr m i 0 0)) (+ i 4))
          2 (apply-instruction (assoc m (get-val-imm m (+ i 3)) (mult-instr m i 0 0)) (+ i 4))
          5 (if (not= 0 (get-val-by-mode m (+ 1 i) 0))
              (apply-instruction m (get-val-by-mode m (+ 2 i) 0))
              (apply-instruction m (+ 3 i)))
          6 (if (= 0 (get-val-by-mode m (+ 1 i) 0))
              (apply-instruction m (get-val-by-mode m (+ 2 i) 0))
              (apply-instruction m (+ 3 i)))
          7 (if (< (get-val-by-mode m (+ 1 i) 0) (get-val-by-mode m (+ 2 i) 0))
              (apply-instruction (assoc m (get-val-imm m (+ i 3)) 1) (+ i 4))
              (apply-instruction (assoc m (get-val-imm m (+ i 3)) 0) (+ i 4)))
          8 (if (= (get-val-by-mode m (+ 1 i) 0) (get-val-by-mode m (+ 2 i) 0))
              (apply-instruction (assoc m (get-val-imm m (+ i 3)) 1) (+ i 4))
              (apply-instruction (assoc m (get-val-imm m (+ i 3)) 0) (+ i 4)))
          10 (apply-instruction (assoc m (get-val-imm m (+ i 3)) (add-instr m i p1 p2)) (+ i 4))
          20 (apply-instruction (assoc m (get-val-imm m (+ i 3)) (mult-instr m i p1 p2)) (+ i 4))
          40 (do
               (println (get-val-by-mode m (+ i 1) p1))
               (if (= 0 (get-val-by-mode m (+ i 1) p1))
                 (apply-instruction m (+ 2 i))
                 (apply-instruction m (+ 2 i))))
          50 (if (not= 0 (get-val-by-mode m (+ 1 i) p1))
               (apply-instruction m (get-val-by-mode m (+ 2 i) p2))
               (apply-instruction m (+ 3 i)))
          60 (if (= 0 (get-val-by-mode m (+ 1 i) p1))
               (apply-instruction m (get-val-by-mode m (+ 2 i) p2))
               (apply-instruction m (+ 3 i)))
          70 (if (< (get-val-by-mode m (+ 1 i) p1) (get-val-by-mode m (+ 2 i) p2))
               (apply-instruction (assoc m (get-val-imm m (+ i 3)) 1) (+ i 4))
               (apply-instruction (assoc m (get-val-imm m (+ i 3)) 0) (+ i 4)))
          80 (if (= (get-val-by-mode m (+ 1 i) p1) (get-val-by-mode m (+ 2 i) p2))
               (apply-instruction (assoc m (get-val-imm m (+ i 3)) 1) (+ i 4))
               (apply-instruction (assoc m (get-val-imm m (+ i 3)) 0) (+ i 4)))
          90 (do
               (println "halt"))
          (println curVal)))))

(apply-instruction (instructions) 0)

(apply-instruction (test-instructions "3,9,8,9,10,9,4,9,99,-1,8") 0)

(test-instructions "3,9,8,9,10,9,4,9,99,-1,8")
