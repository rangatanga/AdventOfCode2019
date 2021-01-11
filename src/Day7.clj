(ns Day7)

(require `[aoc])
(require '[clojure.string :as str])

(defn instructions []
  (zipmap (range) (map #(Integer. %) (aoc/read-file-split "resources/day7.txt" #","))))

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

(defn apply-instruction [m i inp outp]
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
    (do ;;(println curVal " " i)
        (case instr
          3 (apply-instruction (assoc m (get-val-imm m (+ i 1)) (first inp)) (+ 2 i) [(last inp) 0] outp)
          4 ;;(do
            ;;(println i)
              ;;(println (get m (get-val-imm m (+ i 1))))
              (apply-instruction m (+ 2 i) inp (get m (get-val-imm m (+ i 1))))
          ;;)
          1 (apply-instruction (assoc m (get-val-imm m (+ i 3)) (add-instr m i 0 0)) (+ i 4) inp outp)
          2 (apply-instruction (assoc m (get-val-imm m (+ i 3)) (mult-instr m i 0 0)) (+ i 4) inp outp)
          5 (if (not= 0 (get-val-by-mode m (+ 1 i) 0))
              (apply-instruction m (get-val-by-mode m (+ 2 i) 0) inp outp)
              (apply-instruction m (+ 3 i) inp outp))
          6 (if (= 0 (get-val-by-mode m (+ 1 i) 0))
              (apply-instruction m (get-val-by-mode m (+ 2 i) 0) inp outp)
              (apply-instruction m (+ 3 i) inp outp))
          7 (if (< (get-val-by-mode m (+ 1 i) 0) (get-val-by-mode m (+ 2 i) 0))
              (apply-instruction (assoc m (get-val-imm m (+ i 3)) 1) (+ i 4) inp outp)
              (apply-instruction (assoc m (get-val-imm m (+ i 3)) 0) (+ i 4) inp outp))
          8 (if (= (get-val-by-mode m (+ 1 i) 0) (get-val-by-mode m (+ 2 i) 0))
              (apply-instruction (assoc m (get-val-imm m (+ i 3)) 1) (+ i 4) inp outp)
              (apply-instruction (assoc m (get-val-imm m (+ i 3)) 0) (+ i 4) inp outp))
          10 (apply-instruction (assoc m (get-val-imm m (+ i 3)) (add-instr m i p1 p2)) (+ i 4) inp outp)
          20 (apply-instruction (assoc m (get-val-imm m (+ i 3)) (mult-instr m i p1 p2)) (+ i 4) inp outp)
          40 ;;(do
               ;;(println (get-val-by-mode m (+ i 1) p1))
               (apply-instruction m (+ 2 i) inp (get-val-by-mode m (+ i 1) p1))
          ;;)
          50 (if (not= 0 (get-val-by-mode m (+ 1 i) p1))
               (apply-instruction m (get-val-by-mode m (+ 2 i) p2) inp outp)
               (apply-instruction m (+ 3 i) inp outp))
          60 (if (= 0 (get-val-by-mode m (+ 1 i) p1))
               (apply-instruction m (get-val-by-mode m (+ 2 i) p2) inp outp)
               (apply-instruction m (+ 3 i) inp outp))
          70 (if (< (get-val-by-mode m (+ 1 i) p1) (get-val-by-mode m (+ 2 i) p2))
               (apply-instruction (assoc m (get-val-imm m (+ i 3)) 1) (+ i 4) inp outp)
               (apply-instruction (assoc m (get-val-imm m (+ i 3)) 0) (+ i 4) inp outp))
          80 (if (= (get-val-by-mode m (+ 1 i) p1) (get-val-by-mode m (+ 2 i) p2))
               (apply-instruction (assoc m (get-val-imm m (+ i 3)) 1) (+ i 4) inp outp)
               (apply-instruction (assoc m (get-val-imm m (+ i 3)) 0) (+ i 4) inp outp))
          90 outp
          (println curVal)))))

(apply-instruction (instructions) 0)

(apply-instruction (test-instructions "3,9,8,9,10,9,4,9,99,-1,8") 0)

(test-instructions "3,9,8,9,10,9,4,9,99,-1,8")

;;part-one
(let [maxA (atom 0)]
  (doseq [ps1 (range 5)
          ps2 (remove (fn [x] (= x ps1)) (range 5))
          ps3 (remove (fn [x] (or (= x ps1) (= x ps2))) (range 5))
          ps4 (remove (fn [x] (or (= x ps1) (= x ps2) (= x ps3))) (range 5))
          ps5 (remove (fn [x] (or (= x ps1) (= x ps2) (= x ps3) (= x ps4))) (range 5))
          ]
    (let [a1 (apply-instruction (instructions) 0 [ps1 0] 0)
          a2 (apply-instruction (instructions) 0 [ps2 a1] 0)
          a3 (apply-instruction (instructions) 0 [ps3 a2] 0)
          a4 (apply-instruction (instructions) 0 [ps4 a3] 0)
          a5 (apply-instruction (instructions) 0 [ps5 a4] 0)
          ]
      (if (> a5 @maxA)
        (reset! maxA a5))
      ;;(println  "a1: " a1)
      ))
  (print @maxA))
