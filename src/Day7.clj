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
    ;;(do ;;(println curVal " " i)
        (case instr
          3 (apply-instruction (assoc m (get-val-imm m (+ i 1)) (first inp)) (+ 2 i) [(last inp)] outp)
          4 ;;(do
            ;;(println i)
              ;;(println (get m (get-val-imm m (+ i 1))))
              ;;(apply-instruction m (+ 2 i) inp (get m (get-val-imm m (+ i 1))))
            [m (+ 2 i) (get m (get-val-imm m (+ i 1)))]
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
               ;;(apply-instruction m (+ 2 i) inp (get-val-by-mode m (+ i 1) p1))
             [m (+ 2 i) (get m (get-val-by-mode m (+ i 1) p1))]
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
          90 [{} -1 outp]
          (println curVal))))

(apply-instruction (instructions) 0)

(apply-instruction (test-instructions "3,9,8,9,10,9,4,9,99,-1,8") 0)

(defn test-instr []
  (test-instructions "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
)

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


;;part-two

(defn run-amps [[ps1 m1 i1] [ps2 m2 i2] [ps3 m3 i3] [ps4 m4 i4] [ps5 m5 i5]]
  (let [a1 (apply-instruction m1 i1 ps1 0)
        pps2 (if (= -1 (last ps2))
               [(first ps2) (last a1)]
               [(last a1)])
        a2 (apply-instruction m2 i2 pps2 0)
        pps3 (if (= -1 (last ps3))
               [(first ps3) (last a2)]
               [(last a2)])
        a3 (apply-instruction m3 i3 pps3 0)
        pps4 (if (= -1 (last ps4))
               [(first ps4) (last a3)]
               [(last a3)])
        a4 (apply-instruction m4 i4 pps4 0)
        pps5 (if (= -1 (last ps5))
               [(first ps5) (last a4)]
               [(last a4)])
        a5 (apply-instruction m5 i5 pps5 0)]
    ;;(do
     ;; (println ps1 ps2 ps3 ps4 ps5)
    (if (= -1 (first (rest a5)))
      (first ps1)
      (run-amps [[(last a5)] (first a1) (first (rest a1))] 
                [[0] (first a2) (first (rest a2))]
                [[0] (first a3) (first (rest a3))]
                [[0] (first a4) (first (rest a4))]
                [[0] (first a5) (first (rest a5))]
                ))
;;    )
))

(let [maxA (atom 0)]
  (doseq [ps1 (range 5 10)
          ps2 (remove (fn [x] (= x ps1)) (range 5 10))
          ps3 (remove (fn [x] (or (= x ps1) (= x ps2))) (range 5 10))
          ps4 (remove (fn [x] (or (= x ps1) (= x ps2) (= x ps3))) (range 5 10))
          ps5 (remove (fn [x] (or (= x ps1) (= x ps2) (= x ps3) (= x ps4))) (range 5 10))]
    (let [res (run-amps [[ps1 0] (instructions) 0]
                        [[ps2 -1] (instructions) 0]
                        [[ps3 -1] (instructions) 0]
                        [[ps4 -1] (instructions) 0] 
                        [[ps5 -1] (instructions) 0])]
      (if (> res @maxA)
        (reset! maxA res))
      ;;(println  "res: " res)
      ))
  (print @maxA))
