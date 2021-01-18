(ns Day11)

(require '[aoc])
(require '[clojure.string :as str])

(defn instructions []
  (zipmap (range) (map #(bigint %) (aoc/read-file-split "resources/day11.txt" #","))))

(defn test-instructions [x]
  (zipmap (range) (map #(bigint %) (str/split x #","))))

(defn get-val [m i]
  (get m (get m i 0) 0))

(defn get-val-imm [m i]
  (get m i 0))

(defn get-val-by-mode [m i mode rb]
  (case mode
    0 (get-val m i)
    1 (get-val-imm m i)
    2 (get m (+ (get m i 0) rb) 0)
  ))

(defn get-val-by-mode-3 [m i mode rb]
  (case mode
    0 (get-val-imm m i)
    1 (get-val-imm m i)
    2 (+ (get m i 0) rb) 0))

(defn add-instr [m i p1 p2 rb]
  (+ (get-val-by-mode m (+ i 1) p1 rb) (get-val-by-mode m (+ i 2) p2 rb)))

(defn mult-instr [m i p1 p2 rb]
  (* (get-val-by-mode m (+ i 1) p1 rb) (get-val-by-mode m (+ i 2) p2 rb)))

(defn run-instruction [m i rb inp outp]
  (let [lkp (get m i)
        curVal (-> (get m i) aoc/int-to-vec)
        padVal (->> (-> (reverse curVal) vec (conj ,,, 0 0 0 0)) (take 5) vec reverse vec)
        p3 (first padVal)
        p2 (first (rest padVal))
        p1 (first  (rest (rest padVal)))
        instr (if (< lkp 100)
                lkp
                (last curVal))
        inp2 (if (= 1 (count inp))
               []
               [(last inp)])]
    (do ;;(println lkp i rb inp outp)
      (case instr
        3 (do
         ;; (println (get m (+ i 1)) (get-val-by-mode-3 m (+ i 1) p1 rb) p1 rb inp)
            [(assoc m (get-val-by-mode-3 m (+ i 1) p1 rb) (first inp)) (+ 2 i) rb inp2 outp])
        4 (do
           ;; (println (get-val-by-mode m (+ i 1) p1 rb))
          ;;[m (+ 2 i) (get-val-by-mode m (+ i 1) p1 rb)]
            [m (+ 2 i) rb inp (conj outp (get-val-by-mode m (+ i 1) p1 rb))])
        1 [(assoc m (get-val-by-mode-3 m (+ i 3) p3 rb) (add-instr m i p1 p2 rb)) (+ i 4) rb inp outp]
        2 [(assoc m (get-val-by-mode-3 m (+ i 3) p3 rb) (mult-instr m i p1 p2 rb)) (+ i 4) rb inp outp]
        5 (if (not= 0 (get-val-by-mode m (+ 1 i) p1 rb)) ;;jump-if-true
            [m (get-val-by-mode m (+ 2 i) p2 rb) rb inp outp]
            [m (+ 3 i) rb inp outp])
        6 (if (= 0 (get-val-by-mode m (+ 1 i) p1 rb)) ;;jump-if-false
            [m (get-val-by-mode m (+ 2 i) p2 rb) rb inp outp]
            [m (+ 3 i) rb inp outp])
        7 (if (< (get-val-by-mode m (+ 1 i) p1 rb) (get-val-by-mode m (+ 2 i) p2 rb))
            [(assoc m (get-val-by-mode-3 m (+ i 3) p3 rb) 1) (+ i 4) rb inp outp]
            [(assoc m (get-val-by-mode-3 m (+ i 3) p3 rb) 0) (+ i 4) rb inp outp])
        8 (if (= (get-val-by-mode m (+ 1 i) p1 rb) (get-val-by-mode m (+ 2 i) p2 rb))
            [(assoc m (get-val-by-mode-3 m (+ i 3) p3 rb) 1) (+ i 4) rb inp outp]
            [(assoc m (get-val-by-mode-3 m (+ i 3) p3 rb) 0) (+ i 4) rb inp outp])
        9 [m (+ 2 i) (+ rb (get-val-by-mode m (+ 1 i) p1 rb)) inp outp]
        99 [{} -1 -1 [] outp]
        (println curVal)))))



(defn test-instr []
  (test-instructions "1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,3,1,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,33,1,1011,1102,1,26,1010,1101,0,594,1029,1101,0,20,1018,1102,38,1,1000,1102,35,1,1001,1101,800,0,1023,1101,0,599,1028,1101,0,34,1013,1101,0,737,1026,1102,21,1,1005,1102,1,0,1020,1102,1,195,1024,1101,31,0,1016,1101,0,1,1021,1102,22,1,1004,1102,1,32,1014,1102,37,1,1019,1102,36,1,1002,1101,23,0,1003,1102,190,1,1025,1101,28,0,1009,1101,807,0,1022,1102,30,1,1015,1101,0,27,1017,1102,1,25,1012,1102,1,39,1008,1101,0,29,1007,1101,734,0,1027,1101,0,24,1006,109,28,2105,1,-4,4,187,1105,1,199,1001,64,1,64,1002,64,2,64,109,-19,1208,-9,37,63,1005,63,219,1001,64,1,64,1106,0,221,4,205,1002,64,2,64,109,20,1206,-8,233,1106,0,239,4,227,1001,64,1,64,1002,64,2,64,109,-29,2101,0,4,63,1008,63,21,63,1005,63,259,1106,0,265,4,245,1001,64,1,64,1002,64,2,64,109,-2,2107,37,4,63,1005,63,285,1001,64,1,64,1106,0,287,4,271,1002,64,2,64,109,14,1206,8,301,4,293,1105,1,305,1001,64,1,64,1002,64,2,64,109,11,21101,40,0,-6,99"))

(defn f [[m s] [x y]]
  [m (str s x "," y "," (get m [x y]) "\n")]) ;;output as comma separated

;;part-one
(loop [m (instructions)
       i 0
       rb 0
       inp [1]
       outp []
       panels {[0 0] 1}
       x 0
       y 0
       dir 0] ;;0=N, 1=E, etc.
  (let [ex (run-instruction m i rb inp outp)]
    (if (= -1 (get ex 1))
      (spit "resources/cdp.txt" (last (reduce f [panels ""]  (keys panels))))
      (if (not= 2 (count (ex 4)))
        (recur (ex 0) (ex 1) (ex 2) (ex 3) (ex 4) panels x y dir)
        (let [panels2 (assoc panels [x y] (first (ex 4)))
              dir2 (if (= 0 (last (ex 4))) (dec dir) (inc dir))
              dir3 (cond
                     (= dir2 -1) 3
                     (= dir2 4) 0
                     :else dir2)
              x2 (cond
                   (= dir3 0) x
                   (= dir3 1) (inc x)
                   (= dir3 2) x
                   (= dir3 3) (dec x))
              y2 (cond
                   (= dir3 0) (inc y)
                   (= dir3 1) y
                   (= dir3 2) (dec y)
                   (= dir3 3) y)]
          (recur (ex 0) (ex 1) (ex 2) [(get panels2 [x2 y2] 0)] [] panels2 x2 y2 dir3))))))

