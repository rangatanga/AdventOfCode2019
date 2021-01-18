(ns Day11)

(require '[aoc])
(require '[clojure.string :as str])

(defn instructions []
  (zipmap (range) (map #(bigint %) (aoc/read-file-split "resources/day13.txt" #","))))

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


(defn get-tile [screen id]
  (key (first (filter #(= id (second %)) screen))))

(defn run-instruction [m i rb inp outp screen]
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
        3 (let [ball (get-tile screen 4)
                paddle (get-tile screen 3)
                joy (cond (< (first ball) (first paddle)) -1
                          (> (first ball) (first paddle)) 1
                          :else 0)
                ]
            ;;(println "get input")
;;            [(assoc m (get-val-by-mode-3 m (+ i 1) p1 rb) (first inp)) (+ 2 i) rb inp2 outp])
            [(assoc m (get-val-by-mode-3 m (+ i 1) p1 rb) joy) (+ 2 i) rb [] outp])
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


(defn f [[m s] [x y]]
  [m (str s x "," y "," (get m [x y]) "\n")]) ;;output as comma separated

;;part-one
(loop [m (instructions)
       i 0
       rb 0
       inp []
       outp []
       screen {}]
  (let [ex (run-instruction m i rb inp outp screen)]
    (if (= -1 (get ex 1))
      (print (count (filter #(= 2 %) (vals screen))))
      (if (not= 3 (count (ex 4)))
        (recur (ex 0) (ex 1) (ex 2) (ex 3) (ex 4) screen)
        (let [screen2 (assoc screen [(first (ex 4)) (first (rest (ex 4)))] (last (ex 4)))]
          (recur (ex 0) (ex 1) (ex 2) [] [] screen2))))))

;;part-two
  (loop [m (assoc (instructions) 0 2)
         i 0
         rb 0
         inp []
         outp []
         screen {}]
    (let [ex (run-instruction m i rb inp outp screen)]
      (if (= -1 (get ex 1))
        (print (count (filter #(= 2 %) (vals screen))))
        (if (not= 3 (count (ex 4)))
          (recur (ex 0) (ex 1) (ex 2) (ex 3) (ex 4) screen)
          (if (and (= -1 (first (ex 4))) (= 0 (first (rest (ex 4)))))
            (do (println "Current score: " (last (ex 4)))
                (recur (ex 0) (ex 1) (ex 2) (ex 3) [] screen))
            (let [screen2 (assoc screen [(first (ex 4)) (first (rest (ex 4)))] (last (ex 4)))]
              (recur (ex 0) (ex 1) (ex 2) (ex 3) [] screen2)))))))


