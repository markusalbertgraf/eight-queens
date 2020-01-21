(ns core
  "Calculates positin of queens on a chess board with one queen with the number
  of queens equal to the size of the board in which no queen threatens another.")

(defn increase [pos board]
  "Helper to find the next board setup"
  (when (< pos (count board))
    (if (= (nth board pos) (- (count board) 1))
      (recur (inc pos) (assoc board pos 0))
      (update board pos inc))))

(defn next-board [board]
  "Takes a board and returns the next setup"
  (increase 0 board))

(defn threatens? [x1 y1 x2 y2]
  "Returns true if two queens threaten each other"
  (or (= y1 y2) (= (Math/abs (- y1 y2)) (Math/abs (- x1 x2))) (= x1 x2)))

(defn safe? [x1 x2 board]
  "Checks all pairs of queens for safety."
  (let [y1   (nth board x1)
        y2   (nth board x2)
        size (count board)]
    (cond
      (threatens? x1 y1 x2 y2) false
      (< x2 (- size 1))        (recur x1 (inc x2) board)
      (and (= x1 (- size 2))
           (= x2 (- size 1)))  true
      :else                    (recur (inc x1) (+ 2 x1) board))))

(defn passt? [board]
  "Returns true if no queen theatens another queen."
  (safe? 0 1 board))

(defn los [size]
  "Takes the size of the board and returns the first possible solution"
  (loop [board (vec (repeat size 0))]
    (cond
      (nil? board)   (println "Keine Lösung")
      (passt? board) (println "Lösung: " board)
      :else          (recur (next-board board)))))

(comment
  (time (los 7)))
