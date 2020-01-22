(ns core
  "Calculates positin of queens on a chess board with the number
  of queens equal to the number of rows of the board in which no
  queen threatens another queen.")

(set! *warn-on-reflection* true)

(defn next-board*
  "Helper to find the next board setup"
  [pos board]
  (when (< pos (count board))
    (if (= (nth board pos) (- (count board) 1))
      (recur (inc pos) (assoc board pos 0))
      (update board pos inc))))

(defn next-board
  "Takes a board and returns the next setup.
  Only return boards with all queens on different
  y-positions.  This yealds a speedup of 10x."
  [board]
  (let [size      (count board)
        new-board (next-board* 0 board)
        positions (count (set new-board))]
    (if (= size positions)
      new-board
      (recur new-board))))

(defn threatens?
  "Returns true if two queens threaten each other"
  [x1 y1 x2 y2]
  (or (= y1 y2)
      (= (Math/abs ^Integer (- y1 y2))
         (Math/abs ^Integer (- x1 x2)))
      (= x1 x2) ;; this sould be redundant but is here
      )) 

(defn solved?
  "Checks all pairs of queens for safety."
  ([board]
   (solved? 0 1 board))
  ([x1 x2 board]
   (let [y1   (nth board x1)
         y2   (nth board x2)
         size (count board)]
     (cond
       (threatens? x1 y1 x2 y2) false
       (< x2 (- size 1))        (recur x1 (inc x2) board)
       (and (= x1 (- size 2))
            (= x2 (- size 1)))  true
       :else                    (recur (inc x1) (+ 2 x1) board)))))

(defn start
  "Takes the nr of rows of the board and returns the first solution.
  With optionol second parameter all?, returns all solutions"
  ([rows] (start rows false))
  ([rows all?]
   (loop [board   (vec (repeat rows 0))
          counter 1]
     (cond
       (nil? board)    (println "no more solutions")
       (solved? board) (do (printf "Solution %4d: %s\n" counter board)
                           (when all? (recur (next-board board) (inc counter))))
       :else           (recur (next-board board) counter)))))

(comment
  (time (start 8 true)))
