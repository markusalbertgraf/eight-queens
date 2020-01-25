(ns core
  "Calculates position of queens on a chess board with the number
  of queens equal to the number of rows of the board in which no
  queen threatens another queen.")

(set! *warn-on-reflection* true)

;; Helpers

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
  y-positions.  This yields a speedup of 10x."
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
  (or (= y1 y2) ;; this should be filtered in next-board
      (= (Math/abs ^Integer (- y1 y2))
         (Math/abs ^Integer (- x1 x2)))
      (= x1 x2) ;; this should be impossible
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
       (= x1 (- size 2))        board
       :else                    (recur (inc x1) (+ 2 x1) board)))))

;; Recursive, sideeffecting Solution

(defn recursive-queens
  "Takes the nr of rows of the board and returns the first solution.
  With optional second parameter all? returns all solutions"
  ([rows] (recursive-queens rows false))
  ([rows all?]
   (loop [board   (vec (repeat rows 0))
          counter 1]
     (cond
       (nil? board)    (println "no more solutions")
       (solved? board) (do (printf "Solution %4d: %s\n" counter board)
                           (when all? (recur (next-board board) (inc counter))))
       :else           (recur (next-board board) counter)))))

;; Functional Solution

(defn lazy-boards
  "Returns a lazy seq of all boards that have each Quean on a different row"
  [board]
  (lazy-seq
   (when-let [new-board (next-board board)]
     (cons board (lazy-boards new-board)))))

(defn functional-queens
  "Filters all possible boards for solutions. Takes the number of rows and.
  With optional second parameter all? returns all solutions"
  ([rows] (functional-queens rows false))
  ([rows all?]
   (let [board (vec (repeat rows 0))]
     (if all?
       (filter solved? (lazy-boards board))
       (some solved? (lazy-boards board))))))

(comment
  (time (recursive-queens 8 true))
  (time (println (functional-queens 8 true)))
  )








