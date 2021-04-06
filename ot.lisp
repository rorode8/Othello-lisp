#|
                  ***** BOARD-FUNCS.LSP *****

  description:
  This file contains the functions that check if
  moves are valid and make moves on the board itself. the two
  main external functions are get-valid-moves that returns
  the moves a player can make and make-move-int that returns
  an new board state after making a move.

  Author:  J. Anthony Brackins, Marcus Haberling


  Written Spring 2016 for CSC447/547 AI class.

|#


#|
  name: direction constants
 
  description:
  The Direction Constants are a more readable way to
  shift board position in one of the 8 directions
  that need to be checked in relation to move making in othello
|#
(defconstant NORTH     -8)
(defconstant NORTHEAST -7)
(defconstant EAST       1)
(defconstant SOUTHEAST  9)
(defconstant SOUTH      8)
(defconstant SOUTHWEST  7)
(defconstant WEST      -1)
(defconstant NORTHWEST -9)

(defun new-board ()
  "Creates an empty othello board"
  '( - - - - - - - -
     - - - - - - - -
     - - - - - - - -
     - - - B W - - -
     - - - W B - - -
     - - - - - - - -
     - - - - - - - - 
     - - - - - - - -) 
)

#|
  Name: make-safe-movedirection
  Description:
  converts an row col pair into a value
  0-63 on the board
  Paramaters:
    pos-x - the row
    pos-y - the column
|#
(defun make-pos (pos-x pos-y)
  (+ (- pos-x 1) (* (- pos-y 1) 8))
)


#|
  Name: On-Board
  Description:
  Checks if a move from a provided position is still on the board
  checking mathematically if that move is possible from the starting
  position. Eg, if the move is right then the position we are
  moving from cant be in the right most row.

  Paramaters:
    pos - current position on the board 0-63
    move - place to move on the board from the position
           usually passed in as one of the direction 
           constants.
|#
(defun on-board (pos move)
  "Checkis if a move from the provided position is on the board"
  (cond
    ((= move NORTH) (> pos 7))
    ((= move NORTHEAST) (and (> pos 7) (not (= (mod pos 8) 7))))
    ((= move EAST) (not (= (mod pos 8) 7)))
    ((= move SOUTHEAST) (and (< pos 56) (not (= (mod pos 8) 7))))
    ((= move SOUTH) (< pos 56))
    ((= move SOUTHWEST) (and (< pos 56) (not (= (mod pos 8) 0))))
    ((= move WEST) ( and (not (= (mod pos 8) 0)) ( > pos 1 ) )  )
    ((= move NORTHWEST) (and (> pos 7) (not (= (mod pos 8) 0))))
    (t nil)
  )
)

#|
  Name: valid-move-direction
  Description:
  An inner function to be called by valid-move to check if the move
  acomplishes a flank in a provided direction. This function
  recursively calls itself on moves in a provided direction.
  - If you move off the board, false is returned
  - if you encounter your color, there is no flank to be made,
    return null
  - If you encounter the enemy color, you may be able to flank, call
    recursively with the good variable set to true instead of false.
  - If you encounter a space check the good variable to see if you
    have seen a an enemy piece yet. If you have return true else
    return false.
  

  Paramaters:
    board - current board state
    pos - current position on the board 0-63
    move - place to move on the board from the position
           usually passed in as one of the direction 
           constants.
    color - color black or white of current player
    good - have we seen an opponent piece yet?
|#
(defun valid-move-direction (board pos move color good)
  "Checks if moving in a specified direction validates a move position"
  (let ( (newPos (+ pos move)) )
    (cond
      ( (not (on-board pos move)) nil)
      ;Modified these to string= because they seem to work 
      ;more consistently.... JB
      ( (string= color (nth newPos board)) good)
      ( (string= '- (nth newPos board)) nil)
      ( t (valid-move-direction board newPos move color t) )
    )
  )    
)

#|
  Name: valid-move
  Description:
  valid-move checks if a provided move can be made.
  It assumes the move is on the board. It checks if the
  space to move is empty. It then calls valid-move-direction
  in all directions to see if there is any position that you
  can flak from that position. If there is one it returns
  true else false.

  Paramaters:
    board - current board state
    pos - current position on the board 0-63
    color - color black or white of current player
|#
(defun valid-move (board pos color)
  (if (equal (nth pos board) '-)
    (or
       (valid-move-direction board pos NORTH     color nil)
       (valid-move-direction board pos NORTHEAST color nil)
       (valid-move-direction board pos EAST      color nil)
       (valid-move-direction board pos SOUTHEAST color nil)
       (valid-move-direction board pos SOUTH     color nil)
       (valid-move-direction board pos SOUTHWEST color nil)
       (valid-move-direction board pos WEST      color nil)
       (valid-move-direction board pos NORTHWEST color nil)
    )
    nil
  )
)

#|
  Name: get-valid-moves
  Description:
  returns a list of moves a player of the provided color could make
  by calling valid-move on every board posistion, if the move is valid it
  adds that move to the list.

  Paramaters:
    board - current board state
    color - color black or white of current player
|#
(defun get-valid-moves (board color)
  (let ( (return-list nil) )
    (loop for x from 0 to 63 do
      (if (valid-move board x color) (setq return-list (cons x return-list)))
    )

    return-list
  )
)

#|
  Name: make-move-direction
  Description:
  Nearly identical logic to the valid-move-direction. instead of just returning
  true if there is a flank, this function flips all the pieces its encounters
  if not it returns nil instead of a new board.  
  by calling valid-move on every board posistion, if the move is valid it
  adds that move to the list.

  Paramaters:
    board - current board state
    pos - current position on the board 0-63
    move - place to move on the board from the position
           usually passed in as one of the direction 
           constants.
    color - color black or white of current player
    good - have we seen an opponent piece yet?
|#
(defun make-move-direction (board pos move color good)
  (let ( (new-pos (+ pos move)) )
    (cond
      ( (not (on-board pos move)) nil) 
      ( (equal color (nth new-pos board)) good)
      ( (equal '- (nth new-pos board)) nil)
      ( (make-move-direction board new-pos move color t)
  (setf (nth new-pos board) color))
      ( t nil)
    )
  )
)

#|
  Name: make-move-direction
  Description:
  Makes a new board state by flipping the all flanked tiles in 
  all directions using the make-move-direction function
  then returns the new state. This function doesnt check if the
  move was valid in the first place.

  Paramaters:
    board - current board state
    pos - current position on the board 0-63
    color - color black or white of current player
|#
(defun make-move-int (board pos color)
  (let ( (new-board (copy-list board)) )
    (setf (nth pos new-board) color)
    (make-move-direction new-board pos NORTH     color nil)
    (make-move-direction new-board pos NORTHEAST color nil)
    (make-move-direction new-board pos EAST      color nil)
    (make-move-direction new-board pos SOUTHEAST color nil)
    (make-move-direction new-board pos SOUTH     color nil)
    (make-move-direction new-board pos SOUTHWEST color nil)
    (make-move-direction new-board pos WEST      color nil)
    (make-move-direction new-board pos NORTHWEST color nil)
    new-board
  )
  )

#|
  Name: make-safe-movedirection
  Description:
  A testing and development function to check if a provided
  move is safe and make it if it is, returning the new board state.

  Paramaters:
    board - current board state
    pos - current position on the board 0-63
    color - color black or white of current player
|#
(defun make-safe-move (board pos color)
  (if (valid-move board pos color)
    (make-move-int board pos color)
    board
  )
)


#|
                    ***** HEURISTIC-FUNCS.LSP *****

  Descritpion:
  This file contains all functions and constants used for the static evaluation
  function. Our program uses a variation of the coin-parity heuristic in which
  spaces are counted acording to weights.

  Authors:  J. Anthony Brackins, Marcus Haberling

  Written Spring 2016 for CSC447/547 AI class.

|#

#|
  constants: score-weights
  description:
  A value map on the board that causes the weighted-count to
  prioritize different positions. All of the weights were assigned
  as powers of 2.The folling is a list of the weighted regions and 
  the reasoning for those weights.

  Corners 32: 
    Corners had originally been 16, however in testing the
    ai gave them up too easily. 32 was a better number, 
    causing the ai to almost always take corners.
  Areas around corners 01: 
    Since the areas around corners will give your opponents an
    opprotunity to take the corner they are the lowest score
    on the list.
  Outer Sides 16: 
    Outer sides seemed more important than the inner
    sides when playing because they are harder to flank. 
    When they were previously equivilent the ai didn't properly 
    defend them which allowed the other player to build up a strong 
    side of the board more easily. 
  Inner Sides 8: 
    The inner sides are more important than the center board but the
    easiest to flank so we set those up as the weakest of the sides
    this makes the program build up a side from corner to the next.
  2nd Row/Col in 2: These spaces were set to 2  (the default weight) because
    they open up an opportunity to take a side.
  3rd Row/Col in 4: These are usually safe moves so they are prioritized
    over the standard value of 2. They also open the opponent to less
    safe move.
  Starting Positions: These are given the standard value of 2 as to not
    over influence the game since they are both inconsequential and hard
    to control.              

  
|#
(defconstant score-weights
  '( 32 01 16 08 08 16 01 32
     01 01 02 02 02 02 01 01
     16 02 04 04 04 04 02 16
     08 02 04 02 02 04 02 08
     08 02 04 02 02 04 02 08
     16 02 04 04 04 04 02 16
     01 01 02 02 02 02 01 01
     32 01 16 08 08 16 01 32 )
)

#|
  name: score-weight other-color
  description:
  gives the opposite board color as the one provided
|#
(defun other-color (color)
  (cond
    ( (string= color 'B) 'W)
    ( (string= color 'W) 'B)
    ( T '- )
  )
)

#|
  name: weighted-count

  description:
  counts all the weights corresponding to board positions
  where a provided player has pieces. It runs through the list
  recursively if it encounters a color match it adds that positions
  weight to the sum

  paramaters:
    board - othello game board state
    weights - weights being passed in to compliment the board
    color - color of player to count for, black or white
|#
(defun weighted-count (board weights color)
  (cond
   ( (null board) 0)
   ( (equal color (car board))
     (+ (car weights) (weighted-count (cdr board) (cdr weights) color))
   )
   ( t (weighted-count (cdr board) (cdr weights) color))
  )
)

#|
  name: weighted-parity

  description:
  Based on the coin parity algorithim, it compares the weighted
  sums of a player and his oponents pieces on the board

  paramaters:
    board - othello game board state
    weights - weights being passed in to compliment the board
    max-color - color of player to count for, black or white
|#
(defun weighted-parity (board weights max-color)
  (let
    (
      ( max-count (weighted-count board weights max-color))
      ( min-count (weighted-count board weights (other-color max-color)))
    )
    (/ (* 100 (- max-count min-count) )(+ max-count min-count))
  )
)

#|
                  ***** MINIMAX.LSP *****

Generalized recursive minimax routine.

Author: Dr. John M. Weiss. Modifications made by Julian Brackins and
        Marcus Haberling
Class:  SDSM&T CSC447/547 Artificial Intelligence
Date:   Spring 2016

Usage:    (minimax position depth)
          where position is the position to be evaluated,
          and depth is the search depth (number of plys).

Returns:  (value path)
          where value is the backed-up value from evaluation of leaf nodes,
          and path is the path to the desired leaf node.

Functions called:

          (deepenough depth) -
              predicate that returns T if the current position has reached
              the desired search depth, NIL otherwise.

          (move-generator position) -
              generates successors to the position.

          (static position) -
              applies the static evaluation function to the position.

Modifications: 
        Adapted program to fit with our project, implemented alpha beta pruning.

|#


#|
  Name: deepenough
  Description: 
  The function to deepenought is used by Dr. Weiss minimax function
  to check if the program should stop going deeper. We don't have
  any special requirements of depth for our algorithim so it just stops
  at zero.

  Paramater:
    depth - current depth
|#
(defun deepenough (depth)
  (< depth 1)
)

(defun print-moves (list)
  (cond
    ( (null list) nil)
    (t
      (print-board (car list))
      (print-moves (cdr list))
    )
  )
)

#|
  Name: move-generator
  Description: 
  This function is used by Dr. Weiss's minimax function to generate possible
  moves. It workd by generating positions of moves using get-valid-moves
  and then turns them into board states using make-move-int.

  Paramater:
    position - current board state
    color - color of current player (black or white)
|#
(defun move-generator (position color)
  (loop for x in (get-valid-moves position color)
    collect (make-move-int position x color) 
  )
)

#|
  Name: static
  Description:
  static evaluation function for Dr. Weiss's minimax funtion.
  it calls our weighted-parity function to get its value

  Paramaters
    position - current board state
    color - player to calculate a value for.
|#
(defun static (position color)
    (weighted-parity position score-weights color)
)

#|
  Name: minimax
  Description:
  The mini-max function provided by Dr. Weiss modified to enable alpha 
  beta pruning. Alpha and beta are optional paramaters that should only
  be provided internally. If beta is ever found to be less then alpha
  the algorithim stops minimax on that level "pruning off" remaining states
  as a method to save computational time.

  Paramaters
    position - current board state
    depth - how many more levels to search down
    color - player to calculate a value for.
    max? : t - true for maximizing player, false for minimizing player
    alpha : -1000000 - the ever increasing alpha value
    beta  : 1000000 - the ever decreasing beta value.
|#
(defun minimax (position depth color &optional (max? t) (alpha -100000) (beta 100000) )

  ; if we have searched deep enough, or there are no successors,
  ; return position evaluation and nil for the path
  (if (or (deepenough depth) (null (move-generator position color)))
    (list (static position color) nil)

    ; otherwise, generate successors and run minimax recursively
    (let
      (
        ; generate list of sucessor positions
        (successors (move-generator position color))

        ; initialize current best path to nil
        (best-path nil)

        ; initialize current best score to negative infinity
        (best-score -1000000)

                ; other local variables
        succ-value
        succ-score
      )

      ; explore possible moves by looping through successor positions
      (dolist (successor successors)

        ; perform recursive DFS exploration of game tree
        (setq succ-value (minimax successor (1- depth) (other-color color) (not max?) alpha beta ))

        ; change sign every ply to reflect alternating selection
        ; of MAX/MIN player (maximum/minimum value)
        (setq succ-score (- (car succ-value)))

        ; update best value and path if a better move is found
        ; (note that path is being stored in reverse order)
        (when (> succ-score best-score)
          (setq best-score succ-score)
          (setq best-path (cons successor (cdr succ-value)))
        )

        (when (and max? (> best-score alpha))
          (setf alpha best-score)
        )
        (when (and (not max?) (< (- best-score) beta))
          (setf beta  (- best-score) )
        )

        (when (> alpha beta)
          (return)
        )
      )
      ; return (value path) list when done
      (list best-score best-path)
    )
  )
)

;;I/O methods
(defun read-array (filename)
  (with-open-file (in filename)
    (loop for num = (read in nil)
          until (null num)
          collect num)))

(defun write-numeric-list(filename l)
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
    (dolist (segment l)
      (format out "~A " segment))
    (format out "~%")))

(setq lista (read-array "python.txt"))

(setq niv (first lista) color (second lista) estadoOthello (cddr lista))

( defparameter *board-two* 
                            '( - - - - - - - -
                               - - - - - - - - 
                               - - - - - - - - 
                               - - - W B - - - 
                               - - - B B - - - 
                               - - - - B - - - 
                               - - - - - - - - 
                               - - - - - - - - 
                             ) 
)

(setq res (caadr (minimax estadoOthello niv color)))
(write-numeric-list "lispOutput.txt" res)
