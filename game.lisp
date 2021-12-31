;;;; LIGTS OUT GAME LISP PROJECT
;;;; @author Seymour Pashayev


;; Game-Driver ---------------------------------------------------------------------------

;; Starts the easy lights-out game
;; Uses 3, 3 for board-size and percent-fill respectivelly
(defun start-game-easy ()
    (start-game-settings-size-percentfill 3 3)
)

;; Starts the easy lights-out game
;; Uses 3, 3 for board-size and percent-fill respectivelly
(defun start-game-hard ()
    (start-game-settings-size-percentfill 8 1)
)

;; Starts the lights-out game
;; Function with settings for board-size and percent-fill
;; Input is board-size & percent-fill
(defun start-game-settings-size-percentfill (board-size percent-fill)
    (let ((board (generate-board board-size percent-fill '())))                     ;; board is generated and saved to board
        (one-guess board "~%Pick a bit to switch: " 0 board-size)
    )
)


;; Take one guess at a target number.
;; When the lights are out, the game quits
;; Otherwise makes a recursive call to guess again.
;; Parameters: board, message, count, board-size
(defun one-guess (board message count board-size)
    (print-board board-size board)
    (if (lights-out board)
        (format T "~%You won after ~D turns.~%" count)
        (let    (
                (guess (get-input (length board) message))
                )
            (one-guess (toggle guess board board-size) "~%Pick a bit to switch: " (+ 1 count) board-size)
        )
    )
)

;; Prompts user for some input
;; Returns the typed number
;; Enforces the 0 to list-length value of typed text
(defun get-input (list-length prompt)
    (format T prompt)
    (let ( (usernum (read)))
        (if (or (< usernum 0) (> usernum (- list-length 1)))
           (get-input list-length "Not Possible. Enter again: ")
           usernum
        )
    )
)


;; ENDOF: Game-Driver --------------------------------------------------------------------

;; Board-Related -------------------------------------------------------------------------

;; Function that prints the board
;; Takes a list of values
(defun print-board (board-size list)
    
    (let ((currentcount (mycount list)))                ;; currentcount = mycount(list)
        (if (= (mod currentcount board-size) 0)         ;; if the currentcount is divisible by 3
            (format t "~%~A " (car list))               ;; print out the newline char & an item
            (format t "~A " (car list))                 ;; alternavtively, do not print out only the item
        )
    )
    (if (null (cdr list))                               ;; if the rest of the list is empty, don't recurse
        (progn (format t "~%")
                (format t "------------")
                nil)
        (print-board board-size (cdr list))             ;; alternatively, print the rest of the lines
    )
)


;; Function to count numbers in the list
(defun mycount (list)
    (if (null (cdr list))
        1
        (+ 1 (mycount(cdr list)))
    )
)

;; Function that generates a board of given size
;; inputs:  board-size
;;          list, passed into the function as empty
;;          percent-fill is int (1-9) stands for 10% - 90%
(defun generate-board (board-size percent-fill list)
    (if (< (length list) (* board-size board-size))                         ;; if the length of the passed list is less than board-size squared
        (generate-board board-size percent-fill (append list (cons          ;; recursively call generate-board appending 0 or 1 to the list
            (if (< (random 10) percent-fill) 1 0)              nil)))       ;; based on the random funciton on this line
        (if (lights-out list)
            (generate-board board-size percent-fill '())
            list
        )                                                                ;; alternatively return the list
    )
)

;; ENDOF: Board-Related --------------------------------------------------------------

;; Toggle Related --------------------------------------------------------------------

;; Function to toggle the n-th value in the list
(defun toggle-one (n list)
    (if (null n)                                                ;; if null is passed, return the list
        list
        (if (= n 0)                                             ;; if n is zero
            (cons (if (= (car list) 0) 1 0)(cdr list))          ;; toggle the value & connect it to the rest of the list
            (cons (car list) (toggle-one (- n 1) (cdr list)))   ;; cons current w/ the rest changed
        )
    )
)


;; Function to implement toggle with cascading effect onto the adjacent bits
;; n is the toggled by user bit
;; list is the list of the bits
;; bs is board size
(defun toggle (n list bs)
    (let ((locations-to-toggle (list
                                (prev-row n list bs)
                                (prev-col n list bs)
                                n
                                (next-col n list bs)
                                (next-row n list bs))))
        (toggle-helper locations-to-toggle list)
    )
)

;; Toggle helper function
;; Takes a list of locations to toggle and the board list
(defun toggle-helper (loc-list board-list)
    (if (and (null (cdr loc-list)) (null (car loc-list)))                           ;; if the location-list is empty
        board-list                                                                  ;; return the board
        (toggle-helper (cdr loc-list) (toggle-one (car loc-list) board-list))       ;; alternatively, recursively call the helper
    )
)
;; ENDOF: ToggleRelated ----------------------------------------------------------

 ;; Function to check whether all lights are out
 ;; Returns a boolean value
(defun lights-out (list)
    (if (and (null (cdr list)) (= 0 (car list)))        ;; if the rest of the list is empty & car is 0
        t                                               ;; return true
        (if (not (= 0 (car list)))                      ;; if the value is not 0, return nil
            nil
            (lights-out (cdr list))                     ;; alternatively recurse next
        )
    )
)

;; Row & Column Parsing ----------------------------------------------------------------
;; All functions in this section take in a location, list and board-size as arguments (n list board-size)

;; Returns Previous Column Location or NIL if it does not exist
(defun prev-col (n list board-size)
    (if (= 0 (mod n board-size))    ;; if n % board-size == 0
        NIL                         ;; then return nil
        (- n 1)                     ;; alternatively, return the previous element
    )
)

;; Returns Next Column Location or NIL if it does not exist
(defun next-col (n list board-size)
    (if (or (> n (- (length list) 1)) (= 0 (mod (+ n 1) board-size)))   ;; if n + 1 % board-size == 0 or it is the last element
        NIL                                                             ;; return nil
        (+ n 1)                                                         ;; alternatively, return the next element
    )
)

;; Returns Previous Row Location or NIL if it does not exist
(defun prev-row (n list board-size)
    (if (< n board-size)        ;; if n is less than board size (side size)
        NIL                     ;; return nil
        (- n board-size)        ;; alternatively, return n minus board-size
    )
)


;; Return Next Row Location or NIL if it does not exist
(defun next-row (n list board-size)
    (if (> n (- (length list) (+ 1 board-size)))   ;; if the location is more than the length - (board-size + 1)
        NIL                                        ;; Return 0
        (+ n board-size)                           ;; Alternatively, return n plus board-size
    )
)
;; ENDOF: Row & Column Parsing -----------------------------------------------------------


;; Test Functions ------------------------------------------------------------------------

(defun test-toggle ()
    (let ((test (toggle 3 '(0 0 0 0 0 0 0 0 0) 3)))
        (if (equal test '(1 0 0 1 1 0 1 0 0))
            (format t "Passed~%")
            (format t "Failed~%")
        )
        (print-board test)
    )
)


(defun test-toggle-one ()
    (let ((test (toggle-one 0 '(0 0 0 0 0 0 0 0 0))))
        (if (equal test '(1 0 0 0 0 0 0 0 0))
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
)


(defun test-next-row ()
    (let ((test (next-row 0 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 3)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (next-row 1 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 4)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (next-row 2 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 5)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    (let ((test (next-row 3 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 6)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (next-row 4 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 7)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (next-row 5 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 8)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    (let ((test (next-row 6 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test NIL)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (next-row 7 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test NIL)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (next-row 8 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test NIL)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
)


(defun test-prev-row ()
    (let ((test (prev-row 0 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test NIL)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (prev-row 1 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test NIL)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (prev-row 2 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test NIL)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    (let ((test (prev-row 3 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 0)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (prev-row 4 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 1)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (prev-row 5 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 2)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    (let ((test (prev-row 6 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 3)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (prev-row 7 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 4)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (prev-row 8 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 5)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
)



(defun test-prev-col ()
    (let ((test (prev-col 0 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test NIL)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (prev-col 1 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 0)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (prev-col 2 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 1)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    (let ((test (prev-col 3 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test NIL)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (prev-col 4 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 3)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (prev-col 5 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 4)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    (let ((test (prev-col 6 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test NIL)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (prev-col 7 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 6)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (prev-col 8 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 7)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
)


(defun test-next-col ()
    (let ((test (next-col 0 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 1)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (next-col 1 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 2)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (next-col 2 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test NIL)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    (let ((test (next-col 3 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 4)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (next-col 4 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 5)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (next-col 5 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test NIL)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    (let ((test (next-col 6 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 7)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (next-col 7 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test 8)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    
    (let ((test (next-col 8 '(0 1 2 3 4 5 6 7 8) 3)))
        (if (equal test NIL)
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
)


(defun test-toggle-old ()
    (let ((test (toggle 4 '(0 1 0 1 1 1 0 1 0) 3)))
        (if (equal test '(0 0 0 0 0 0 0 0 0))
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    (let ((test (toggle 0 '(0 1 0 1 1 1 0 1 0) 3)))
        (if (equal test '(1 0 0 0 1 1 0 1 0))
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
    (let ((test (toggle 7 '(0 1 0 1 1 1 0 1 0) 3)))
        (if (equal test '(0 1 0 1 0 1 1 0 1))
            (format t "Passed~%")
            (format t "Failed~%")
        )
    )
)

;; ENDOF: Test Functions -----------------------------------------------------------------
