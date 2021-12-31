;; LIGHTS OUT AI PROJECT
;; @author Seymour Pashayev

;; Load the game file
(load "game.lisp")

;; Take one guess at a target number.
;; When the lights are out, the game quits
;; Otherwise makes a recursive call to guess again.
;; Parameters: board, message, count, board-size
(defun one-guess (board message count board-size)
    (if (> count 450)
        (progn (format t "~%The AI took more than 450 turns to complete the game, terminating...")
               (exit))
    )

    (print-board board-size board)
    (if (lights-out board)
        (format T "~%You won after ~D turns.~%" count)
        (let    (
                (guess (get-input-from-ai board board-size))
                )
            (one-guess (toggle guess board board-size) "~%Pick a bit to switch: " (+ 1 count) board-size)
        )
    )
)


;; Basic input from AI
(defun get-input-from-ai (board board-size)
    (let ((loc (random (length board))))
        (if (> (locations-around-exist loc board board-size) 5)
            loc
            (random (length board))
        )
    )
    
)


(defun locations-around-exist (n list bs)
        (let ((locations-to-toggle (list
                                (prev-row n list bs)
                                (prev-col n list bs)
                                n
                                (next-col n list bs)
                                (next-row n list bs))))
        (non-nulls-count locations-to-toggle)
    )
)

;; Function to count non-nulls in the list
(defun non-nulls-count (list)
    (if (null (cdr list))
        1
        (if (null (car list))
            (mycount (cdr list))
            (+ 1 (mycount(cdr list)))
        )
        
    )
)

;; Function to end the AI playthrough
(defun lost-game ()
    
)
