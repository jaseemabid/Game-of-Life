;; Conway's Game of Life
;; Author: Jaseem Abid <jaseemabid@gmail.com>

(setq
 life-buffer "*life*"
 alive 176
 dead 32
 gen '((0 0 0 1 0 0 0 1)
	   (1 1 0 1 0 1 0 1)
	   (0 0 0 1 0 0 1 0)
	   (1 1 0 0 0 1 0 1)
	   ))

(defun show-gen ()
  (erase-buffer)
  ;; Display grid
  (loop for row in gen
		do
		(insert (concat (loop for i in row
							  collect (if (= i 0) dead alive)) "\n" )
				)))

(defun get-neighbour (x y)
  (if (and (> x -1) (> y -1))
	  (nth x (nth y gen))
	nil))

(defun count-neighbours (x y)
  (count 1 (list
			(get-neighbour (- x 1) (- y 1))
			(get-neighbour x (- y 1))
			(get-neighbour (+ x 1) (- y 1))

			(get-neighbour (- x 1) y)
			(get-neighbour (+ x 1) y)

			(get-neighbour (- x 1) (+ y 1))
			(get-neighbour x (+ y 1))
			(get-neighbour (+ x 1) (+ y 1))
			)))

(defun mutate ()
  "Take a gen as arg and return the next"
  (loop for row in gen
		collect (loop for cell in row
					  collect (if (= cell 0) 1 0))
		))

(defun progress ()
  (setq gen (mutate))
  (show-gen))

(defun cleanup ()
  "Exit from game of life, cleanup"
  (if (string= (buffer-name) life-buffer)
	  (progn
		(message "Exit from game of life, cleanup")
		(cancel-timer timer))))

(defun game-of-life ()
  "Game of life"
  (interactive)
  (switch-to-buffer (get-buffer-create life-buffer))
  (setq timer (run-with-timer 0 1 'progress))
  (add-hook 'kill-buffer-hook 'cleanup))

;; (game-of-life)
