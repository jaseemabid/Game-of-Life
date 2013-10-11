;; Conway's Game of Life
;; Author: Jaseem Abid <jaseemabid@gmail.com>

(setq
 life-buffer "*life*"
 alive 176
 dead 32
 glider '((0 1 0 0 0 0 0 0)
		  (0 0 1 0 0 0 0 0)
		  (1 1 1 0 0 0 0 0)
		  (0 0 0 0 0 0 0 0)
		  (0 0 0 0 0 0 0 0)
		  (0 0 0 0 0 0 0 0)
		  (0 0 0 0 0 0 0 0)
		  (0 0 0 0 0 0 0 0))
 gen glider
 )

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

;; 1. Any live cell with fewer than two live neighbours dies, as if caused by
;; under-population.
;; 2. Any live cell with two or three live neighbours lives on to the next
;; generation.
;; 3. Any live cell with more than three live neighbours dies, as if by
;; overcrowding.
;; 4. Any dead cell with exactly three live neighbours becomes a live cell, as
;; if by reproduction.

(defun mutate ()
  "Take a gen as arg and return the next"
  (loop for row in gen
		for i from 0
		collect (loop for cell in row
					  for j from 0
					  do
					  (setq n (count-neighbours i j))
					  collect  (if cell
								   ;; Alive cell, rules 1 - 3
								   (case n
									 (0 0)
									 (1 0)
									 (2 1)
									 (3 1)
									 (otherwise 0 ;; "Am I dead or alive?"
												))
								 ;; Dead cell, rule 4
								 (if (= n 3) 1 0)
								 )
					  )))

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
