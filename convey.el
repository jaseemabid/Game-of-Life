;; Conway's Game of Life
;; Author: Jaseem Abid <jaseemabid@gmail.com>

(setq
 gen '((0 0 0 1 0 0 0 1)
	   (0 1 0 0 0 0 0 1)
	   (0 0 0 1 0 0 0 1)
	   (0 1 0 0 0 1 0 1)
	   ))

(defun show-gen ()
  (erase-buffer)
  ;; Display grid
  (loop for row in gen
		do
		(insert (concat (loop for i in row
							  collect (if (= i 0) 32 176)) "\n" )
				))
  )

(defun mutate ()
  "Take a gen as arg and return the next"
  (setq result ())
  (loop for row in gen
		do
		(setq tmp ())
		(loop for i in row
			  do
			  (push (if (= i 0) 1 0) tmp)
			  )
		(push tmp result)
		)
  (setq gen result)
  )

(defun progress ()
  (mutate)
  (show-gen)
  )

(defun gol-cleanup ()
  "Exit from game of life, cleanup"
  (if (string= (buffer-name) "game-of-life")
	  (progn
		(message "Exit from game of life, cleanup")
		(cancel-timer timer)))
  )

(defun game-of-life ()
  "Game of life"
  (interactive)
  (switch-to-buffer (get-buffer-create "game-of-life"))
  (setq timer (run-with-timer 0 1 'progress))
  (add-hook 'kill-buffer-hook 'gol-cleanup)
  )

(game-of-life)
