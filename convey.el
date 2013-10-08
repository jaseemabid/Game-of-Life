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

(defun game-of-life ()
  "Game of life"
  (interactive)
  (switch-to-buffer (get-buffer-create "game-of-life"))
  (show-gen)
  )

(game-of-life)
