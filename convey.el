;; Conway's Game of Life
;; Author: Jaseem Abid <jaseemabid@gmail.com>

(setq
 buffer (get-buffer-create "game-of-life")
 gen '((0 0 0 1 0 0 0 1)
	   (0 1 0 0 0 0 0 1)
	   (0 0 0 1 0 0 0 1)
	   (0 1 0 0 0 1 0 1)
	   ))

(defun show-gen ()
  (progn
	  (switch-to-buffer buffer)
	  (erase-buffer)

	  ;; Display grid
	  (loop for row in gen
			do
			(insert (concat "\n"
							(loop for i in row
								  collect (if (= i 0) 32 95 )
								  )))
			)
	  )
  )

(show-gen)
