
;; Conway's Game of Life
;; Author: Jaseem Abid <jaseemabid@gmail.com>

(setq gen '((0 0 0 1 0 0 0 1)
			(0 1 0 0 0 0 0 1)
			(0 0 0 1 0 0 0 1)
			(0 1 0 0 0 1 0 1)
			))

(defun show-gen ()

  (let ((p (point)))

	(progn
	  ;; Jump to the end of the buffer
	  (goto-char (point-max))

	  ;; Display grid
	  (loop for row in gen
			do
			(insert (concat "\n"
							(loop for i in row
								  collect (if (= i 0) 32 95 )
								  )))
			)

	  ;; Go back to where cursor was before
	  (goto-char p)
	  )
	)
  )

(show-gen)
