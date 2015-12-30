;;; life.el --- Game of life

;; Author: Jaseem Abid <jaseemabid@gmail.com>
;; Created: Mon Oct 7 19:39:19 2013 +0530
;; Keywords: game, game-of-life, zero-player-game

;; This file is not part of GNU Emacs.

;; The MIT License (MIT)

;; Copyright (C) 2014 Jaseem Abid <jaseemabid@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Install:
;; Refer to README

;;; Commentary:
;; A simple program to play game of life in a Emacs buffer

;;; Code:
(eval-and-compile
  (require 'cl))

;; [todo] - Use a standard format for reading cell files
(load-file "./patterns.el")

;;; Settings
;;;; Custom Groups

(defgroup life nil
  "Game of life implementation in Emacs lisp."
  ;; :prefix "life-"
  :group 'games)

(defcustom life-buffer "*life*"
  "Name of the buffer to show the game."
  :type 'string
  :group 'life)

(defcustom life-alive-char ?*
  "Character that represents a alive point.

Ideally this should be a good unicode character that fills up a
complete block and have the same size as a `life-dead-char' to
prevent jerks.  Depends on the font used.

Alternatives: ● ◌"
  :type 'character
  :group 'life)

(defcustom life-dead-char 32
  "Character that represents a dead point.

Refer: `life-alive-char'"
  :type 'character
  :group 'life)

(defcustom life-pattern (gethash 'gun life-patterns)
  "Default pattern to start the game.

[todo] - Read a standard cell file instead of a list"
:group 'life)

(defvar life-timer nil
  "The main timer.")

(defun life-show-gen (gen)
  "Render a generation GEN of life.

[todo] - Consider lazy render.  Change what needs to be changed."
  (erase-buffer)
  ;; Display grid
  (loop for row in gen
        do
        (insert (concat (loop for i in row
                          collect (if (= i 0)
                                          life-dead-char
                                        life-alive-char)) "\n" ))))

(defun life-get-neighbour (x y)
  "Get item at coordinates X and Y of life-pattern."
  (if (and (> x -1) (> y -1))
      (nth x (nth y life-pattern))
    nil))

(defun life-count-neighbours (x y)
  "Return the number of alive neighbors item at X Y has.

Ref: `life-get-neighbor'."
  (count 1 (list
            (life-get-neighbour (- x 1) (- y 1))
            (life-get-neighbour x (- y 1))
            (life-get-neighbour (+ x 1) (- y 1))

            (life-get-neighbour (- x 1) y)
            (life-get-neighbour (+ x 1) y)

            (life-get-neighbour (- x 1) (+ y 1))
            (life-get-neighbour x (+ y 1))
            (life-get-neighbour (+ x 1) (+ y 1)))))

;; 1. Any live cell with fewer than two live neighbors dies, as if caused by
;; under-population.
;; 2. Any live cell with two or three live neighbors lives on to the next gen
;; 3. Any live cell with more than three live neighbors dies, as if by crowding
;; 4. Any dead cell with exactly three live neighbors becomes a live cell, as
;; if by reproduction.

(defun life-mutate (gen)
  "Take a GEN as arg and return the next."
  (loop for row in gen
        for i from 0
        collect (loop for cell in row
                      for j from 0
                      collect (let ((n (life-count-neighbours j i)))
                                (if (= cell 1)
                                    ;; Alive cell, rules 1 - 3
                                    (case n
                                      (0 0)
                                      (1 0)
                                      (2 1)
                                      (3 1)
                                      (otherwise 0))
                                  ;; Dead cell, rule 4
                                  (if (= n 3) 1 0))))))

(defun life-progress ()
  "Step one generation."
  (if (string= (buffer-name) life-buffer)
      ;; Run in own buffer only
      (let ((next (life-mutate life-pattern)))
        (life-show-gen next)
        ;; Avoid this mutation if possible
        (setq life-pattern next))

    ;; Kill on buffer change,
    (progn
      (message "Exit from game of life, cleanup")
      (cancel-timer life-timer)
      (kill-buffer (get-buffer-create life-buffer)))))

(defun game-of-life ()
  "Game of life."
  (interactive)
  (switch-to-buffer (get-buffer-create life-buffer))
  (setq life-timer (run-with-timer 0 0.25 'life-progress)))

;; (game-of-life)

(provide 'life)

;;; life.el ends here
