;;; sboo-increment.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://emacsredux.com/blog/2013/07/25/increment-and-decrement-integer-at-point/
;; Keywords: convenience
;; Created: 16 Jul 2019
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; DWIM: Increment/Decrement ‘region’ or ‘thinatpt’.
;; 
;; “I have a key binding which implements a similar idea. I wanted to be able to increment or decrement all numbers in a region (for operations like duplicate line, increment index). Also works nicely together with multiple-cursors. The nice thing, if no region is active it will take the symbol-at-point and this way behave similar like your functions. Higher increments/decrements can be passed as universal-argument.”
;;
;; Written by Daniel on URL ‘https://emacsredux.com/blog/2013/07/25/increment-and-decrement-integer-at-point/’
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(progn
  (require 'thingatpt)
  (require 'cl-lib))

;;----------------------------------------------;;
;;; Commands -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-increment-number-dwim (&optional beg end arg)

  "Increment all decimal numbers in region between `beg' and
`end' by `arg'. If no prefix arg is given, increment by 1. If the
mark is not active, try to build a region using
`symbol-at-point'."

  (interactive "r\np")

  (or arg (setq arg 1))
  (unless (and mark-active transient-mark-mode)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if bounds (setq beg (car bounds) end (cdr bounds)))))
  (if (< end beg)
      (let ((tmp end))
        (setq beg end end tmp)))
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "-?[0-9]+" end t)
      (replace-match (number-to-string (+ arg (string-to-number (match-string 0)))))))
  (setq deactivate-mark nil))

;;----------------------------------------------;;

(defun sboo-decrement-number-dwim (&optional beg end arg)

  "Decrement all decimal numbers in region between `beg' and
`end' by `arg'. If no prefix arg is given, increment by 1. If the
mark is not active, try to build a region using
`symbol-at-point'."

  (interactive "r\np")

  (or arg (setq arg 1))
  (unless (and mark-active transient-mark-mode)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if bounds (setq beg (car bounds) end (cdr bounds)))))
  (sboo-increment-number-dwim beg end (- arg)))

;;----------------------------------------------;;

(defalias 'sboo-increment-dwim #'sboo-increment-number-dwim)
(defalias 'sboo-decrement-dwim #'sboo-decrement-number-dwim)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-increment)

;;; sboo-increment-number.el ends here