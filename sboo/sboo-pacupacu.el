;;; sboo-pacupacu.el --- By pacupacu -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 27 Jun 2019
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

;; Commands written by pacupacu, modified by sboosali.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile
  (require 'rx)
  (require 'pcase))

;;----------------------------------------------;;

(progn
  (require 'seq)
  (require 'cl-lib))




(defun pacupacu/replace-convert-to-sexp-and-kill-new-rx (beg end)

  "`replace-regexp' with `rx' syntax.

=== Usage ===

• M-x pacupacu/replace-convert-to-sexp-and-kill-new-rx RET
  (and bol (group nonl)) RET
  \,(upcase \1) RET

URL `https://www.reddit.com/r/emacs/comments/36qbzn/comment/crgapy8'"

  (interactive "*r")

  (let ((strs (split-string (buffer-substring-no-properties beg end)
                            pacupacu/replace-from-to-separator)))
    (kill-new (prin1-to-string
               (pacupacu//replace-convert-to-sexp (rx-to-string (read (car strs)))
                                            (cadr strs))))))

;;

(defvar pacupacu/replace-from-to-separator (char-to-string 20))

(defun pacupacu//replace-convert-to-sexp (regexp to)
  `(replace-regexp ,regexp (query-replace-compile-replacement ,to t)))

(defun pacupacu/replace-convert-to-sexp-and-kill-new (beg end)
  (interactive "r")
  (let ((strs (split-string (buffer-substring-no-properties beg end)
                            pacupacu/replace-from-to-separator)))
    (kill-new (prin1-to-string
               (pacupacu//replace-convert-to-sexp (car strs) (cadr strs))))))

;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-pacupacu)

;;; sboo-pacupacu.el ends here
