;;; sboo-xah.el --- Xah Lee functions
;;; -*- lexical-binding: t; -*-

;;==============================================;;
;; Copyright (C) GPL-3.0-or-later 2019 Spiros Boosalis

;; Author: Spiros Boosalis <samboosalis@gmail.com>
;; Author: Xah Lee

;; This file is NOT part of GNU Emacs.

;;==============================================;;
;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;==============================================;;
;;; Commentary:

;; Functions written by Xah Lee,
;; and modified by Spiros Boosalis.
;;

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;;; Requirements -------------------------------;;
;;----------------------------------------------;;

(require 'cl-lib)
(require 'pcase)
(require 'seq)
(require 'subr-x)

;;----------------------------------------------;;
;;; Commands -----------------------------------;;
;;----------------------------------------------;;

(defun xah-forward-block (&optional n)
 "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
 (interactive "p")
 (let ((n (if (null n) 1 n)))
   (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" n)))

;;----------------------------------------------;;

(defun xah-backward-block (&optional n)
 "Move cursor to previous text block.
See: `xah-forward-block'
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
 (interactive "p")
 (let ((n (if (null n) 1 n))
       ($i 1))
   (while (<= $i n)
     (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
         (progn (skip-chars-backward "\n\t "))
       (progn (goto-char (point-min))
              (setq $i n)))
     (setq $i (1+ $i)))))

;;==============================================;;
(provide 'sboo-xah)

;; Local Variables:
;; End:

;;; sboo-xah.el ends here