;;; sboo-prog.el --- -*- lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 05 May 2019
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

;; Configure `prog-mode' (and `derived-mode-p's).
;; 
;; Syntax Highlighting:
;; 
;; • Keywords: “NOTE”, “TODO”, “FIXME”, ...
;; • 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile 
  (require 'pcase)
  (require 'cl-lib))

;;----------------------------------------------;;

(progn
  (require 'seq))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-prog nil

  "Personal `prog-mode' configuration."

  :prefix 'sboo
  :group 'sboo)

;;==============================================;;

;;; `prog-mode' Hooks:

(defvar sboo-prog-mode-hooks

        '( #'sboo-color-long-lines
           #'sboo-color-comment-keywords
         )

  "Commands for `prog-mode-hook' (i.e. all program files).")

;;----------------------------------------------;;

;;; Highlight Long-Lines:

(defvar sboo-prog-long-line-length 80

  "Defines “How long a line is too long?” In columns / characters.")

;;----------------------------------------------;;

;;; Highlight keywords:

(defcustom sboo-prog-comment-keywords

  '( "TODO"
     "NOTE"
     "TEST"
     "FIXME"
     "BUG"
   )

  "Keywords (to highlight) within comments.")

;;----------------------------------------------;;

(defcustom sboo-prog-comment-keywords-haskell

  '( "<BLANKLINE>"
   )

  "Keywords (to highlight) within `haskell-mode' comments.")

;;TODO "<BLANKLINE>" for Haskell comments (a `doctest' keyword).

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-prog-config ()

  "Configure `prog-mode'."

  ;; (remove-hook 'prog-mode-hook #'turn-on-auto-fill)
  ;; (add-hook 'prog-mode-hook #'turn-on-visual-line-mode)

  'prog-mode-hook)

;;----------------------------------------------;;

(defun sboo-prog-color-long-lines ()

  "Highlight over-long lines

See `sboo-prog-long-line-length' (e.g ≥80 columns)."

  (interactive)

  (let ((REGEX
         (concat "^"
                 "[^\n]"
                 (format "\\{%d\\}" sboo-prog-long-line-length)
                 "\\(.*\\)"
                 "$")))

    (font-lock-add-keywords nil `((,REGEX
                                   1
                                   font-lock-warning-face
                                   t)))))

;;----------------------------------------------;;

(defun sboo-prog-regexp-of-strings (STRINGS)
 
  "A regexp that matches a string in `STRINGS'."

  (when (require 's nil :noerror);;TODO manage depencies better

    (s-wrap (s-join "\\|" STRINGS) "\\(" "\\)")))

;; ^ M-: (sboo-prog-regexp-of-strings sboo-prog-comment-keywords)
;;
;;     → (s-between "\\(" "\\)" (s-intercalate "\\|" '( "TODO" "NOTE" "TEST" "FIXME" "BUG" )))
;;
;;     → "\\(TEST\\|TODO\\|FIXME\\|BUG\\|NOTE\\)"
;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-prog-color-comment-keywords ()

  "Highlight universal keywords within comments.

See `sboo-prog-comment-keywords' (e.g. NOTE and TODO)."
  (interactive)

  (let ((REGEX (concat "\\<" (sboo-prog-regexp-of-strings sboo-prog-comment-keywords)))
        )

  (font-lock-add-keywords nil `((,REGEX
                                 1
                                 font-lock-warning-face
                                 prepend)))))

;;----------------------------------------------;;

;;TODO sboo-prog-new
;: e.g. sboo-haskell-new-module in haskell-mode

;;TODO (indent-according-to-mode)
;; indent-line-function

;;----------------------------------------------;;
;; Mode ----------------------------------------;;
;;----------------------------------------------;;

;; ;;;###autoload
;; (define-minor-mode sboo-keyword-mode  

;;   "Minor mode for highlighting keywords (e.g. Warnings like “TODO”, “FIXME”)."

;;   nil
;;   " KW"
;;   nil
;;   :global

;;   :group fixme-mode
;;   :version fixme-mode-version


;;   (if fixme-mode
;;       (fixme-reload-keywords)
;;   (fixme-remove-keywords)))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-prog)

;;; sboo-prog.el ends here