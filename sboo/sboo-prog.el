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

;; .
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile 
  (require 'cl-lib))

;;----------------------------------------------;;

(progn
  (require 'pcase)
  (require 'seq))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;TODO sboo-prog-new
;: e.g. sboo-haskell-new-module in haskell-mode

;;TODO (indent-according-to-mode)
;; indent-line-function



;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;








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








;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for `keywords'.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'keywords)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlight long lines:

(defvar sboo-long-line-length 80

  "How long a line is too long? In columns / characters.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-color-long-lines ()

  "Highlight over-long lines

See `sboo-long-line-length' (e.g ≥80 columns)."
  (interactive)

  (let ((REGEX
         (concat "^"
                 "[^\n]"
                 (format "\\{%d\\}" sboo-long-line-length)
                 "\\(.*\\)"
                 "$")))

    (font-lock-add-keywords nil `((,REGEX
                                   1
                                   font-lock-warning-face
                                   t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlight keywords:

(defcustom sboo-comment-keywords

  '( "TODO"
     "NOTE"
     "TEST"
     "FIXME"
     "BUG"
   )

  "Keywords (to highlight) within comments.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sboo-comment-keywords-haskell

  '( "<BLANKLINE>"
   )

  "Keywords (to highlight) within comments, under `haskell-mode'.")

;;TODO "<BLANKLINE>" for Haskell comments (a `doctest' keyword).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO (defvar sboo--cached--comment-keywords 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-regexp-of-strings (STRINGS)
 
  "A regexp that matches a string in `STRINGS'."

  (when (require 's nil :noerror);;TODO manage depencies better

    (s-wrap (s-join "\\|" STRINGS) "\\(" "\\)")))

;; ^ M-: (sboo-regexp-of-strings sboo-comment-keywords)
;;
;;     → (s-between "\\(" "\\)" (s-intercalate "\\|" '( "TODO" "NOTE" "TEST" "FIXME" "BUG" )))
;;
;;     → "\\(TEST\\|TODO\\|FIXME\\|BUG\\|NOTE\\)"
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-color-comment-keywords ()

  "Highlight universal keywords within comments.

See `sboo-comment-keywords' (e.g. NOTE and TODO)."
  (interactive)

  (let ((REGEX (concat "\\<" (sboo-regexp-of-strings sboo-comment-keywords)))
        )

  (font-lock-add-keywords nil `((,REGEX
                                 1
                                 font-lock-warning-face
                                 prepend)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `prog-mode' Hooks:

(defvar sboo-prog-mode-hooks

        '( #'sboo-color-long-lines
           #'sboo-color-comment-keywords
         )

  "For `prog-mode-hook' (i.e. all program files).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;###autoload(defvar fixme-mode nil)
;; (define-minor-mode fixme-mode  

;;   "A minor mode for making FIXME and other warnings stand out"

;;   nil
;;   " Fixme"
;;   nil
;;   :global

;;   :group fixme-mode
;;   :version fixme-mode-version


;;   (if fixme-mode
;;       (fixme-reload-keywords)
;;   (fixme-remove-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;; 
;; 
;;

;;; Links
;;
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-prog)