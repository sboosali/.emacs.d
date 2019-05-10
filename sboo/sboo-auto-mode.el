;;; sboo-auto-mode.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 10 May 2019
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

;; Utilities for configuring `*-mode-alist'.
;; 
;; Functions:
;;
;; • ‘sboo-add-auto-mode-file-extension’ — registers file-extensions.
;;   e.g. « *.conf ».
;;
;; • ‘sboo-add-auto-mode-basename’— registers base-names. 
;;   e.g. « README ».
;;
;; Requires:
;;
;; • `auto-mode-alist'
;; • `interpreter-mode-alist'
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile 
  (require 'cl-lib))

;;----------------------------------------------;;

(progn
  (require 'seq))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-add-auto-mode-file-extension (file-extension mode &key pure suffix)

  "Associates the FILE-EXTENSION with the MODE, in `auto-mode-alist'.

Inputs:

• PURE — a `booleanp'.
  Whether to perform mutation (on a global variable).
  impure by default.

• SUFFIX — a `booleanp'.
  (Same as the « APPEND » option in `add-to-list'.)
  prepends by default.

Output:

• a `consp'.

Effects:

• mutates `auto-mode-alist' (unless PURE is set).

Examples:

• M-: (sboo-add-auto-mode-file-extension \"hs\" 'haskell-mode :pure t)
    ⇒ (\"\\.hs\\'\" . 'haskell-mode)"

  (let* ((Pattern
          (concat "\\."
                  file-extension
                  "\\'"))

         (Association
          `(,Pattern . ,mode))
         )

    (progn

      (unless pure
        (progn
          (add-to-list 'auto-mode-alist Association suffix)
          (message "%S" Association)))

      (if pure
          Association
        nil))))

;;----------------------------------------------;;

(cl-defun sboo-add-auto-mode-basename (base-name mode &key pure suffix)

  "Associates the file BASE-NAME with the MODE, in `auto-mode-alist'.

Inputs:

• PURE — a `booleanp'.
  Whether to perform mutation (on a global variable).
  impure by default.

• SUFFIX — a `booleanp'.
  (Same as the « APPEND » option in `add-to-list'.)
  prepends by default.

Output:

• a `consp'.

Effects:

• mutates `auto-mode-alist' (unless PURE is set).

Examples:
 
• M-: (sboo-add-auto-mode-basename \"README.md\" 'gfm-mode :pure t)
    ⇒ (\"README\\.md\\'\" . 'gfm-mode)"

  (let* ((Pattern
          (concat (replace-regexp-in-string "\\." "\\\\." base-name)
                  "\\'"))

         (Association
          `(,Pattern . ,mode))
         )

    (progn

      (unless pure
        (progn
          (add-to-list 'auto-mode-alist Association suffix)
          (message "%S" Association)))

      (if pure
          Association
        nil))))

;; ^ internals:
;;
;; M-: (replace-regexp-in-string "\\." "\\\\." "README.md")
;;     ("\\.hs\\'" . 'haskell-mode)
;;
;; M-: (message "%S" '("x" . y))"
;;     (\"x\" . y)"
;;
;; M-: (message "%s" '("x" . y))
;;     (x . y)"
;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; `auto-mode-alist':
;;
;; e.g. 
;;     (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;
;; The auto-mode-alist variable is an AssociationList that associates MajorModes with a pattern to match a buffer filename when it is first opened.
;;

;; NOTE
;; 
;; A \' matches the end of a string, whereas $ matches the empty string before a newline. Thus, $ may lead to unexpected behavior when dealing with filenames containing newlines (admittedly uncommon).
;; 
;; The \. matches ‘.’ (a period); ‘.’ must be escaped by a backslash because the period is a special character in Regular Expressions.
;; 
;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-auto-mode)

;;; sboo-auto-mode.el ends here