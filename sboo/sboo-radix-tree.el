;;; sboo-radix-tree.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 09 Jun 2019
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

;; Utilities for `radix-tree-p's.
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
  (require 'radix-tree)
  (require 'seq)
  (require 'cl-lib))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-radix-tree-insert-true (tree word)

  "Specialized `radix-tree-insert'.

Reducer for constructing a `radix-tree-p'."

  (radix-tree-insert tree word t))

;;----------------------------------------------;;

(defun sboo-radix-tree-from-seq (words)

  "Return a `radix-tree-p' of WORDS.

Inputs:

• WORDS — a `sequencep' of `stringp's.

Examples:

• M-: (sboo-radix-tree-from-seq '(\"application\" \"appetizer\" \"applicative\" \"apple\"))
  → ((\"app\" (\"l\" (\"icati\" ... ...) (\"e\" . t)) (\"etizer\" . t)))

Links:

• URL `http://justinhj.github.io/2018/10/24/radix-trees-dash-and-company-mode.html'"

  (seq-reduce #'sboo-radix-tree-insert-true words radix-tree-empty))

;;----------------------------------------------;;

(defun sboo-radix-tree-from-file (filepath)

  "Return a `radix-tree-p' from FILEPATH's contents.

Inputs:

• FILEPATH — a `string p'.

Examples:

• M-: (sboo-radix-tree-from-file \"dictionary.txt\")

Links:

• URL `http://justinhj.github.io/2018/10/24/radix-trees-dash-and-company-mode.html'"

   (let* ((TEXT  (with-temp-buffer
                   (insert-file-contents-literally filepath)
                   (buffer-substring-no-properties (point-min) (point-max))))
          (WORDS (split-string TEXT)))

    (sboo-radix-tree-from-seq WORDS)))

;;----------------------------------------------;;

(defun sboo-radix-tree-read (filepath)

  "Read a `radix-tree-p' from FILEPATH.

Inputs:

• FILEPATH — a `string p'.

Examples:

• M-: (sboo-radix-tree-read \"example-radix-tree.el\")

Links:

• URL `http://justinhj.github.io/2018/10/24/radix-trees-dash-and-company-mode.html'"

      (progn

   (save-excursion
 
     (let* ((BUFFER (find-file FILEPATH))
            (TREE   (read BUFFER)))
 
        (kill-buffer BUFFER)))

       (when (radix-tree-p TREE)
         TREE))))))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; Links:
;;
;; • URL `http://justinhj.github.io/2018/10/24/radix-trees-dash-and-company-mode.html'
;; • URL `https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/radix-tree.el'
;; • URL `https://en.wikipedia.org/wiki/Radix_tree'
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-radix-tree)

;;; sboo-radix-tree.el ends here
