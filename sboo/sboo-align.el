;;; sboo-align.el --- Customize ‘align’ -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 13 Jun 2019
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

;; Personal ‘align’ customize.
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
  (require 'align)
  (require 'cl-lib))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-align nil

  "Customize ‘align’."

  :prefix 'sboo
  :group 'sboo
  :group 'align)

;;==============================================;;

(defvar sboo-align/rules

  (list '(text-column-whitespace
          (regexp  . "\\(^\\|\\S-\\)\\([ \t]+\\)")
          (group   . 2)
          (modes   . align-text-modes)
          (repeat  . t))
        )

  "`align-rules-list' extensions.

a `listp' of Alignment-Rules.")

;;----------------------------------------------;;

(defvar sboo-align/history-list nil

  "History for `align' commands.

a `listp' of `stringp's.

Related:

• `align-regexp-history'")

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-align/register-alignment-rules ()

  "Extend `align-rules-list'.

Customizes command `align' via variable `align-rules-list'.

Related:

• Gets `sboo-align/rules'
• Visit URL `https://www.emacswiki.org/emacs/AlignCommands#toc8'"

  (dolist (RULE sboo-align/rules)
    (add-to-list 'align-rules-list RULE)))

;;----------------------------------------------;;

(defun sboo-align/setup ()

  "Setup `align'."

  (interactive)

  (progn

    ;;------------------------;;

    (add-hook 'align-load-hook #'sboo-align/register-alignment-rules)

    ;;------------------------;;

    (progn

      (defadvice align-regexp (around align-regexp-with-spaces)
        "Align with spaces only (never use tabs for alignment.)"
        (let ((indent-tabs-mode nil))
          ad-do-it))

      (ad-activate 'align-regexp))

    ;;------------------------;;

    ()))

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-align-regexp (beg end regexp)

  "`align-regexp'."

  (interactive (list
                (region-beginning)
                (region-end)
                (read-string "Align regexp: " nil 'sboo-align/history-list)))

  (align-regexp beg end regexp 0 align-default-spacing nil))

;;----------------------------------------------;;

(defun sboo-align (beg end)

  "Align the region or block."

  (interactive "*r")

  (let* ((HISTORY 'sboo-align/history-list)
         )

    (align beg end)))

;;----------------------------------------------;;

(defun sboo-align-code (beg end &optional arg)

  "Indent & Align."

  (interactive "*r\nP")

  (if (null arg)
      (align beg end)

    (let ((end-mark (copy-marker end))
          )
      (indent-region beg end-mark nil)
      (align beg end-mark))))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; ‘align-*-modes’ include:
;;
;; • `align-text-modes'
;; • `align-lisp-modes'
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-align)

;;; sboo-align.el ends here