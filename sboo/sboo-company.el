;;; sboo-company.el --- Personal `company-mode' configuration -*- lexical-binding: t -*-

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

;; Personal `company-mode' configuration.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile 
  (require 'pcase)
  (require 'cl-lib))

(progn
  (require 'seq))

;;==============================================;;

(progn
  (require 'company))

;;----------------------------------------------;;
;; Utilities: ----------------------------------;;
;;----------------------------------------------;;

;; (defun sboo-company/register-backends (variable value)
;;   "Register `sboo-company-backends'."
;;   (setq company-backends sboo-company-backends))

;; (defun cwm--set-and-recenter-windows (var val)
;;   "Set customizable variable VAR to VAL and recenter windows.
;; All windows in all frames are recentered.
;; This is intended for use as the `setfunction' of a
;; `defcustom'. See Info node `(elisp) Variable Definitions'."
;;   (set-default var val)
;;   (dolist (frame (frame-list))
;;     (with-selected-frame frame
;;       (cwm-center-windows))))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-company nil

  "Personal `company' customization."

  :prefix 'sboo
  :group 'sboo)

;;==============================================;;

(defcustom sboo-company-backends

  '(

    (
     company-capf      ; `completion-at-point' Functions.
     )

    (
     company-keywords  ; `prog-mode' Keywords.
     company-etags     ; `prog-mode' TAGS.
     )

    (
     company-files     ; Filepaths.
     )

    (
     company-abbrev    ; Abbreviations.
     company-dabbrev   ; Dynamic Abbreviations.
     )

    (
     company-ispell    ; Spell-Checking.
     )

    (
     company-yasnippet ; `yasnippet'.
     )

    )

  "Personal `company-backends', ordered by priority.

each Company Backend in a group has a higher priority than
any Company Backend in a later group (and a lower priority than
any Company Backend in an earlier group).

`listp' of `listp's of `symbolp's."

  :type '(repeat (function :tag "Company Backend"))

  ;; :set #'sboo-company/register-backends

  :safe #'listp
  :group 'sboo-company)

;; ^ Default (/ Global) `company-backends':

;;----------------------------------------------;;

(defcustom sboo-company-frontends

  '(company-pseudo-tooltip-unless-just-one-frontend
    company-echo-metadata-frontend
    company-preview-frontend)

  "Personal `company-frontends'.

a `listp' of `functionp's."

  :type '(repeat (function :tag "Company Frontend"))

  :safe #'listp
  :group 'sboo)

;;----------------------------------------------;;
;; Company Backends ----------------------------;;
;;----------------------------------------------;;
;; Mode-specific `company-backends': Text

(defun sboo-company-text-setup ()

  "Setup `company-mode' for `text-mode' deriveés."

  (add-to-list (make-local-variable 'company-backends)
               #'company-ispell)

  ;; ^ the `company-ispell' backend completes words.

  (setq company-ispell-dictionary (file-truename "~/.emacs.d/misc/english-words.txt"))

  ;; OPTIONAL, if `company-ispell-dictionary' is nil, `ispell-complete-word-dict' is used
  ;;  but I prefer hard code the dictionary path. That's more portable.

  ())

;; ^ `company-backends':
;;
;; • must be buffer-local, otherwise
;;   modifying it affects completion across all major modes.
;;

;;==============================================;;
;; Mode-specific `company-backends': Haskell

;;==============================================;;
;; Mode-specific `company-backends': ELisp

;;==============================================;;
;; Mode-specific `company-backends': bash

;;==============================================;;
;; Mode-specific `company-backends': Python

(defun sboo-company-python-setup ()

  "Setup `company-mode' for `python-mode'."

  (add-to-list (make-local-variable 'company-backends)
               #'company-anaconda))

;;==============================================;;
;; Mode-specific `company-backends': JavaScript

(defun sboo-company-javascript-setup ()

  "Setup `company-mode' for `javascript-mode'."

  (tern-mode t)

  (add-to-list (make-local-variable 'company-backends)
               #'company-tern))

;;----------------------------------------------;;
;; Company Frontends ---------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-toggle-company-ispell (&optional argument)

  "Toggle the `company-ispell' backend.

URL `http://blog.binchen.org/posts/emacs-auto-completion-for-non-programmers.html'"
  
  (interactive "P")

  (cond
   
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "‘company-ispell’ disabled"))

   (t
    (add-to-list 'company-backends 'company-ispell)
    (message "‘company-ispell’ enabled"))))

;;==============================================;;

;; `company-complete-number' utilities:
;;
;; (company-complete-number N)
;; Insert the Nth candidate visible in the tooltip.

;;----------------------------------------------;;

(defun sboo-company-complete-1 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 1))

;;----------------------------------------------;;

(defun sboo-company-complete-2 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 2))

;;----------------------------------------------;;

(defun sboo-company-complete-3 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 3))

;;----------------------------------------------;;

(defun sboo-company-complete-4 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 4))

;;----------------------------------------------;;

(defun sboo-company-complete-5 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 5))

;;----------------------------------------------;;

(defun sboo-company-complete-6 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 6))

;;----------------------------------------------;;

(defun sboo-company-complete-7 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 7))

;;----------------------------------------------;;

(defun sboo-company-complete-8 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 8))

;;----------------------------------------------;;

(defun sboo-company-complete-9 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 9))

;;----------------------------------------------;;

(defun sboo-company-complete-10 () "Specialized `company-complete-number'."
       (interactive)
       (company-complete-number 10))

;;==============================================;;

(defun sboo-company-complete-common-or-previous-cycle ()

  "Backwards `company-complete-common-or-cycle'."

  (interactive)

  (company-complete-common-or-cycle -1))

;; ^ `company-complete-common-or-cycle' :
;; 
;; > "Insert the common part of all candidates, or select the next one."
;;

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-company-register-backends ()
  "Register `sboo-company-backends'."
  (setq company-backends sboo-company-backends))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; CompanyBackend
;; 
;; Each CompanyBackend has this signature:
;; 
;; (defun company-<name>-backend (COMMAND &optional _ &rest _) ...)
;; 
;; where:
;; 
;; - `COMMAND' is a `symbol' (see below)
;; - 
;; - 
;; 

;; `company-backends'
;;
;; `company-backends' default value is:
;;
;; (`company-bbdb'
;;  `company-nxml'
;;  `company-css'
;;  `company-eclim'
;;  `company-semantic'
;;  `company-clang'
;;  `company-xcode'
;;  `company-cmake'
;;  `company-capf'
;;  `company-files'
;;  (`company-dabbrev-code'
;;   `company-gtags'
;;   `company-etags'
;;   `company-keywords')
;;  `company-oddmuse'
;;  `company-dabbrev')
;;
;; `company-backends' type is a `list', where each item is either:
;;
;; - a backend; has type `function'.
;; - a backend group; has type `list' of `function'.
;; 

;; CompanyCommand
;; 
;; A CompanyBackend takes a CompanyCommand.
;; 
;; CompanyCommand is a `symbol', one of:
;; 

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-company)

;;; sboo-company.el ends here