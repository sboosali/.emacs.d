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

;;----------------------------------------------;;

(progn
  (require 'subr-x)
  (require 'seq))

;;==============================================;;

(progn
  (require 'company)
  (require 'company-ispell))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-company-register-backends ()
  "Register `sboo-company-backends' with `company-backends'."
  (setq company-backends sboo-company-backends))

;;----------------------------------------------;;

(defun sboo-company-register-frontends ()
  "Register `sboo-company-frontends' with `company-frontends'."
  (setq company-frontends sboo-company-frontends))

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-company/customization-changed (variable value)

  "Callback handling a `sboo-company' customization being changed.

Effects:

• Set VARIABLE to VALUE,
• Register the VALUE with `company'.
• Reload `company-mode' (TODO)."

  (set-default variable value)

  (pcase variable

    ('sboo-company-backends (sboo-company-register-backends))
    ('sboo-company-frontends (sboo-company-register-frontends))

    (_ ())))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-company nil

  "Personal `company'."

  :prefix 'sboo
  :group 'sboo
  :group 'company)

;;==============================================;;

(defcustom sboo-company-backends

  '(

    (
     company-files     ; Filepaths.
     )

    (:separate
     company-capf      ; `completion-at-point' Functions.
     company-keywords  ; `prog-mode' Keywords.
     company-etags     ; `prog-mode' TAGS.
     :with
     company-yasnippet    ; `yasnippet'.
     )

    ;; (
    ;;  company-ispell    ; Spell-Checking.
    ;;  )


    ;; the Backends below are all in the last Backend-Grouping
    ;; because their ‘prefix’ bool command always returns non-nil
    ;; (even when their ‘candidates’ list command is empty).

    (
     company-abbrev       ; Abbreviations.
     company-dabbrev      ; Dynamic Abbreviations.
     company-dabbrev-code ;
     )

    )

  "Personal `company-backends', ordered by priority.

each Company Backend in a group has a higher priority than
any Company Backend in a later group (and a lower priority than
any Company Backend in an earlier group).

`listp' of `listp's of `symbolp's."

  :type '(repeat (function :tag "Company Backend"))

  :set #'sboo-company/customization-changed

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

  :set #'sboo-company/customization-changed

  :safe #'listp
  :group 'sboo-company)

;;----------------------------------------------;;
;; Company Backends ----------------------------;;
;;----------------------------------------------;;
;; `text-mode'-specific `company-backends'...

(defun sboo-company-text-setup ()

  "Setup `company-mode' for `text-mode' deriveés."

  (make-local-variable 'company-backends)
  (add-to-list 'company-backends #'company-ispell :append)

  ;; ^ the `company-ispell' backend completes (natural-language) words.

  (setq-local company-ispell-dictionary (sboo-data-file "english-words.txt"))

  ;; ^ “if `company-ispell-dictionary' is nil, `ispell-complete-word-dict' is used but I prefer hard code the dictionary path. That's more portable.”

  ())

;; ^ `company-backends':
;;
;; • must be buffer-local, otherwise
;;   modifying it affects completion across all major modes.
;;

;;==============================================;;
;; `prog-mode'-specific `company-backends'...

(defun sboo-company-prog-setup ()

  "Setup `company-mode' for `prog-mode' deriveés."

  ;; (make-local-variable 'company-backends)
  ;; (add-to-list 'company-backends #'company- :append)

  ())

;;==============================================;;
;; Haskell-specific `company-backends'.

(defun sboo-company-haskell-setup ()

  "Setup `company-mode' for `haskell-mode'."

  ;; (make-local-variable 'company-backends)
  ;; (add-to-list 'company-backends #'company- :append)

  ;; (make-local-variable 'company-transformers)
  ;; (add-to-list 'company-transformers #'sboo-company-transformer-bury-haskell-private-methods :append)

  ())

;;==============================================;;
;; ELisp-specific `company-backends'.

(defun sboo-company-elisp-setup ()

  "Setup `company-mode' for `emacs-lisp-mode'."

  ;; (make-local-variable 'company-backends)
  ;; (add-to-list 'company-backends #'company- :append)

  (make-local-variable 'company-transformers)
  (add-to-list 'company-transformers #'sboo-company-transformer-elisp-bury-private-or-obsolete-symbols :append)

  ())

;; ^ `company-transformers':
;;
;; • each Company-Transformer is an Endomorphism.
;;

;;==============================================;;
;; Bash-specific `company-backends'.

;;==============================================;;
;; Nix-specific `company-backends'.

(defun sboo-company-nix-setup ()

  "Setup `company-mode' for `nix-mode'."

  (make-local-variable 'company-backends)

  (when (require 'nix-company nil :no-error)
    (add-to-list 'company-backends #'company-nix :append))

  ;; (make-local-variable 'company-transformers)
  ;; (add-to-list 'company-transformers #'sboo-company-transformer-bury-nix-private-methods :append)

  ())

;;==============================================;;
;; Python-specific `company-backends'.

(defun sboo-company-python-setup ()

  "Setup `company-mode' for `python-mode'."

  (make-local-variable 'company-backends)
  (add-to-list 'company-backends #'company-anaconda))

;;==============================================;;
;; JavaScript-specific `company-backends'.

(defun sboo-company-javascript-setup ()

  "Setup `company-mode' for `javascript-mode'."

  (tern-mode t)

  (add-to-list (make-local-variable 'company-backends)
               #'company-tern))

;;----------------------------------------------;;
;; Company Frontends ---------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Company Transformers ------------------------;;
;;----------------------------------------------;;
;; `company-transformers':

(defun sboo-company-transformer-elisp-bury-private-or-obsolete-symbols (candidates)

  "Company-Transformer to deprioritize private/obsolete Elisp symbols.

Reorder CANDIDATES by “burying” any candidate satisfying
`sboo-company-elisp-symbol-private-or-obsolete-p', to the end of the
list (which should show up at bottom of he l of the suggested
candidates:

=== Inputs ===

• CANDIDATES — a `listp' of `stringp's.

=== Output ===

a `listp' of `stringp's.

=== Laws ===

• Output preserves all elements in CANDIDATES.

=== Usage ===

e.g. register with `company-transformers':

    (make-local-variable 'company-transformers)
    (add-to-list 'company-transformers
                 #'sboo-company-transformer-elisp-bury-private-or-obsolete-symbols
                 :append)

=== Notes ===

• Naming — c.f. `bury-buffer'.

=== Links ===

URL `https://emacs.stackexchange.com/questions/12360/how-to-make-private-elisp-methods-the-last-company-mode-choices'."

  (let* ((TRANSFORMED-CANDIDATES

          (cl-loop for SYMBOL in candidates
               for PUBLICITY = (sboo-company-elisp-symbol-private-or-obsolete-p SYMBOL)

             if (eq nil PUBLICITY)       collect SYMBOL into PUBLIC
             else
             if (eq 'private PUBLICITY)  collect SYMBOL into PRIVATE
             else
             if (eq 'obsolete PUBLICITY) collect SYMBOL into OBSOLETE
             else
             collect SYMBOL into NON-PUBLIC

             finally return (append PUBLIC PRIVATE OBSOLETE NON-PUBLIC))))

    TRANSFORMED-CANDIDATES))

;; M-: (sboo-company-transformer-elisp-bury-private-or-obsolete-symbols '(yas-installed-snippets-dir sboo-company--private sboo-company-public sboo-company/private))
;; → '(sboo-company-public sboo-company--private sboo-company/private yas-installed-snippets-dir)
;;

;;----------------------------------------------;;

(defun sboo-company-elisp-symbol-private-or-obsolete-p (symbol)

  "Whether SYMBOL seems private or is obsolete.

Inputs:

• SYMBOL — a `symbolp' or `stringp'.

Output:

• a `booleanp', or Enum:
    • ‘private’
    • ‘obsolete’

Predicates:

• Private if — the `symbol-name'
  has a double-hyphen (i.e. ‹--›) or a forward-slash (i.e. ‹/›).
• Obsolete if — SYMBOL has been given to `make-obsolete'.

Links:

• URL `'
• URL `'"

  (cl-check-type symbol (or symbol string))

  (let ((SYMBOL (cond
                  ((symbolp symbol) symbol)
                  ((stringp symbol) (intern-soft symbol))
                  (t nil)))

        (STRING (cond
                  ((symbolp symbol) (symbol-name symbol))
                  ((stringp symbol) symbol)
                  (t nil)))
        )

    (cond ((or (string-match-p (regexp-quote "--") STRING)
               (string-match-p (regexp-quote "/") STRING))
           'private)

          ((or (get SYMBOL 'byte-obsolete-variable)
               (get SYMBOL 'byte-obsolete-info))
           'obsolete)

          (t nil))))

;; M-: (sboo-company-elisp-symbol-private-or-obsolete-p 'sboo-company-public)
;;  → nil
;;
;; M-: (sboo-company-elisp-symbol-private-or-obsolete-p 'sboo-company--private)
;;  → 'private
;;
;; M-: (sboo-company-elisp-symbol-private-or-obsolete-p 'sboo-company/private)
;;  → 'private
;;
;; M-: (sboo-company-elisp-symbol-private-or-obsolete-p 'yas-installed-snippets-dir)
;;  → 'obsolete
;;

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