;;; sboo-theme.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 24 Jun 2019
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

;; Personal utilities for ‘theme’s.
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

;;----------------------------------------------;;
;;; Variables ----------------------------------;;
;;----------------------------------------------;;

(defcustom sboo-theme-post-init-hook

  (list)

  "Hook for loaded custom themes.

Run (once) by `sboo-load-theme', which passes the name of the theme which was
loaded (successfully).

Type: `listp' of unary `functionp's (from `symbolp' to anything).

Usage: personalizing themes generically."

  :type 'hook

  :safe #'listp
  :group 'sboo-theme)

;;----------------------------------------------;;
;;; Functions ----------------------------------;;
;;----------------------------------------------;;

(defun sboo-load-theme (theme)

  "Load THEME and run `sboo-theme-post-init-hook'.

Completion: via `sboo-read-theme' .

Validation: via `custom-theme-name-valid-p'.

Related:

• Implementation ported from `custom-theme-visit-theme'"

  (interactive (list (sboo-read-theme :prompt "Custom theme to load: ")))

  ;; Validate the Theme Name:

  (unless (custom-theme-name-valid-p theme)
    (error "[‘sboo-load-theme’] Invalid theme name: `%S'" theme))

  ;; Load (& Enable) the Theme:

  (when (load-theme theme nil nil) ; t if loaded successfully.
    (run-hook-with-args 'sboo-theme-post-init-hook theme))

  theme)

;;----------------------------------------------;;

(cl-defun sboo-read-theme (&key prompt)

  "Read a theme name.

Inputs:

• PROMPT — an optional `stringp'.

Output: a `symbolp'.

Related:

• `custom-available-themes'"

  (let* ((PROMPT (or prompt "Custom theme: "))
         (STRING (completing-read PROMPT (mapcar #'symbol-name (custom-available-themes))))
         )
    (intern STRING)))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-theme)

;;; sboo-theme.el ends here