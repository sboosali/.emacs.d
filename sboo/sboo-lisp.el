;;; sboo-lisp.el --- -*- lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 03 May 2019
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

;; Personal configuration for `lisp-mode' submodes.
;;
;; • Add keywords for Emacs Lisp.
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile 
  (require 'cl-lib))

(progn
  (require 'pcase)
  (require 'seq))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-lisp nil

  "Customize `lisp-mode' and deriveés (e.g. `emacs-lisp-mode')."

  :prefix 'sboo
  :group 'sboo)

;;==============================================;;

(defcustom sboo-lisp-modes

  '(
    lisp-mode
    lisp-interaction-mode
    emacs-lisp-mode
    scheme-mode
    )

  "LISP Modes.

(Both major modes or minor modes)."

  :type '(repeat (symbol :tag "Mode"))

  :safe #'listp
  :group 'sboo-lisp)

;;----------------------------------------------;;

(defcustom sboo-lisp-hooks

  '(
    lisp-mode-hook
    lisp-interaction-mode-hook
    emacs-lisp-mode-hook
    scheme-mode-hook
    )

  "Hooks for `sboo-lisp-modes'."

  :type '(repeat (symbol :tag "Hook"))

  :safe #'listp
  :group 'sboo-lisp)

;;----------------------------------------------;;

(defcustom sboo-lisp-keywords

  '(
    "provide-theme"
    )

  "LISP Keywords to highlight."

  :type '(repeat (string :tag "Keyword"))

  :safe #'listp
  :group 'sboo-lisp)

;; (add-to-list emacs-lisp-keywords "provide-theme")
;; (font-lock-add-keywords 'emacs-lisp-mode '("\\<provide-theme\\>"))
;; (font-lock-add-keywords 'emacs-lisp-mode '())

;; ^ See `font-lock-keywords'

;; ^ See `https://stackoverflow.com/questions/13908192/apply-font-lock-to-quoted-symbols-in-elisp'

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
(provide 'sboo-lisp)