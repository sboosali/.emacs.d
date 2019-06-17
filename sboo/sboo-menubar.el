;;; sboo-menubar.el --- Personal Menubar configuration -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 08 May 2019
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

;; Customize the appearence/behavior of the Menubar.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile
  (require 'pcase)
  (require 'cl-lib))

;;----------------------------------------------;;

(progn
  (require 'menu-bar))

;;----------------------------------------------;;
;; Constants -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-menubar-map nil

  "Personal Menu-Bar.

Rename / Remove / Reorder Menu Items
from the standard Menu Bar.")

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-menubar-setup ()

  "Setup the Menubar.

Effects:

• Modifies `menu-bar-search-menu'.

Links:

• URL `'"

  (let* ()

    (define-key menu-bar-search-menu [search-forward]
      '(menu-item "Find next String…" isearch-forward :help "« M-x isearch-forward »"))

    ()))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; ‘menu-bar.el’:
;;
;; • Provides the default menubar.
;; •
;;

;; `menu-bar-':
;;
;; M-: 
;;
;; M-: 
;;

;; `menu-bar-i-search-menu':
;;
;;     ;; The Edit->Search->Incremental Search menu
;;     (defvar menu-bar-i-search-menu
;;       (let ((menu (make-sparse-keymap "Incremental Search")))
;;         (bindings--define-key menu [isearch-backward-regexp]
;;           '(menu-item "Backward Regexp..." isearch-backward-regexp
;;             :help "Search backwards for a regular expression as you type it"))
;;         (bindings--define-key menu [isearch-forward-regexp]
;;           '(menu-item "Forward Regexp..." isearch-forward-regexp
;;             :help "Search forward for a regular expression as you type it"))
;;         (bindings--define-key menu [isearch-backward]
;;           '(menu-item "Backward String..." isearch-backward
;;             :help "Search backwards for a string as you type it"))
;;         (bindings--define-key menu [isearch-forward]
;;           '(menu-item "Forward String..." isearch-forward
;;             :help "Search forward for a string as you type it"))
;;         menu))

;; Links:
;;
;;   • URL `http://ergoemacs.org/emacs/modernization_menu.html'
;;   • URL `'
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-menubar)

;;; sboo-menubar.el ends here