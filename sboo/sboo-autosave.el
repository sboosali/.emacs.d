;;; sboo-autosave.el --- -*- lexical-binding: t -*-

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

;; Personal configuration for `autosave' and `backup'.
;; 
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

;; Backup Files & Auto-Save Files: Consolidate.

;; These files are useful if something goes wrong, but they're also annoying in how they clutter directories. Put them in ~/.emacs.d/tmp to remedy this.

;; TODO:
;;
;;(setq backup-directory-alist `((".*" . "~/.emacs.d/tmp")))
;;
;;(setq  auto-save-file-name-transforms `((".*" , "~/.emacs.d/tmp" t)))
;;
;;(xdg-cache-dir "emacs/backup")
;;
;;(xdg-cache-dir "emacs/autosave")

;;----------------------------------------------;;
;; Configuration -------------------------------;;
;;----------------------------------------------;;

(defun sboo-autosave-init! ()
  "Initialize `auto-save-visited-mode' variables."
  
  (interactive)

  (setq auto-save-visited-interval 1)
  ;; ^ autosave each second (by default, 5s).

  (setq auto-save-visited-file-name nil)
  ;; ^ disable a setting which itself disables our feature.

  ())

;;----------------------------------------------;;
;;; `:config'

(defun sboo-autosave-config! ()

  "Configure `auto-save-visited-mode'."
  (interactive)

  (auto-save-visited-mode 1)

  ;; ^ Autosave by overwriting the visited file.
  ;; 
  ;; `auto-save-visited-mode' is a GlobalMinorMode.
  ;;
  ;; Enable globally.

  ())

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; ^ `auto-save-visited-mode':
;;
;; NOTE `auto-save-visited-mode' was introduced in `emacs-major-version' 26.1
;;
;; Unlike `auto-save-visited-file-name', the new `auto-save-visited-mode' mode uses normal saving-procedure, and thus obeys saving-hooks.

;; See:
;;
;; - URL `https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Files.html'
;; - URL `https://www.reddit.com/r/emacs/comments/7h5til/uelizaretskii_emacs_26_is_nearing_its_release_the/'
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-autosave)

;;; sboo-autosave.el ends here