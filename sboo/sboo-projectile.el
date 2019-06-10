;;; sboo-projectile.el --- Personal `projectile-mode' configuration-*- coding: utf-8; lexical-binding: t -*-

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

;; Personal `projectile-mode' configuration
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


;;; Commentary:

;; Configuration for the `projectile' package.
;;
;; See:
;;
;; * `sboo-projectile-excluded-directories'.
;; * `sboo-projectile-excluded-file-names'
;; * `sboo-projectile-excluded-file-extensions'.
;;
;; 
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(require 'cl-lib)
(require 'pcase)

;;

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-projectile/excluded-global-directories      '(".sboo" "tmp" "ignore" ".git" ".dropbox-dist" ".nixnote"))
(defvar sboo-projectile/excluded-global-file-extensions  '("~" "#" "log"))
(defvar sboo-projectile/excluded-global-file-names       '("TAGS" "tags" ".bash_history_eternal"))

;;----------------------------------------------;;

(defvar sboo-projectile/excluded-haskell-directories     '("dist" "dist-newstyle" "dist-dante" ".stack-work" ".cabal-sandbox"))
(defvar sboo-projectile/excluded-haskell-file-extensions '("o" "hi" "chi" "chs.h"))
(defvar sboo-projectile/excluded-haskell-file-names      '())

;;----------------------------------------------;;

(defvar sboo-projectile/excluded-emacs-directories       '("db" "auto-save-list" "backups" "elpa" "eshell" "smex-items" "cask" ))
(defvar sboo-projectile/excluded-emacs-file-extensions   '("elc" "window-layout"))
(defvar sboo-projectile/excluded-emacs-file-names        '(".emacs.desktop" ".emacs.desktop.lock" "bookmarks" ".emacs.desktop..el" "bookmarks.el" "savehist.el" ".emacs-buffers" "places" "saved-places" "ido.last" "tramp" ".abbrev_defs" ".smex-items" ".yas-compiled-snippets.el"))

  ;;TODO `session.*` (prefix, not suffix)

;;----------------------------------------------;;

(defvar sboo-projectile/excluded-nix-directories       '("result"))
(defvar sboo-projectile/excluded-nix-file-extensions   '())
(defvar sboo-projectile/excluded-nix-file-names        '())

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defcustom sboo-projectile-excluded-directories

  (append
   sboo-projectile/excluded-global-directories      
   sboo-projectile/excluded-haskell-directories     
   sboo-projectile/excluded-emacs-directories       
   sboo-projectile/excluded-nix-directories)

  "Directories to exclude from `projectile' searches.

a `listp' of `stringp's."

  :type '(repeat (string :tag "Directory"))

  :safe #'listp
  :group 'sboo-projectile)

;;----------------------------------------------;;

(defcustom sboo-projectile-excluded-file-names

  (append
   sboo-projectile/excluded-global-file-names      
   sboo-projectile/excluded-haskell-file-names     
   sboo-projectile/excluded-emacs-file-names       
   sboo-projectile/excluded-nix-file-names)

  "File basenames to exclude from `projectile' searches.

a `listp' of `stringp's."

  :type '(repeat (string :tag "File Path"))

  :safe #'listp
  :group 'sboo-projectile)

;;----------------------------------------------;;

(defcustom sboo-projectile-excluded-file-extensions

  (append
   sboo-projectile/excluded-global-file-extensions      
   sboo-projectile/excluded-haskell-file-extensions     
   sboo-projectile/excluded-emacs-file-extensions       
   sboo-projectile/excluded-nix-file-extensions)

  "File extensions to exclude from `projectile' searches.

a `listp' of `stringp's."

  :type '(repeat (string :tag "File Extension"))

  :safe #'listp
  :group 'sboo-projectile)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; =======================
;; `projectile' Exclusions
;; =======================
;;
;; [1] `projectile-globally-ignored-files':
;;
;;     A list of files globally ignored by projectile.
;;
;; [2] `projectile-globally-ignored-directories':
;;
;;     A list of directories globally ignored by projectile.
;;
;; [3] `projectile-globally-ignored-file-suffixe':
;;
;;     A list of file suffixes globally ignored by projectile.
;;
;; [4] `projectile-globally-ignored-mode':
;;
;;     A list of regular expressions for major modes ignored by projectile.
;;     If a buffer is using a given major mode, projectile will ignore it for functions working with buffers.
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-projectile)

;;; sboo-projectile.el ends here