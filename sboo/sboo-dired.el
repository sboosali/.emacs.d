;;; sboo-dired.el --- Pesronal ‘dired’ configurations. -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 15 Jun 2019
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

;; Pesronal ‘dired’ & ‘find-dired’ configurations.
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
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-dired nil

  "Customize ‘dired’ & ‘find-dired’."

  :prefix 'sboo
  :group 'dired
  :group 'sboo)


;;==============================================;;

(defcustom sboo-dired-find-switces-elisp

  "-type f \\( -name '*.el' -or -name '*.el.gz' \\)"

  "Command-Line Options to program ‘find’ for Elisp files.

a `stringp'."

  :type '(string :tag "Find Switches")

  :safe #'stringp
  :group 'sboo-dired)

;;----------------------------------------------;;

(defcustom sboo-dired-find-switces-nix

  "-type f -name '*.nix'"

  "Command-Line Options to program ‘find’ for Nix files.

a `stringp'."

  :type '(string :tag "Find Switches")

  :safe #'stringp
  :group 'sboo-dired)

;;----------------------------------------------;;

(defcustom sboo-dired-find-switces-haskell

  "-type f \\( -name '*.hs' -or -name '*.lhs' -or -name '*.chs' \\)"

  "Command-Line Options to program ‘find’ for Haskell files.

a `stringp'."

  :type '(string :tag "Find Switches")

  :safe #'stringp
  :group 'sboo-dired)

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-dired (&key directory type)

  "Launch `dired' recursively.

Inputs:

• DIRECTORY — a `stringp'.
  Filepath.
  Defaults to `default-directory'.

• TYPE — a `symbolp'.
  Defaults to TODO.

Effects:

• Launches a `dired-mode' buffer.

Related:

• Wraps `find-dired'.

Links:

• URL `https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired-and-Find.html'"

  (interactive)

  (let* ((DIRECTORY      (or directory default-directory))
         (FIND-ARGUMENTS sboo-dired-find-switces-elisp)
         )

    (find-dired DIRECTORY FIND-ARGUMENTS)))






(when (>= emacs-major-version 26)
  
  (require 'wdired)

  (setq wdired-create-parent-directories t)


  ;; ^ create new subdirectories, when adding slashes or editing (sub)directory names.
  ;;
  ;; i.e. when editing filenames with slash characters,
  ;; and when adding slash characters to filenames,
  ;; automatically create the induced directories.
  ;;

  ())

;; ^
;;
;; NOTE 'W' gets bound to `browse-url-of-dired-file' (e.g. for viewing HTML files).
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-dired-descendants ()
  ""
  (interactive)
  ())  ;; TODO find-name-dired?

  (let ((include-directories t)
         (filepath-blacklist-regexp ""))
    (directory-files-recursively default-directory filepath-blacklist-regexp include-directories))



;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; Invoke Dired:
;;
;; * M-x dired
;; * C-x d
;;
;; 

;; Dired Keymap
;;
;; ‘i’: Insert Subdirectory (in Dired buffer). i.e. expand its contents, or descend into it.
;; ‘^’: Ascend into the Parent Directory.
;;

;; M-x `find-name-dired'
;;
;; reads arguments DIRECTORY and PATTERN,
;; finds all files in DIRECTORY or (its subdirectories) whose individual names match PATTERN,
;; then displays them in a single Dired buffer.
;;

;; M-x `find-grep-dired'
;;
;; 

;; See:
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired-Enter.html
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Subdirectories-in-Dired.html#Subdirectories-in-Dired
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired-and-Find.html
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Contents-of-Directories.html
;; - https://emacs.stackexchange.com/questions/33332/recursively-list-all-files-and-sub-directories
;; - http://man7.org/linux/man-pages/man1/ls.1.html
;; 

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-dired)

;;; sboo-dired.el ends here