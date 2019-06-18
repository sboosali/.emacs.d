;;; sboo-shell.el --- My Shell/Term config -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 17 Jun 2019
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

;; Personal configuration for Terminals / Shell.
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile
  (require 'rx))

;;----------------------------------------------;;

(progn
  (require 'dirtrack)
  (require 'cl-lib))

;;----------------------------------------------;;
;; Customize -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-shell nil

  "Personal Shell Mode customization."

  :prefix 'shell

  :group 'shell
  :group 'sboo)

;;----------------------------------------------;;

(defgroup sboo-term nil

  "Personal Term Mode customization."

  :prefix 'term

  :group 'term
  :group 'sboo)

;;==============================================;;

(defcustom sboo-shell-hooks-list

  (list #'superword-mode
        )

  "Hooks for `shell-mode'.

a `listp' of `functionp's."

  :type '(repeat (function))

  :safe #'listp
  :group 'sboo-shell)

;;----------------------------------------------;;

(defcustom sboo-term-hooks-list

  (list ;; #'sboo-local-unset-tab
        #'superword-mode
        )

  "Hooks for `term-mode'.

a `listp' of `functionp's."

  :type '(repeat (function))

  :safe #'listp
  :group 'sboo-term)

;;----------------------------------------------;;

(defvar sboo-path-regexp

  (rx (any "/.~")
      (zero-or-more (not (any "$"))))

   "Regular expression (fragment) matching a (POSIX) filepath.")

;;----------------------------------------------;;

(defvar sboo-prompt-regexp

  (rx bol
      (zero-or-more space)
      (group (any "/.~")                   ;; the present working directory
             (zero-or-more (not (any "$"))))
      "$"
      (zero-or-more space))

      "Regular expression (for a whole line) to extract the working directory from a command prompt.

MUST be able to match the user's `PS1'.

For `dirtrack-list'.

e.g. Prompt \"~/.emacs.d$ \" holds directory \"~/.emacs.d\".")

;; ^ e.g. matches this command prompt

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-process-disable-query-on-exit () 

  "Disable the `query-on-exit' flag.

=== Usage ===

e.g.:

    (dolist (HOOK '(comint-exec-hook term-exec-hook)
      (add-hook HOOK #'sboo-process-disable-query-on-exit))

    ;; ^ Don't ask when quitting shells & terminals.

=== Links ===

URL `https://stackoverflow.com/questions/2706527/make-emacs-stop-asking-active-processes-exist-kill-them-and-exit-anyway'"

  (when-let* ((CURRENT-PROCESS (get-buffer-process (current-buffer))))
    (when (processp CURRENT-PROCESS)
      (set-process-query-on-exit-flag CURRENT-PROCESS nil))))

;;----------------------------------------------;;

(defun sboo-local-unset-tab ()
 
  "`local-unset-key' for `<tab>'."

  (local-unset-key (kbd "<tab>")))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; `comint-mode':
;;
;; • `comint' abbreviates "COMmand INTerpreter".
;;

;; `dirtrack-mode':
;;
;; • 
;;

;; ^ Links:
;;
;;   • URL `https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell-Options.html'
;;   • URL `https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion.html'
;;   • URL `https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Tracking.html'
;;   • URL `https://emacs.stackexchange.com/questions/5589/automatically-update-default-directory-when-pwd-changes-in-shell-mode-and-term-m'
;;   • URL `https://snarfed.org/why_i_run_shells_inside_emacs'
;;   • URL `https://francismurillo.github.io/2017-03-30-Exploring-Emacs-rx-Macro/ '
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-shell)

;;; sboo-shell.el ends here