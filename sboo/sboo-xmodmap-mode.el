;;; sboo-xmodmap-mode.el --- Major mode for editing .xmodmaprc

;; Copyright (C) 1998, 2001 St�phane Levant <sun@tuxfamily.org>

;; Author: St�phane Levant <sun@tuxfamily.org>
;; Created: 1998
;; Keywords: languages
;; Version: 1.1
;; URL: http://arsunik.free.fr/emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Major mode for editing .xmodmaprc

;; INSTALL: Adds this lines to your .emacs :
;;  (autoload 'sboo-xmodmap-mode "sboo-xmodmap-mode" "" t)
;;  (setq auto-mode-alist
;;        (cons '("xmodmaprc" . sboo-xmodmap-mode) auto-mode-alist))
;;

;;; Code:

(defvar xmodmap-command "xmodmap"
  "The xmodmap command")

(defvar sboo-xmodmap-mode-hook nil)

(defvar sboo-xmodmap-mode-syntax-table nil)

(if sboo-xmodmap-mode-syntax-table
    ()
  (setq sboo-xmodmap-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?= "." sboo-xmodmap-mode-syntax-table)
  (modify-syntax-entry ?: "." sboo-xmodmap-mode-syntax-table)
  (modify-syntax-entry ?| "." sboo-xmodmap-mode-syntax-table)
  (modify-syntax-entry ?. "w" sboo-xmodmap-mode-syntax-table)
  (modify-syntax-entry ?! "<" sboo-xmodmap-mode-syntax-table)
  (modify-syntax-entry ?\n ">" sboo-xmodmap-mode-syntax-table))

(defvar xmodmap-map nil
  "Local keymap for xmodmap buffers.")

(if xmodmap-map
  nil
  (let ((map (make-keymap)))
    (define-key map "\^c\^c" 'xmodmap-read-current-file)
    (define-key map "\^c\^h" 'xmodmap-man)
    (define-key map "\^c\^s" 'xmodmap-shell-command)
    (easy-menu-define
      xmodmap-easy-menu map
      "Menu for Xmodmap mode."
      '("Xmodmap"
	 ["Read current file" xmodmap-read-current-file t]
	 ["Start xmodmap command" xmodmap-shell-command t]
	 ["-" nil t]
	 ["Xmodmap manual page" xmodmap-man t]))
    (setq xmodmap-map map)))

(defvar xmodmap-font-lock-keywords
  '(( "^!!!.*$" 0 font-lock-title-face t)
  ( "\\<\\(keycode\\|keysym\\|pointer\\|default\\|remove\\|add\
\\|any\\|clear\\)\\>" 1 font-lock-keyword-face)))

(cond ((not (facep 'font-lock-title-face))
       (defvar font-lock-title-face 'font-lock-title-face)
       (copy-face 'bold 'font-lock-title-face)))

(defun xmodmap-read-current-file (arg)
  "Lauch xmodmap on the current file"
  (interactive "P")
  (shell-command
    (concat xmodmap-command  " "
      (if arg
	(read-file-name "File: ")
	(buffer-file-name (current-buffer))))))

(defun xmodmap-man ()
  "Display the man page for xmodmap"
  (interactive)
  (man "1 xmodmap"))

(defun xmodmap-shell-command ()
  "Start a xmodmap command"
  (interactive)
  (shell-command (concat xmodmap-command  " " (read-string "" "xmodmap "))))

;;;###autoload
(defun sboo-xmodmap-mode ()
  "Major mode for editing ~/.xmodmaprc"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table sboo-xmodmap-mode-syntax-table)
  (make-local-variable 'comment-start)
  (make-local-variable 'font-lock-defaults)
  (use-local-map xmodmap-map)
  (setq comment-start "!"
	major-mode 'sboo-xmodmap-mode
	mode-name "XModMap"
	font-lock-defaults '(xmodmap-font-lock-keywords nil nil nil))
  (run-hooks 'sboo-xmodmap-mode-hook))

;;; sboo-xmodmap-mode.el ends here
