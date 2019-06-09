;;; sboo-compilation.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 04 Jun 2019
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

;; Personal `compilation-mode' configuration.
;;
;; See:
;;
;; * `'.
;;
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(progn
  (require 'compile)
  (require 'seq)
  (require 'cl-lib))

;;----------------------------------------------;;

(eval-when-compile
  (require 'rx)
  (require 'pcase))

;;----------------------------------------------;;
;; Variables: `compilation-mode' ---------------;;
;;----------------------------------------------;;

(defgroup sboo-compilation

  nil

  "Personal `compilation-mode'."

  :group 'compilation)

;;==============================================;;

(defcustom sboo-compilation-issue-string-alist

  `((success   . "✔")
    (errors    . "⛔")                  ; the "Stop" Sign.
    (warnings  . "⚠")                  ; the "Warning" Sign.
   )

  "Aliases for `compile' issues.

Associates `symbolp's with `stringp's."

  :type '(alist :key-type   (symbol :tag "Issue")
                :value-type (choice (const nil)
                                    (string :tag "Alias")))

  :safe #'listp
  :group 'sboo-compilation)

;;----------------------------------------------;;

(defcustom sboo-compile-colorize-buffer-blacklist

  '(grep-mode ag-mode rg-mode)

  "Exceptions (`major-mode's) for `sboo-compile-colorize-buffer'.

`listp' of `symbolp's."

  :type '(repeat (symbol :tag "Major Mode"))

  :safe #'listp
  :group 'sboo-compilation)

;;----------------------------------------------;;
;; Variables: `flycheck-mode' ------------------;;
;;----------------------------------------------;;

(defgroup sboo-flycheck

  nil

  "Personal `flycheck-mode'."

  :group 'flycheck)

;;==============================================;;

(defcustom sboo-flycheck-status-char-alist

  `(
    (running     . ?⟲)
    (no-checker  . ?⚠)
    (not-checked . ?✖)
    (errored     . ?⚠)
    (interrupted . ?⛔)
   )

  "Aliases for `flycheck' statuses.

Associates `symbolp's with `characterp's."

  :type '(alist :key-type   (symbol :tag "Status")
                :value-type (choice (const nil)
                                    (character :tag "Alias")))

  :safe #'listp
  :group 'sboo-flycheck)

;;----------------------------------------------;;

;; ('finished (if flycheck-current-errors
;;                (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
;;                               (+ (or .warning 0) (or .error 0)))))
;;                  (format "✖ %s Issue%s" count (if (eq 1 count) "" "s")))
;;              "✔ No Issues"))
;; ('running     "⟲ Running")
;; ('no-checker  "⚠ No Checker")
;; ('not-checked "✖ Disabled")
;; ('errored     "⚠ Error")
;; ('interrupted "⛔ Interrupted")

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-compile-colorize-buffer ()

  "Colorize ANSI Color Codes in non-`grep-mode' `compilation-buffer's.

Implementation:

• Invokes `ansi-color-apply-on-region'.

Links:

• URL `http://stackoverflow.com/a/13408008/1219634'.
• URL `https://github.com/kaushalmodi/.emacs.d/blob/08f8256f3de346bf6d389f922c52b4605f700fc4/setup-files/setup-compile.el'."

  (unless (seq-some #'derived-mode-p sboo-compile-colorize-buffer-blacklist)

    (ansi-color-apply-on-region compilation-filter-start (point))))

;;----------------------------------------------;;

;; TODO unicode-messages:
;;
;; (spaceline-define-segment
;;     ati-flycheck-status "An `all-the-icons' representaiton of `flycheck-status'"
;;     (let* ((text
;;             (pcase flycheck-last-status-change
;;               (`finished (if flycheck-current-errors
;;                              (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
;;                                             (+ (or .warning 0) (or .error 0)))))
;;                                (format "✖ %s Issue%s" count (if (eq 1 count) "" "s")))
;;                            "✔ No Issues"))
;;               (`running     "⟲ Running")
;;               (`no-checker  "⚠ No Checker")
;;               (`not-checked "✖ Disabled")
;;               (`errored     "⚠ Error")
;;               (`interrupted "⛔ Interrupted")
;;               (`suspicious  "")))
;;            (f (cond
;;                ((string-match "⚠" text) `(:height 0.9 :foreground ,(face-attribute 'spaceline-flycheck-warning :foreground)))
;;                ((string-match "✖ [0-9]" text) `(:height 0.9 :foreground ,(face-attribute 'spaceline-flycheck-error :foreground)))
;;                ((string-match "✖ Disabled" text) `(:height 0.9 :foreground ,(face-attribute 'font-lock-comment-face :foreground)))
;;                (t '(:height 0.9 :inherit)))))
;;       (propertize (format "%s" text)
;;                   'face f
;;                   'help-echo "Show Flycheck Errors"
;;                   'display '(raise 0.2)
;;                   'mouse-face '(:box 1)
;;                   'local-map (make-mode-line-mouse-map 'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))
;;     :when active :tight t )

;;----------------------------------------------;;
;;; Configuration ------------------------------;;
;;----------------------------------------------;;

(defun sboo-compilation-init! ()

  "Initialize `compilation-mode' variables."

  (interactive)

  ;;TODO ;; ^ continuously recompile, on each save.

  (setq compilation-ask-about-save nil)

  ;; ^ Save buffer(s) (without asking).

  (setq compilation-always-kill t)

  ;; ^ `t' means TODO.

  (setq compilation-scroll-output t)

  ;; ^ 

  (setq next-error-highlight         t)
  (setq next-error-follow-minor-mode t)

  ;; ^ start at the first error (link).

  ())

;;----------------------------------------------;;

(defun sboo-compilation-config! ()

  "Configure `compilation-mode'."

  (interactive)

  (when (require 'ansi-color nil :noerror)
    (add-hook 'compilation-filter-hook #'sboo-compile-colorize-buffer))

  ;; ^ Don't mess up colors in Grep/Ag results buffers"

  ;; ^ Compilation.
  ;; 
  ;; `compilation-mode' is a MajorMode.

  ())

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; NOTE The `compilation-mode' configuration also affects `projectile-compile-project'.

;;----------------------------------------------;;

;; 

;;----------------------------------------------;;

;; e.g. add NodeJS error format:
;; 
;; See
;; - https://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/
;;
;; (setq compilation-error-regexp-alist-alist
;;       (cons '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
;;                          1 ;; file
;;                          2 ;; line
;;                          3 ;; column
;;                          )
;;             compilation-error-regexp-alist-alist))
;;
;;
;; ;; ^ [1] define a regex for filename / line number /column number, of errors/warnings.
;;
;; (setq compilation-error-regexp-alist
;;       (cons 'node compilation-error-regexp-alist))
;;
;; ;; ^ [2] register that (named) regex.
;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-compilation)

;;; sboo-compilation.el ends here