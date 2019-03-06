;;; -*- lexical-binding: t -*-

;;; Commentary:

;;----------------------------------------------;;
;; Aliases for Commands
;;
;; Motivation: shorten frequently-typed commands
;; (c.f. keybindings, i.e. frequently-pressed commands).
;;
;;----------------------------------------------;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtin packages:

(require 'cl)

;;----------------------------------------------;;

;; sboo packages:

(require 'sboo-utilities)
(require 'sboo-unicode)
;(require 'sboo-functions)

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;;; Settings -----------------------------------;;
;;----------------------------------------------;;

;; Single-Character Aliases...

;; NOTE! shadowing existing symbols can corrupt your configuration.
;;
;; in particular:
;; * `t' is a builtin variable (i.e. true).
;; * `s.el' and `f.el' are ubiquitous dependencies
;;   (which define « 's » and « 'f », respectively).
;;
;; Thus, I qualify aliases under an "alias-namespace",
;; by prefixing "/".
;;

;;----------------------------------------------;;

(defalias '/t #'dabbrev-expand)
;; ^ « t » for "tab-complete".

;;----------------------------------------------;;
;; Internal Packages ---------------------------;;
;;----------------------------------------------;;

(defalias '/te #'dabbrev-expand)
;; ^ « e » for "expand".

(defalias '/tc #'dabbrev-completion)
;; ^ « c » for "completion".

;;----------------------------------------------;;

(defalias '/ic #'insert-char)

;;(defalias '/ic #'sboo-insert-character-by-name)
;;(defalias '/ic #'insert-char)

(defalias '/rq #'query-replace-regexp)
(defalias '/rs #'replace-string)

(defalias '/ar  #'align-regexp)
(defalias '/ae  #'align-entire)

(defalias '/ir   #'indent-region)

;(defalias '/sl   #'sort-lines)
;(defalias '/rr   #'reverse-region)

(defalias '/md #'make-directory)

;;----------------------------------------------;;

(defalias '/eb   #'eval-buffer)
(defalias '/er   #'eval-region)
(defalias '/ed   #'eval-defun)
(defalias '/el   #'eval-last-sexp)

(defalias '/rb!  #'revert-buffer)

;;----------------------------------------------;;

(defalias '/dr   #'desktop-read)

(if (require 'sboo-desktop nil :noerror)
    (defalias '/ds! #'sboo-desktop-save)
  (defalias '/ds! #'desktop-save))

;;----------------------------------------------;;

(defalias '/bs #'bookmark-set)
(defalias '/bg #'bookmark-jump)        ; abbreviates "bookmark get"
(defalias '/bl #'bookmark-bmenu-list)  ; abbreviates "bookmark list"

;;----------------------------------------------;;

;; (defalias '/wm   #'whitespace-mode)
;; (defalias '/gwm  #'global-whitespace-mode)
;; (defalias '/glm  #'global-linum-mode)
;; (defalias '/vm   #'visual-line-mode)

;;----------------------------------------------;;

;; (defalias '/sbgc  #'set-background-color)

;;----------------------------------------------;;

;; (defalias '/sh   #'shell)

;;----------------------------------------------;;

;; (defalias '/fb   #'flyspell-buffer)

;;----------------------------------------------;;

;;----------------------------------------------;;
;; External Packages ---------------------------;;
;;----------------------------------------------;;

(defalias '/pg #'projectile-grep)
(defalias '/pf #'projectile-find-file)
(defalias '/pc #'projectile-compile-project)

;;----------------------------------------------;;

(defalias '/fle #'flycheck-list-errors)

;;----------------------------------------------;;

(defalias '/m  #'magit)
(defalias '/ms #'magit-status)

;;----------------------------------------------;;

(defalias '/hr #'dante-restart) ; mnemonic: "h" for "Haskell".

;;----------------------------------------------;;

(defalias '/fs #'ranger)  ; mnemonic: "fs" for "File System".

;; ^ directory sidebar with automatic viewing (click filename to open it in a buffer).

;;TODO ; mnemonic: "d" for "Directory".

;;----------------------------------------------;;

;(defalias '/ras #'real-auto-save-mode)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; See
;;    - http://ergoemacs.org/emacs/emacs_alias.html

;;----------------------------------------------;;
(provide 'sboo-aliases)
