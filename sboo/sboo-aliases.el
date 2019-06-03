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

(require 'cl-lib)

;;----------------------------------------------;;

;; sboo packages:

(require 'sboo-utilities)
(require 'sboo-unicode)
;(require 'sboo-functions)

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;; TODO 

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

(defalias '/cc #'compile)

;;----------------------------------------------;;

(defalias '/c  #'sboo-insert-char)
(defalias '/cu #'sboo-insert-unicode-name)

;;(defalias '/ic #'sboo-insert-char)
;;(defalias '/ic #'insert-char)

;;----------------------------------------------;;

(defalias '/rq #'query-replace-regexp)
(defalias '/rs #'replace-string)

(defalias '/ar  #'align-regexp)
(defalias '/ae  #'align-entire)

(defalias '/ir   #'indent-region)

;(defalias '/sl   #'sort-lines)
;(defalias '/rr   #'reverse-region)

(defalias '/md #'make-directory)

;;----------------------------------------------;;

(defalias '/jl #'goto-line)             ;[J]ump-to-[L]ine
(defalias '/jc #'goto-char)             ;[J]ump-to-[C]har

;; ^ use `goto-char' with « --debug-init » (from messages like « eval-buffer( ... ) ; Reading at buffer position 7897 »)

;;----------------------------------------------;;

(defalias '/e    #'sboo-eval-dwim)
(defalias '/eb   #'eval-buffer)
(defalias '/er   #'eval-region)
(defalias '/ed   #'eval-defun)          ; `eval-defun' resets `defvar's (unlike `eval-buffer' or `eval-region')
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

(defalias '/h1 #'sboo-comment-insert-h1)     ; Mnemonic: « Header 1 »
(defalias '/h2 #'sboo-comment-insert-h2)     ; Mnemonic: « Header 2 »
(defalias '/hc #'sboo-comment-insert-header) ; Mnemonic: « Header Comment »

;;----------------------------------------------;;

(defalias '/tex #'sboo-set-input-method-TeX)

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

(defalias '/w/ir #'indent-region)
(defalias '/w/sl #'sort-lines)
(defalias '/w/fr #'fill-region)
(defalias '/w/dtw #'delete-trailing-whitespace)
;(defalias '/w/   #')

;; ^ Naming:
;;
;; • "w" abbreviates "write"
;;   (i.e. run non-read-only actions on the text/region/buffer).
;; • « /w/ » namespaces these aliases
;;

;;----------------------------------------------;;
;; Project -------------------------------------;;
;;----------------------------------------------;;

(defalias '/mtg #'mtg-insert-card)

;;----------------------------------------------;;
;; External Packages ---------------------------;;
;;----------------------------------------------;;

(defalias '/pg #'projectile-grep)
(defalias '/pf #'projectile-find-file)
(defalias '/pc #'projectile-compile-project)

;;----------------------------------------------;;

(defalias '/fle #'flycheck-list-errors)

;;----------------------------------------------;;

(defalias '/g  #'magit)                 ;[G]it
(defalias '/gs #'magit-status)          ;[G]it [S]tatus

;;----------------------------------------------;;

(defalias '/yas #'yas-insert-snippet)

;;----------------------------------------------;;

(defalias '/hs #'sboo-dante-mode) ; mnemonic: "h" for "Haskell".
(defalias '/hr #'dante-restart)   ; mnemonic: "h" for "Haskell".

;;----------------------------------------------;;

;; `markdown-mode' aliases.
;; : (<PACKAGE>)-<OBJECT>-<ACTION>.

(progn

  (defalias '/mli #'markdown-insert-link) ;[M]arkdown [I]nsert [L]ink
  (defalias '/mci #'markdown-insert-code) ;[M]arkdown [I]nsert [C]ode

  'markdown-mode)

;;----------------------------------------------;;

;;(defalias '/fs #'ranger)  ; mnemonic: "fs" for "File System".

;; ^ directory sidebar with automatic viewing (click filename to open it in a buffer).

;;TODO ; mnemonic: "d" for "Directory".

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; See
;;    - http://ergoemacs.org/emacs/emacs_alias.html

;;----------------------------------------------;;
(provide 'sboo-aliases)