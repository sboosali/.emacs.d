
;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Personal configuratioin for `emacs-lisp-mode'.
;;
;; • Add keywords for Emacs Lisp.
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(require 'cl)
;;(require 'pcase)
;;(require 'seq)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Configuration -------------------------------;;
;;----------------------------------------------;;

(use-package lisp-mode

  :init

  ()

  :config

  (defvar sboo-lisp-keywords

    '( "provide-theme"
       )

    "Keywords to highlight in elisp/lisp.")

  (dolist (MODE '(emacs-lisp-mode lisp-mode))
    (font-lock-add-keywords MODE sboo-lisp-keywords))

;; (add-to-list emacs-lisp-keywords "provide-theme")
;; (font-lock-add-keywords 'emacs-lisp-mode '("\\<provide-theme\\>"))
;; (font-lock-add-keywords 'emacs-lisp-mode '())

  ())

;; ^ See `font-lock-keywords'

;; ^ See `https://stackoverflow.com/questions/13908192/apply-font-lock-to-quoted-symbols-in-elisp'

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 
;;----------------------------------------------;;
(provide 'sboo-elisp)