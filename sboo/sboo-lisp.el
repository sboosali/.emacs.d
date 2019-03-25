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

(require 'cl-lib)
;;(require 'pcase)
;;(require 'seq)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defcustom sboo-lisp-modes

  '(
    emacs-lisp-mode
    lisp-mode
    )

  "LISP Modes.

(Both major modes or minor modes)."

  :type '(repeat (symbol :tag "Mode"))

  :safe  t
  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-lisp-keywords

  '(
    "provide-theme"
    )

  "LISP Keywords to highlight."

  :type '(repeat (string :tag "Keyword"))

  :safe  t
  :group 'sboo)

;; (add-to-list emacs-lisp-keywords "provide-theme")
;; (font-lock-add-keywords 'emacs-lisp-mode '("\\<provide-theme\\>"))
;; (font-lock-add-keywords 'emacs-lisp-mode '())

;; ^ See `font-lock-keywords'

;; ^ See `https://stackoverflow.com/questions/13908192/apply-font-lock-to-quoted-symbols-in-elisp'

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 
;;----------------------------------------------;;
(provide 'sboo-lisp)