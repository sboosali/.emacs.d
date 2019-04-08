;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; Fonts.
;; 
;; • Iosevka
;;
;; 

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(require 'cl-lib)
;;(require 'pcase)
;;(require 'seq)

;; Project:

(require 'sboo-utilities)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-default-fixed-width-font-name

  "Iosevka"

  "My default (fixed-width) font for: programs, configs, etc.

`Iosevka' is an open-source font, designed for code. 

Links:

• URL `https://be5invis.github.io/Iosevka'.")

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-set-default-font ()
  "`sboo-set-font' to the `sboo-default-fixed-width-font-name' font family."
  (interactive)
  (sboo-set-font sboo-default-fixed-width-font-name))

;;----------------------------------------------;;

(defun sboo-set-font-to-iosevka ()
  "`sboo-set-font' to the Iosevka font family."
  (interactive)
  (sboo-set-font "Iosevka"))

;;----------------------------------------------;;

(defun sboo-set-font (font)

  "Sets the font of the `current-buffer' to `FONT'.

Inputs:

• FONT is a string.

Examples:

• M-: (sboo-set-font \"Iosevka\")
• M-x sboo-set-font RET Iosevka

Related:

• `font-family-list'
• `find-font'
• `buffer-face-set'
"

  (interactive (list
                (completing-read "Font name: " (font-family-list) nil t)))

  ;; ^ `"s"` means "read a string from the user until they press RET".

  (if (find-font (font-spec :name font))

      ;; ^ 
      ;; e.g. (find-font (font-spec :name "iosevka"))
      ;; e.g. (find-font (font-spec :name "garamond"))
    
    (progn
      (buffer-face-set `(:family ,font))
      (buffer-face-mode)
      t)
    
    nil))

;; ^ NOTE to list all (available) fonts:
;; 
;;     M-: (print (font-family-list))
;;
;; > Starting with Emacs 23, you can set the face for the current buffer, using ‘M-x buffer-face-set’. 
;; > You can toggle this on/off using ‘M-x buffer-face-mode’. 
;; > Internally, this uses ‘face-map-add-relative’ to remap faces. 
;; > For example, (face-remap-add-relative 'default :family "Source Code Pro" :height 140)
;;
;; 

;; ^ `completing-read':
;;
;; (completing-read PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH
;; INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)
;;

;;----------------------------------------------;;

(defun sboo-register-default-font-for-hook (hook)

  "Register ‘sboo-default-fixed-width-font-name’ with HOOK.

Inputs:

• HOOK — a `symbolp'.
         a hook variable. most are named « *-mode-hook »."

  (interactive (list (sboo-read-hook)))

  (add-hook hook #'sboo-set-default-font))

;;----------------------------------------------;;

(defun sboo-fonts-config! ()

  "Configuration of Fonts.

Use the ‘Iosevka’ font for all buffers with code.

(i.e. whose `major-mode' inherits from `prog-mode';
e.g. `lisp-mode', `haskell-mode', `nix-mode', etc).
"
  (interactive)

  (dolist (HOOK '(prog-mode-hook
                  haskell-cabal-mode-hook
                  conf-mode
                  yaml-mode
                  ))

    (add-hook HOOK #'sboo-set-default-font)))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; Noto (Noto Sans Symbols)
;;
;; TODO use Noto, a typeface with one of the largest coverages of Unicode, for the minibuffer font.
;;
;; 
;;

; [Font Test] char and monospace:
;; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
;; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?

;; Links:
;;
;; • URL `https://github.com/rolandwalker/unicode-fonts'
;; • URL `http://ergoemacs.org/emacs/emacs_minibuffer_font_size.html'
;; • URL `https://stackoverflow.com/questions/7869429/altering-the-font-size-for-the-emacs-minibuffer-separately-from-default-emacs'
;;
;; • URL `https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-font-check.el'
;; • URL `https://emacs.stackexchange.com/questions/3038/using-a-different-font-for-each-major-mode'
;; • URL `https://www.emacswiki.org/emacs/FacesPerBuffer'
;;

;;==============================================;;
(provide 'sboo-fonts)