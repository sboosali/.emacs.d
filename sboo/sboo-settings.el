;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General Settings
;;
;; Settings should be both **fast** and **safe**
;; (like the `:init' initialization in `use-package').
;; 
;; Most settings are literally just `setq' statements.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq undo-limit        20000000)
(setq undo-strong-limit 40000000)

;; ^ ensure undo limits are as high as possible.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cua-mode t)

;; ^ the standard keybindings: C-c, C-x, C-v, C-z.

(setq cua-keep-region-after-copy t) 

;; ^ Standard Windows behaviour.

(transient-mark-mode 1) 

;; ^ No region when nothing is highlighted.

(setq kill-whole-line t)

;; ^ « C-k » eats newline (a.k.a. "kills the whole line").
;;
;; Thus, we can type « C-k » where before we typed « C-k C-k ».

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq truncate-lines nil)

(setq require-final-newline      nil)
(setq mode-require-final-newline nil)

;; ^ Prevent automatic insertion of a final-newline.

(setq-default indent-tabs-mode nil)

;; ^ Prevent Extraneous Tabs

(show-paren-mode t)

;; ^ `show-paren-mode' automatically highlights matching parentheses.

(setq show-paren-delay 0)
(setq show-paren-style 'expression) 

;; ^ `show-paren-style' can be: `expression', `parenthesis', `mixed'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq scroll-step                     1)
(setq scroll-preserve-screen-position 1)
;; (setq scroll-margin 10)
;; (setq scroll-conservatively 10000)
(setq redisplay-dont-pause            t)

;; ^ "peeking" behavior when scrolling.

(when (>= emacs-major-version 26)
  (global-linum-mode +1))

;; ^ 

(column-number-mode +1)

;; ^ Always show column numbers.
;;
;; (By default, only row numbers are shown).

;;;TODO (add-hook ) 
;; ^ Disable `overwrite-mode'.

(global-font-lock-mode t) 
;; ^ Syntax highlighting, by default.

(auto-compression-mode t)
;; ^ Transparently open compressed files.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq visible-bell t)

;; ^
;; on user errors, flash a black square on to the screen.
;; instead of honking loudly through your speakers.

(setq inhibit-splash-screen     t)
(setq initial-scratch-message nil)

;; ^ fewer startup buffers

(setq initial-major-mode 'text-mode)

;; ^ Set the Default Major Mode for a new Buffer.

;(setq initial-buffer-choice 'xah-new-empty-buffer)
;; ^ Start Emacs with Empty Buffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'yes-or-no-p 'y-or-n-p)

;; ^ with `y-or-n-p', you press just one key for boolean prompts.
;;
;; i.e. the single character "y", instead of typing out the phrase "yes".
;;

(setq use-dialog-box nil)

;; ^ `nil' replaces Dialog Boxes with `yes-or-no' prompts.
;;
;; i.e. minibuffer prompts, which use the echo area and keyboard input.
;;
;; `use-dialog-box' also determines whether to use native file selection windows.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'safe-local-variable-values '(lexical-binding . t))

;; ^ `safe-local-variable-values':
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-background-color "#f4f4f4")

 ;; ^
 ;; i.e. R=xF4 G=xF4 B=xF4 
 ;; i.e. rgb(244, 244, 244)
 ;; i.e. faint gray (near-white)
 ;;
 ;; see:
 ;;     https://ux.stackexchange.com/questions/8153/what-are-the-negative-and-positive-aspects-of-dark-color-scheme

;;(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
;;(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq enable-recursive-minibuffers   t)
(setq minibuffer-depth-indicate-mode t)

;; ^ minibuffer settings.
;;
;; e.g. you can press "M-x" within a "M-x".
;; e.g. you can search through (via a second "C-s") the minibuffer of a search command for the (non-mini) buffer (having pressed the first "C-s").
;;
;; e.g. minibuffer displays "M-x [2]" when you've (often accidentally) double-{M-x}'d.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq Buffer-menu-name-width 30)
;; (setq Buffer-menu-size-width 6)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq echo-keystrokes 0.1)

;; (setq mouse-yank-at-point t)

;; (setq switch-to-buffer-preserve-window-point t)

;; (setq select-enable-clipboard t)
;; (setq select-enable-primary   t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable disabled commands:

(put 'upcase-region   'disabled nil)  ;; « C-x C-u »: same as M-u, but on whole regions.
(put 'downcase-region 'disabled nil)  ;; « C-x C-l »: same as M-l, but on whole regions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffap-bindings)
;; ^ a.k.a. `find-file-at-point'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DirEd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-dired-settings ()
  "My `dired' settings (auto-refresh)."

  (setq dired-auto-revert-buffer t)
  ())

(add-hook 'dired-mode-hook #'sboo-dired-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ProcEd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-proced-settings ()
  "My `proced' settings (auto-refresh)."

  (setq proced-auto-update-interval 3)  ; in Seconds.
  (proced-toggle-auto-update +1)
  ())

(add-hook 'proced-mode-hook #'sboo-proced-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Extensions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist (cons "\\.xpm\\'" #'c-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hacks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "LD_PRELOAD" "") ;;HACK;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Minibuffers
;;
;; common Minibuffers include:
;;
;; - `minibuffer-inactive-mode': the search prompt.
;; - 
;; -
;;
;;

;;; `setq-default'
;;
;; `setq-default' sets a values **only in** buffers that don't already have their own (i.e. `buffer-local') values for the variable.
;;
;; `setq-default' vs `setq': 
;;

;;; `undo'
;;
;; the "internal" undo behavior is:
;;
;; - To redo, just Press Ctrl+g first then undo. further undo will be redo. 
;; - Press Ctrl+g again to reverse direction. ("If you are careful, one can avoid the undo/redo roller-coaster confusion.")
;;
;; > all external undo-packages have corruption problems.

;;; e.g. Mode Discovery
;; 
;; [1] open a mini-buffer (e.g. `C-s` for the search mini-buffer, a.k.a `minibuffer-inactive-mode');
;; [2] then, focused on the minibuffer, run `describe-mode` (i.e. `C-h m`).
;;

;;; `dired':
;;
;; <>
;;
;; 

;;; `proced':
;;
;; <https://www.masteringemacs.org/article/displaying-interacting-processes-proced>
;; 
;; `proced` abbreviates "PROCess EDitor".
;; 
;; 

;;; Links
;;
;; - http://ergoemacs.org/emacs/emacs_best_redo_mode.html
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-settings)