;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NOTE
;;
;; I want these few settings to be always present,
;; even if the rest of this file fails, 
;; for easier debugging.
;;
;; Thus, this section must not have any errors itself,
;; nor do anything too complicated.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cua-mode t)
;; ^ the standard keybindings: C-c, C-x, C-v, C-z.

(setq cua-keep-region-after-copy t) 
;; ^ Standard Windows behaviour.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(transient-mark-mode 1) 
;; ^ No region when nothing is highlighted.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
;; ^ TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 ;; "peeking" behavior when scrolling.
 redisplay-dont-pause t
 ;; ^
 ;; scroll-margin 10
 ;; ^
 scroll-step 1
 ;; ^
 ;; scroll-conservatively 10000
 ;; ^
 scroll-preserve-screen-position 1)
 ;; ^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'yes-or-no-p
  'y-or-n-p)
  ;; ^
  ;; so you can just press one key for prompts
  ;; (i.e. the single character "y",
  ;; instead of typing out the phrase "yes").

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 require-final-newline      nil
 ;; ^ 
 mode-require-final-newline nil)
 ;; ^ 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-background-color "#f4f4f4")
 ;; ^
 ;; i.e. R=xF4 G=xF4 B=xF4 
 ;; i.e. rgb(244, 244, 244)
 ;; i.e. faint gray (near-white)
 ;;
 ;; see:
 ;;     https://ux.stackexchange.com/questions/8153/what-are-the-negative-and-positive-aspects-of-dark-color-scheme
 
(setq
 visible-bell              t)
 ;; ^ on user errors,
 ;; flash a black square on to the screen
 ;; instead of honking loudly through your speakers.

(setq
 ;; fewer startup buffers
 inhibit-splash-screen     t
 ;; ^ 
 initial-scratch-message nil)
 ;; ^ 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
 ;; minibuffer settings.
 (setq
  enable-recursive-minibuffers t)
  ;; ^
  ;; e.g. you can press "M-x" within a "M-x".
  ;; e.g. you can search through (via a second "C-s") the minibuffer of a search command for the (non-mini) buffer (having pressed the first "C-s").
 (minibuffer-depth-indicate-mode t))
 ;; ^
 ;; e.g. displays "M-x [2]" when you've
 ;; (often accidentally) double-{M-x}'d.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDENTATION

(setq-default
 indent-tabs-mode nil)
 ;; ^ Prevent Extraneous Tabs
 ;; '''Note that this line uses setq-default rather than the setq command that we have seen before;
 ;; The setq-default command sets values only 
 ;; in buffers that do not have their own local values for the variable.'''
 
;; (when (fboundp 'electric-indent-mode)
;;  (electric-indent-mode -1))
;;  ;; ^ disable automatic indentation on newlines(/ pressing return).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO
(column-number-mode 1)
 ;; ^ show column numbers.
 ;; only row numbers are shown by default.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parentheses

(show-paren-mode t)	
;; ^ highlight matching parens, automatically.

(setq 
 show-paren-delay 0
 ;; ^ 
 show-paren-style 'expression) 
 ;; ^ alternatives are: `parenthesis' and `mixed'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-font-lock-mode t) 
;; ^ Syntax highlighting, by default.

(auto-compression-mode t)
;; ^ (Transparently) open compressed files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFERS 

(setq
 ediff-window-setup-function 'ediff-setup-windows-plain)

(setq
 Buffer-menu-name-width 30)
;; (setq Buffer-menu-size-width 6)

(add-hook 'Buffer-menu-mode-hook (lambda() 
  (setq Buffer-menu-files-only t)
  ;; ^ i.e. file-buffers, not all buffers.
  (revert-buffer)
  ;; ^ ?
))
;; ^ see buff-menu.el 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'safe-local-variable-values
  '(lexical-binding . t))

  ;; ^
  ;; `safe-local-variable-values':
  ;; 

;;? (add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
  use-dialog-box t)

;; ^
;;
;; To disable the use of dialog boxes, change the variable use-dialog-box to nil. In that case, Emacs always performs yes-or-no prompts using the echo area and keyboard input.
;; This variable also controls whether to use file selection windows (but those are not supported on all platforms).
;;
;; A file selection window is a special kind of dialog box for asking for file names.
;; You can customize the variable use-file-dialog to suppress the use of file selection windows, even if you still want other kinds of dialogs. This variable has no effect if you have suppressed all dialog boxes with the variable use-dialog-box.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffap-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 
;; 
;; 

;; See:
;;     - 
;;     - 
;;     - https://www.gnu.org/software/emacs/manual/html_node/emacs/Dialog-Boxes.html
;;     - 
;;     - 

;;TODO
;; (set-face-attribute 'default nil :font "Garamond" :height 200)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-settings-safe)