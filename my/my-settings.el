(provide 'my-settings)
(require 'my-functions)

(require 'recentf)


;; hide menu bar
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; peek
(setq
 redisplay-dont-pause t
 scroll-margin 10
 scroll-step 1
 scroll-conservatively 10000
 scroll-preserve-screen-position 1)

;; command as meta, not option
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; suppresses obnoxious sights and sounds
(setq visible-bell t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; Remove text in active region if inserting text
;;;  commented out because it's easy to lose your work
;; (pending-delete-mode t)

;; font-size
;(set-face-attribute 'default nil :height 50)
;(set-frame-parameter nil 'font "Monospace-2")
(set-default-font "-apple-Monaco-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")
(setq keyboard-coding-system nil)

;; Save a list of recent files visited.
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;  look and feel
(set-background-color "gray")
;;   (set-face-background 'default "black")
;;   (set-face-background 'region "black")
;;   (set-cursor-color "red")

;; Prevent Emacs from extending file when
;; pressing down arrow at end of buffer.
(setq next-line-add-newlines nil)

;; don't silently ensure newline at end of file
(setq require-final-newline nil)

;;; lines can be any length
;; http://stackoverflow.com/questions/11061453/in-emacs-how-to-disable-comment-auto-indent-in-c-c
(setq-default fill-column 1000)

; disable splitting long lines into short lines
(auto-fill-mode -1)

;;; Unicode
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; ?
;; (setq left-margin-width nil)
;; (setq right-margin-width nil)

; prevent stupid disabling pop-up
(put 'upcase-region 'disabled nil)

;; must only be set once (because it writes not appends?)
;; (custom-set-faces
;;   '(font-lock-comment-face ((t (:foreground "MediumAquamarine")))))

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Output-Variables.html
(setq print-length nil)

; delete seleted text when typing
; (delete-selection-mode 1)

; http://stackoverflow.com/questions/11380918/clean-way-to-perform-commands-in-the-emacs-minibuffer
; for commands' evalEmacs
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode t)

;;; make Emacs' path environment variable, i.e. (getenv "PATH"), consistent with your shell's
;;; from exec-path-from-shell
;; (when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize))

(when (equal system-type 'darwin)
  (set-exec-path-from-shell-path))

; http://emacswiki.org/emacs/EvaluatingExpressions
(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

;?
(put 'upcase-region 'disabled nil)


