;;;;;;;;;;;;;;; Dependencies ;;;;;;;;;;;;;;;;;;;;;;
; ~/.templates/*.tpl

;;;;;;;;;;;;;;; INIT ;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)
;(require 'cl-lib)

(setq HOME (expand-file-name "~/.emacs.d/"))


;;;;;;;;;;;;;;; Paths ;;;;;;;;;;;;;;;;;;;;;;

(defvar load-paths '(
 "."
 "my"                                   ; utilities or wrappers around libraries
 "apps"                                 ; initialization depending on <X>.app

 "packages"                             ; package files that were copied and pasted

 "back-button"                          ; etc.

 "structured-haskell-mode/elisp"        ; Haskell
 "ghc-server/elisp"
 "hindent/elisp"

 "emacs_chrome/servers"                 ; Emacs in chromium
 "edit-server-htmlize"

) "load paths that don't obey the normal package-name/module-name.el format.")

(loop for location in load-paths
      do (add-to-list 'load-path (concat HOME location)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Packaging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(
 ("melpa" . "http://melpa.milkbox.net/packages/")
 ("gnu" . "http://elpa.gnu.org/packages/")
))

(setq package-enable-at-startup nil)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

; run this every week
;(package-refresh-contents)

(defvar packages '(
 cl-lib
 starter-kit
 smex
 undo-tree
 magit
 solarized-theme
 smart-tabs-mode
 ido-complete-space-or-hyphen
; elscreen
; async
; helm
; edit-server
))

(dolist (p packages)
  (unless (package-installed-p p)
    (package-install p)))

;; (require 'shm)
;; (require 'hindent)
;; (require 'ghc)
;; (require 'shm-case-split)

;; (smex-initialize)
;; (turn-on-haskell-simple-indent)
;; (load "haskell-mode-autoloads.el")

 ;; (when (require 'dired-aux)
 ;;   (require 'dired-async))

;; (when (require 'helm-config)
;;  (helm-mode 1))

;;;;;;;;;;;;;;; SETTINGS ;;;;;;;;;;;;;;;;;;;;;;
(require 'my-settings)



;;;;;;;;;;;;;;; utilities
(require 'etc)


;;;;;;;;;;;;;;; Persist ;;;;;;;;;;;;;;;;;;;;;;
; reopens the open buffers when Emacs last closed
; make it work for multiple applications
;(desktop-save-mode 1)


;;;;;;;;;;;;;;; IDO ;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)

(require 'ido-complete-space-or-hyphen)
(ido-mode t)


(setq ido-ignore-buffers '(
 "^ "
 "*Completions*"
 "*Shell Command Output*"
 "*Messages*"
 "Async Shell Command"
 "*scratch*"
 "*Messages*"
 "*Quail Completions*"
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Command Examples

; replace-regexp
; Parsable\(\w+\)
; \1Grammar
;
; ParsableType -> GrammarType



;;;;;;;;;;;;;;;;;;;;;;;;;;;; ParEdit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'enable-paredit-mode "paredit" "" t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; SavePlace ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'saveplace)

(setq-default save-place t)
;can't use setq because the variable is buffer-local.

(setq save-place-file "~/.emacs.d/saved-places")
;your saved places are written to this file


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Abbreviations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tell emacs where to read abbrev definitions from
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")

;; save abbrevs when files are saved
;; you will be asked before the abbreviations are saved
(setq save-abbrevs t)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; keep it always on
;(setq default-abbrev-mode t)

;; turn it on sometimes
;(dolist (hook '(text-mode-hook python-mode-hook))
;  (add-hook hook (lambda () (abbrev-mode 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Deft ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'deft)

(setq deft-extension "note")
(setq deft-directory "~/Dropbox")

(setq deft-use-filename-as-title t)
;(setq deft-text-mode 'markdown-mode)
(setq deft-separator "")

(global-set-key [f9] 'deft)

; dynamic scope...
(defun deft-parse-summary (contents title) "")


;;;;;;;;;;;;;;;;;;;;;;;; Smex ;;;;;;;;;;;;;;;;;;;;;;;;
; "Smex is a M-x enhancement for Emacs. Built on top of IDO, it provides a convenient interface to your recently and most frequently used commands."

;
(global-set-key [(meta x)] (lambda ()
 (interactive)
 (or (boundp 'smex-cache)
     (smex-initialize))
 (global-set-key [(meta x)] 'smex)
 (smex)))

;
(global-set-key [(shift meta x)] (lambda ()
 (interactive)
 (or (boundp 'smex-cache)
    (smex-initialize))
 (global-set-key [(shift meta x)] 'smex-major-mode-commands)
 (smex-major-mode-commands)))


;;;;;;;;;;;;;;;;;;;;;;;; PYTHON ;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smart-tabs-mode)

(defun my-python-mode-hook ()
  (smart-tabs-advice python-indent-line-1 python-indent)

  (setq tab-width 4)
  (setq py-indent-offset 4)

  (setq py-indent-offset tab-width)
  (setq python-indent tab-width)
  (setq py-smart-indentation nil)

  (setq-default show-trailing-whitespace t)
  ;(add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq py-indent-comments nil)
  (setq py-electric-comment-p nil)
)

(add-hook 'python-mode-hook 'my-python-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;; Templates ;;;;;;;;;;;;;;;;;;;;;;;;
;http://emacs-template.sourceforge.net/
(require 'template)
(template-initialize)

;$ mkdir ~/.templates/
;$ touch ~/.templates/TEMPLATE.hs.tpl
;$ ls ~/.templates/*.tpl

;M-x template-new-file
;M-x template-expand-template

;some standard embedded tags (note the parentheses):
;; (>>>POINT<<<)
;; (>>>FILE_SANS<<<)
;; (>>>DATE<<<)
;; (>>>USER_NAME<<<)

; e.g. defines the tag (>>>GUIID<<<)
;(add-to-list 'template-expansion-alist
; '("GUIID" (insert (substring (shell-command-to-string "uuidgen") 0 -1))))

;?
;(add-to-list 'template-find-file-commands 'ido-exit-minibuffer)


;;;;;;;;;;;;;;;;;;;;;;;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;

(defun note-hook ()
 (when (is-note-file)
  (note-mode)))

(add-hook 'find-file-hook 'note-hook)

(defun note-mode ()
 (set-input-method "TeX") ; inline latex. inserts unicode.
 (goto-address-mode)
 (setq comment-start ".")
 (setq comment-end "")
 (setq coding-system 'utf-8)
)

(defun is-note-file ()
 (or
  (not (string-match "\\." buffer-file-name)) 
  (string-match "\\.note$" buffer-file-name)))


;;;;;;;;;;;;;;; ??? ;;;;;;;;;;;;;;;;;;;;;;
; I forget why
(defun set-exec-path-from-shell-path ()
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$"
          ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-path))
(set-exec-path-from-shell-path)


;; ;;;;;;;;;;;;;;; Haskell ;;;;;;;;;;;;;;;;;;;;;;
(require 'my-haskell)

;; ;;;;;;;;;;;;;;; Prolog ;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Remote Access ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")


;;;;;;;;;;;;;;; Abbreviations ;;;;;;;;;;;;;;;;;;;;;;

(defun after-abbrev-expand-hook ()
  (when (looking-back "\"\"\\|''\\|()\\|\\[\\]\\|{}")
    (backward-char 1))
  t)
(put 'after-abbrev-expand-hook 'no-self-insert t)


;;;;;;;;;;;;;;; Terminal ;;;;;;;;;;;;;;;;;;;;;;
(require 'multi-term)

(setq multi-term-program "/bin/bash")

;; default (print-list term-bind-key-alist)
;; ("C-c C-c" . term-interrupt-subjob)
;; ("C-p" . previous-line)
;; ("C-n" . next-line)
;; ("C-s" . isearch-forward)
;; ("C-r" . isearch-backward)
;; ("C-m" . term-send-raw)
;; ("M-f" . term-send-forward-word)
;; ("M-b" . term-send-backward-word)
;; ("M-o" . term-send-backspace)
;; ("M-p" . term-send-up)
;; ("M-n" . term-send-down)
;; ("M-M" . term-send-forward-kill-word)
;; ("M-N" . term-send-backward-kill-word)
;; ("M-r" . term-send-reverse-search-history)
;; ("M-," . term-send-input)
;; ("M-." . comint-dynamic-complete)

;; default (print-list term-unbind-key-list)
;; "C-z"
;; "C-x"
;; "C-c"
;; "C-h"
;; "C-y"
;; "<ESC>"

(setq term-bind-key-alist '(
   ("C-c" . term-interrupt-subjob)

   ("<up>" . term-send-up)
   ("<down>" . term-send-down)

   ("C-r" . term-send-reverse-search-history)

   ("M-f" . term-send-forward-word)
   ("M-b" . term-send-backward-word)
   ("M-d" . term-send-forward-kill-word)
   ("M-DEL" . term-send-backward-kill-word)

   ("M-v" . term-paste)
))

(setq term-unbind-key-list '(
   "C-z"
   "C-x"
   "C-h"
   "C-y"
   "<ESC>"

   ; copy and paste
   "M-v"
   "M-c"
))

;unicode
;(set-terminal-coding-system 'utf-8-unix)
;(setq default-process-coding-system '((utf-8-unix . utf-8-unix)))

;;;;;;;;;;;;;;; transpar ;;;;;;;;;;;;;;;;;;;;;;
;transpose paragraphs
(require 'transpar)


;;;;;;;;;;;;;;; Windows ;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-S-<left>") 'windmove-left)
(global-set-key (kbd "C-S-<right>") 'windmove-right)


;;;;;;;;;;;;;;; Frames ;;;;;;;;;;;;;;;;;;;;;;
; full-screen the frame
; frames are called windows in other applications
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))


;;;;;;;;;;;;;;; Compilation mode ;;;;;;;;;;;;;;;;;;;;;;
(require 'compile)


;; (defun compile-haskell ()
;;  (set (make-local-variable 'compile-command)
;;   "cd ~/dragon/Haskell-DragonNaturallySpeaking; cabal build")
;;  (set 'compilation-scroll-output t)
;; )


;;;;;;;;;;;;;;; Applications ;;;;;;;;;;;;;;;;;;;;;;

(require 'notes-app)
(require 'work-app)
(require 'diary-app)
(require 'observations-app)
(require 'server-app)
(require 'terminal-app)

;;;;;;;;;;;;;;; AUTOSAVE ;;;;;;;;;;;;;;;;;;;;;;

(require 'my-autosave)

(require 'my-speedbar)
(require 'my-frame)


;;;;;;;;;;;;;;; back-button ;;;;;;;;;;;;;;;;;;;;;;
(require 'back-button)
(back-button-mode 1)

;;;;;;;;;;;;;;; buffer ;;;;;;;;;;;;;;;;;;;;;;

;(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;; dired-details-+ ;;;;;;;;;;;;;;;;;;;;;;
(require 'dired-details)
(dired-details-install)

;;;;;;;;;;;;;;; shortcuts ;;;;;;;;;;;;;;;;;;;;;;

(require 'my-shortcuts)
(require 'my-functions)

;;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;
; from
;; kmacro-start-macro
;; kmacro-name-last-macro
;; insert-kbd-macro

(fset 'munge-facebook-songs
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([down 5 M-left M-left left 11 18 98 121 67108896 1 134217848 down 11 11 down] 0 "%d")) arg)))

