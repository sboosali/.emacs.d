;;;;;;;;;;;;;;; INIT ;;;;;;;;;;;;;;;;;;;;;;
(setq LOCAL t)

(when LOCAL
  (setq AUCTEX nil)
  (setq HOME "/Users/sambo/")
)


;;;;;;;;;;;;;;; UTIL ;;;;;;;;;;;;;;;;;;;;;;
(defun cons! (x xs) ; ~ add-to-list
  (setq xs (cons x xs)))


;;;;;;;;;;;;;;; SHORTUCTS ;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\M-q" 'save-buffers-kill-terminal)
(global-set-key "\M-`" "\C-xb")
(global-set-key "\M-g" 'goto-line)
;(global-set-key "\M-<up>" 'beginning-of-buffer)
;(global-set-key "\M-<down>" 'end-of-buffer)
(global-set-key "\M-\-" "\C-a-  -  -  -  -  -  -  -\C-o\C-a\C-n")
(global-set-key [(control x) (control b)] 'electric-buffer-list)
(global-set-key [C-return] 'dabbrev-expand)
(global-set-key "\M-o" "\M-f \M-b ")
(global-set-key (kbd "<escape>") 'save-buffer)
(global-set-key "\C-\\" 'find-file)
;(global-set-key (kbd "M-<escape>") 'kmacro-start-macro-or-insert-counter)
;(global-set-key (kbd "<escape>") 'kmacro-end-call-mouse)
(global-set-key (kbd "M-r") 'query-replace)

(defun indent-and-next () (interactive)
  (move-beginning-of-line 1)
  (insert " ")
  (next-line)
  (move-beginning-of-line 1))
(global-set-key "\M-j" 'indent-and-next)

(defun kill-line-save () (interactive)
  (kill-line)
  (yank))
(global-set-key "\M-k" 'kill-line-save)


;;;;;;;;;;;;;;; SETTINGS ;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)

; hide menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;peek
(setq scroll-step            1
      scroll-conservatively  10000)

;;abbreviations
;; tell emacs where to read abbrev definitions from...
(setq abbrev-file-name
      "~/.emacs.d/.abbrev_defs")
;; save abbrevs when files are saved
;; you will be asked before the abbreviations are saved
(setq save-abbrevs t)           
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; if you want it always on:
;(setq default-abbrev-mode t)
;;If you only want it on in text and derived modes, you could do something like this:
(dolist (hook '(text-mode-hook python-mode-hook))
  (add-hook hook (lambda () (abbrev-mode 1))))

(set-face-attribute 'default nil :height 140)

(setq keyboard-coding-system nil)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(setq visible-bell t)

;font-size
;(set-face-attribute 'default nil :height 50)
;(set-frame-parameter nil 'font "Monospace-2")
(when LOCAL 
  (set-default-font "-apple-Monaco-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1"))

(set-background-color "gray")

; reverse video 
;; (defun black-on-white ()
;;   (set-background-color "black")
;;   (set-face-background 'default "black")
;;   (set-face-background 'region "black")
;;   (set-face-foreground 'default "white")
;;   (set-face-foreground 'region "gray60")
;;   (set-foreground-color "white")
;;   (set-cursor-color "red")
;; )
;; (black-on-white)



;;;;;;;;;;;;;;;;;;;;;;;; PYTHON ;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun python-add-breakpoint ()
;;   (interactive)
;;   (py-newline-and-indent)
;;   (insert "import ipdb; ipdb.set_trace()")
;;   (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

;; (define-key py-mode-map (kbd "C-c C-t") 'python-add-breakpoint)

(defun python-init () (interactive)
  (insert "#!/usr/bin/python")
  (newline)
  (insert "from __future__ import division")
  (newline)
  (insert "from sam.sam import *")
  (newline)
  (newline)
  )

(defun python-init-math () (interactive)
    (python-init)
    (insert "from numpy import *")
    (newline)
    (insert "from matplotlib.pyplot import *")
    (newline)
    (insert "import nltk")
    (newline)
    (insert "import numpy.random as sample")
    (newline)
    (insert "import scipy.stats as pdf")
    (newline)
    (newline)
    (newline)
)

(defun python-init-script () (interactive)
  (python-init)
  (insert "from glob import *")
  (newline)
  (insert "import argparse")
  (newline)
  (newline)
  (insert "p = argparse.ArgumentParser()")
  (newline)
  (insert "p.add_argument('file')")
  (newline)
  (insert "args = p.parse_args()")
  (newline)
  (newline)
  )


;;;;;;;;;;;;;;;;;;;;;; COQ ;;;;;;;;;;;;;;;;;;;;;;

;(global-unset-key (kbd "C-RET"))
;(global-set-key (kbd "C-RET") 'proof-add-completions)

;commenting
(global-set-key "\C-]" "\C-a(* \C-e *)\C-n")

(defun coq-mode ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.v$'" buffer-file-name))
    (global-set-key (kbd "M-RET") 'proof-goto-point)))
(add-hook 'find-file-hook 'note-mode)

(global-set-key (kbd "M-RET") 'proof-goto-point)

(when LOCAL
  (let ((default-directory (concat HOME "bin/ProofGeneral/")))
    (normal-top-level-add-subdirs-to-load-path))
  ;(cons! /Users/sambo/bin/ProofGeneral/coq/ex/ex-ssreflect.v ?)
  (setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
  (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
  (load-file (concat HOME "bin/ProofGeneral/generic/proof-site.el"))
)

;(load-file "~/course/195x/pg-setup.el")
;(global-set-key "\C-x k" (lambda ()
;			   (interactive)
;			   (kill-buffer (buffer-name))))


;;;;;;;;;;;;;;;;;;;;;;;; TEX ;;;;;;;;;;;;;;;;;;;;;;;;
(set-input-method "TeX")

;;;;;;;;;;;;;;;;;;;;;;;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;

;;inline latex, insert unicode
;(add-to-list 'auto-mode-alist '("\\.note$" . text-mode))
(defun note-mode ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.note$'" buffer-file-name))
    (set-input-method "TeX")))
;syntax; (add-hook 'Hook 'Mode)
(add-hook 'find-file-hook 'note-mode)
 
;; head
;; (defun goto-head (head)
;;  "notes"
;;  )
;; (global-set-key "\C-h \C-f" 'goto-head)

;; (defun goto-current-head ()
;;  "notes"
;;  (isearch-backwards "head:")
;;  (next-word)
;;  (next-word)
;;  (goto-head (current-word)))



;;;;;;;;;;;;;;; ??? ;;;;;;;;;;;;;;;;;;;;;;

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
      (replace-regexp-in-string "[[:space:]\n]*$" "" 
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))
(set-exec-path-from-shell-PATH)

;;;;;;;;;;;;;;; Haskell ;;;;;;;;;;;;;;;;;;;;;;

(when LOCAL
  (load "~/bin/emacs/haskell-mode/haskell-mode.el")

  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

  (setq auto-mode-alist (cons '("\\.hs$" . haskell-mode) auto-mode-alist))
  )

;;;;;;;;;;;;;;; Prolog ;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
  (cons (cons "\\.pl" 'prolog-mode)
     auto-mode-alist))

;(global-set-key "\C-<right>" 'other-window) ;TODO 'windmove-* maybe?
;(global-set-key "\C-<right>" 'BACKWARDS-other-window)

;;;;;;;;;;;;;;; Octave ;;;;;;;;;;;;;;;;;;;;;;
(autoload 'octave-mode "octave-mod" nil t)
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; frame switching
;(add-to-list 'load-path "/Users/sambo/.emacs.d/frame-tag/")
;(require 'frame-tag)
;(frame-tag-mode 1)

;TODO WHY???
;disable backup
;(setq backup-inhibited t)
;disable auto save
;(setq auto-save-default nil)

;;;;;;;;;;;;;;;;; AUCTEX ;;;;;;;;;;;;;;;;;;;;;
;(normal-top-level-add-subdirs-to-load-path)

;; must it enumerate, what about recursive above? too slow?
(when (and LOCAL AUCTEX)
  (add-to-list 'load-path "~/.emacs.d/auctex/")
  (add-to-list 'load-path "~/.emacs.d/auctex/images/")
  (add-to-list 'load-path "~/.emacs.d/auctex/doc/")
  (add-to-list 'load-path "~/.emacs.d/auctex/style/")

  (add-to-list 'load-path "~/.emacs.d/auctex/preview/")
  (add-to-list 'load-path "~/.emacs.d/auctex/preview/images/")
  (add-to-list 'load-path "/Users/sambo/.emacs.d/auctex/preview/auctex/images/")
  (add-to-list 'load-path "~/.emacs.d/auctex/preview/latex/")
  (add-to-list 'load-path "~/.emacs.d/auctex/preview/auto/")
  (add-to-list 'load-path "~/.emacs.d/auctex/preview/")
  
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)
  
  (setq load-path (cons "/Users/sambo/notes" load-path))
  
					;debug
  (defun display (f)
    "debug"
    (message "%s: %s" f (eval (list f))))
  
  (defun test ()
    "debug"
    (interactive)
    
    (message "\n")
    (display 'current-left-margin)
    (message "current-column:\t\t %s" (current-column))
    (message "current-indentation:\t %s" (current-indentation))
    (message "current-word:\t\t %s" (current-word)))
  (global-set-key "\C-q" 'test)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(preview-gs-options (quote ("-q" "-dSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted"))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(tool-bar-mode nil))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; PySmell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path (concat HOME ".emacs.d/"))
;; (require 'pysmell)
;; (add-hook 'python-mode-hook (lambda () (pysmell-mode 1)))

;; (defvar ac-source-pysmell
;;   '((candidates
;;      . (lambda ()
;;          (require 'pysmell)
;;          (pysmell-get-all-completions))))
;;   "Source for PySmell")

;; (add-hook 'python-mode-hook
;;           '(lambda ()             
;;              (set (make-local-variable 'ac-sources) (append ac-sources '(ac-source-pysmell)))))

