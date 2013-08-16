
;;;;; reed's .emacs ;;;;;

;; Add marmalade repos
(require 'package)
(add-to-list 'package-archives
             ;'("marmalade" . "http://marmalade-repo.org/packages/")
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit smex undo-tree magit solarized-theme smart-tabs-mode))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))



;; Use smex for M-x
(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))

;; Prevent Emacs from extending file when
;; pressing down arrow at end of buffer.
(setq next-line-add-newlines nil)
;; Silently ensure newline at end of file
;; (setq require-final-newline t)
;; or make Emacs ask about missing newline
(setq require-final-newline nil)





;; cheetah mode
(define-derived-mode cheetah-mode html-mode "Cheetah"
  (make-face 'cheetah-variable-face)
  (font-lock-add-keywords
   nil
   '(
     ("\\(#\\(from\\|else\\|include\\|extends\\|set\\|def\\|import\\|for\\|if\\|end\\)+\\)\\>" 1 font-lock-type-face)
     ("\\(#\\(from\\|for\\|end\\)\\).*\\<\\(for\\|import\\|def\\|if\\|in\\)\\>" 3 font-lock-type-face)
     ("\\(##.*\\)\n" 1 font-lock-comment-face)
     ("\\(\\$\\(?:\\sw\\|}\\|{\\|\\s_\\)+\\)" 1 font-lock-variable-name-face))
   )
  (font-lock-mode 1)
  (setq indent-tabs-mode 1)
  (setq tab-width 4))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . cheetah-mode))

(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))



;; yelp tabs
(add-to-list 'load-path "~/.emacs.d/scripts")
(require 'smart-tabs-mode)
(smart-tabs-advice python-indent-line-1 python-indent)
(defun my-yelp-python-mode-hook ()
	  (setq tab-width 4)
	  (setq py-indent-offset 4)
          (let ((yelp-project-p (and buffer-file-name (or (string-match "/pg/yelp-main/" (buffer-file-name))
                                                          (string-match "/pg/clientlibs/" (buffer-file-name))
                                                          (string-match "/pg/submodules" (buffer-file-name))
                                                          (and (string-match "/pg/services/" (buffer-file-name))
                                                               (not (or (string-match "/zygote/" (buffer-file-name))
                                                                        (string-match "/rate_tracker/" (buffer-file-name))
                                                                        (string-match "/google/" (buffer-file-name)))))))))
        (when yelp-project-p
          (setq indent-tabs-mode yelp-project-p)
          (smart-tabs-mode)))

	  ;; (if indent-tabs-mode (setq tab-width 3)) ;; for funsies
	  (setq py-indent-offset tab-width)
	  (setq python-indent tab-width)
	  (setq py-smart-indentation nil)

	  (setq-default show-trailing-whitespace t)
	  (add-hook 'before-save-hook 'delete-trailing-whitespace)

	  (message "yelp python mode, tabs is %s" indent-tabs-mode))

(add-hook 'python-mode-hook 'my-yelp-python-mode-hook)


;;;;; my .emacs ;;;;;


(global-set-key "\C-w" 'clipboard-kill-region)
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)


;; Helpers
; ~ add-to-list
(defun cons! (x xs)
  (setq xs (cons x xs)))

(setq LOCAL t)

(when LOCAL
  (setq AUCTEX nil)
  (setq HOME "/Users/sambo/")
)

;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;(global-set-key [(control x) (control b)] 'electric-buffer-list)

;;peek
;; like C-n C-l C-l C-l
;;recenter-top-bottom
(global-set-key "\M-n" "\C-n\C-l\C-l\C-l")
(setq scroll-step            1
      scroll-conservatively  10000)

;commenting
(global-set-key "\C-]" "\C-a(* \C-e *)\C-n")
;(cond major-mode
;      []
;      []
;)

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


(global-set-key [C-return] 'dabbrev-expand)

(setq inhibit-splash-screen t)

(global-set-key (kbd "<escape>") 'save-buffer)
(global-set-key "\C-\\" 'find-file)

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

(global-set-key (kbd "M-r") 'query-replace)

(global-set-key (kbd "C-;") 'backward-kill-word)

(set-face-attribute 'default nil :height 130)

;disable backup
(setq backup-inhibited t)

(setq keyboard-coding-system nil)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq visible-bell t)

;;;;;;;;;;;;;;; SHORTUCTS ;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-\-" "\C-a-  -  -  -  -  -  -  -  -  -  -  -  -  -  -\C-o\C-a\C-n\C-o\C-o")
(global-set-key "\M-`" "\C-xb")
(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)

(set-background-color "gray")

; reverse video
(defun black-on-white ()
  (set-background-color "black")
  (set-face-background 'default "black")
  (set-face-background 'region "black")
  (set-face-foreground 'default "white")
  (set-face-foreground 'region "gray60")
  (set-foreground-color "white")
  (set-cursor-color "red")
)
;(black-on-white)

(defun python-debug ()
  "Insert breakpoint above cursor point."
  (interactive)
  (beginning-of-line)
  (open-line)
  (insert "import ipdb;ipdb.set_trace()")
  )
