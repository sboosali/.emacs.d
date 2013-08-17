;;;;;;;;;;;;;;; INIT ;;;;;;;;;;;;;;;;;;;;;;
(setq HOME "/Users/sambo/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Packaging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add marmalade repos

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit smex undo-tree magit solarized-theme smart-tabs-mode))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;;;;;;;;;;;;;; UTIL ;;;;;;;;;;;;;;;;;;;;;;
(defun cons! (x xs) ; ~ add-to-list
  (setq xs (cons x xs)))

;;;;;;;;;;;;;;; SETTINGS ;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)

; hide menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;peek
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;;abbreviations
;; tell emacs where to read abbrev definitions from...
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
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
(set-default-font "-apple-Monaco-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")

(set-background-color "gray")

;;   (set-background-color "black")
;;   (set-face-background 'default "black")
;;   (set-face-background 'region "black")
;;   (set-face-foreground 'default "white")
;;   (set-face-foreground 'region "gray60")
;;   (set-foreground-color "white")
;;   (set-cursor-color "red")

;; Prevent Emacs from extending file when
;; pressing down arrow at end of buffer.
(setq next-line-add-newlines nil)
;; Silently ensure newline at end of file
;; (setq require-final-newline t)
;; or make Emacs ask about missing newline
(setq require-final-newline nil)


;;;;;;;;;;;;;;;;;;;;;;;; Smex ;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;; PYTHON ;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun python-debug ()
  "Insert breakpoint above cursor point." (interactive)
  (previous-line)
  (move-end-of-line nil)
  (newline-and-indent)
  (insert "import ipdb;ipdb.set_trace()")
  (save-buffer)
  )

;;;;;;;;;;;;;;;;;;;;;; COQ ;;;;;;;;;;;;;;;;;;;;;;

;(global-unset-key (kbd "C-RET"))
;(global-set-key (kbd "C-RET") 'proof-add-completions)

;commenting
;; (global-set-key "\C-]" "\C-a(* \C-e *)\C-n")

;; (defun coq-mode ()
;;   (when (and (stringp buffer-file-name)
;;              (string-match "\\.v$'" buffer-file-name))
;;     (global-set-key (kbd "M-RET") 'proof-goto-point)))
;; (add-hook 'find-file-hook 'note-mode)

;; (global-set-key (kbd "M-RET") 'proof-goto-point)

;; (let ((default-directory (concat HOME "bin/ProofGeneral/")))
;;   (normal-top-level-add-subdirs-to-load-path))
;; (setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
;; (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
;; (load-file (concat HOME "bin/ProofGeneral/generic/proof-site.el"))

;(load-file "~/course/195x/pg-setup.el")


;;;;;;;;;;;;;;;;;;;;;;;; TEX ;;;;;;;;;;;;;;;;;;;;;;;;
(set-input-method "TeX")

;;;;;;;;;;;;;;;;;;;;;;;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;
;inline latex. inserts unicode.
(defun note-mode ()
  (when (string-match "\\.note$" buffer-file-name)
    (set-input-method "TeX")))
(add-hook 'find-file-hook 'note-mode)

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
(load "~/.emacs.d/haskell-mode/haskell-mode.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(setq auto-mode-alist (cons '("\\.hs$" . haskell-mode) auto-mode-alist))

;;;;;;;;;;;;;;; Prolog ;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
  (cons (cons "\\.pl" 'prolog-mode)
     auto-mode-alist))

;;;;;;;;;;;;;;; Octave ;;;;;;;;;;;;;;;;;;;;;;
(autoload 'octave-mode "octave-mod" nil t)
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Remote Access ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Yelp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

	  (setq py-indent-offset tab-width)
	  (setq python-indent tab-width)
	  (setq py-smart-indentation nil)

	  (setq-default show-trailing-whitespace t)
	  (add-hook 'before-save-hook 'delete-trailing-whitespace)

	  (message "yelp python mode, tabs is %s" indent-tabs-mode))

(add-hook 'python-mode-hook 'my-yelp-python-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; section ;;;;;;;;;;;;;;;;;;;;;;;;;;;;








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
;(global-set-key "\C-<right>" 'other-window) ;TODO 'windmove-* maybe?
;(global-set-key "\C-<right>" 'BACKWARDS-other-window)

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

(defun force-kill-buffer ()
  (interactive)
  (kill-buffer (buffer-name)))

(global-set-key "\C-x k" 'force-kill-buffer)


