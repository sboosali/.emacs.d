;;;;;;;;;;;;;;; INIT ;;;;;;;;;;;;;;;;;;;;;;
(setq HOME "/Users/sambo/")
(add-to-list 'load-path "~/.emacs.d/packages/")

(defun key (key act)
  (global-set-key key act))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Packaging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add marmalade repos

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar packages '(starter-kit smex undo-tree magit solarized-theme smart-tabs-mode)) ; scala-mode
(dolist (p packages)
  (unless (package-installed-p p)
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Deft ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'deft)

(setq deft-extension "txt")
(setq deft-directory "~/Dropbox/deft")

(setq deft-text-mode 'markdown-mode)

(global-set-key [f8] 'deft)

;;;;;;;;;;;;;;; UTIL ;;;;;;;;;;;;;;;;;;;;;;
(defun cons! (x xs) ; ~ add-to-list
  (setq xs (cons x xs)))

(defun print-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))


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

(defun python-init () (interactive)
  (insert "#!/usr/bin/python")
  (newline)
  (insert "from __future__ import division")
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
    (insert "import scipy.stats as pdfs")
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
  (insert "cmdln = argparse.ArgumentParser()")
  (newline)
  (insert "cmdln.add_argument('file')")
  (newline)
  (insert "args = cmdln.parse_args()")
  (newline)
  (newline)
  )

(defun python-debug ()
  "Insert breakpoint above cursor point." (interactive)
  (previous-line)
  (move-end-of-line nil)
  (newline-and-indent)
  (insert "import pdb; pdb.set_trace()")
  (save-buffer)
  )

(defun python-main ()
  "Insert breakpoint above cursor point." (interactive)
  (move-beginning-of-line nil)
  (insert "if __name__=='__main__':")
  (newline)
  (indent)
  (save-buffer)
  )


;;;;;;;;;;;;;;;;;;;;;;;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;
;inline latex. inserts unicode.
(defun note-mode ()
  (when (or (string-match "\\.note$" buffer-file-name)
            (string-match "\\.md$" buffer-file-name)
            (string-match "\\.agda$" buffer-file-name))
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

;; ;;;;;;;;;;;;;;; Haskell ;;;;;;;;;;;;;;;;;;;;;;
;(add-to-list 'load-path "~/lib/emacs/haskell-mode/")
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))


;; ;;;;;;;;;;;;;;; Prolog ;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))


;; ;;;;;;;;;;;;;;; Scala ;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat HOME ".emacs.d/scala-mode2/"))
(require 'scala-mode2)


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

(setq
 term-bind-key-alist
 '(
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

(setq
 term-unbind-key-list
 '(
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

;;;;;;;;;;;;;;; Windows ;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-S-<left>") 'windmove-left)
(global-set-key (kbd "C-S-<right>") 'windmove-right)


;;;;;;;;;;;;;;; Frames ;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))


;;;;;;;;;;;;;;; Applications ;;;;;;;;;;;;;;;;;;;;;;

(defun terminal-app ()
  (split-window-horizontally)
  (multi-term)
  (other-window 1)
  (multi-term)
;  (insert "ipy3")
;  (newline)
  (other-window 1)
)
(if (string-match "Terminal\\.app" (getenv "EMACSPATH"))
    (terminal-app))

(defun notes-app ()
  (find-file "~/Dropbox/.note")
  (end-of-buffer)
)
(if (string-match "Notes\\.app" (getenv "EMACSPATH"))
    (notes-app))

(defun obs-app ()
  (find-file "~/Dropbox/.obs")
  (next-line)
)
(if (string-match "Obs\\.app" (getenv "EMACSPATH"))
    (obs-app))

(defun diary-app ()
  (find-file "~/diary/sleep")
)
(if (string-match "Diary\\.app" (getenv "EMACSPATH"))
    (diary-app))

(defun work-app ()
  (find-file "~/TODO")
  (end-of-buffer)

  ;; (split-window-horizontally)
  ;; (find-file "/Users/sambo/Library/Application Support/LightTable/plugins/Claire/README.md")
  ;; (find-file "/Users/sambo/poc/README")
  ;; (other-window 1)
  ;; (find-file "~/poc_everything/example-lighttable-plugin/README.md")
)
(if (string-match "Work\\.app" (getenv "EMACSPATH"))
    (work-app))


;;;;;;;;;;;;;;; AUTOSAVE ;;;;;;;;;;;;;;;;;;;;;;
;(setq auto-save-timeout 1)
;(setq auto-save-interval 20)

(run-with-idle-timer 5 t 'save-buffer)


;;;;;;;;;;;;;;; SHORTUCTS ;;;;;;;;;;;;;;;;;;;;;;

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

(defun force-kill-buffer () (interactive)
  (kill-buffer (buffer-name)))
(global-set-key "\C-x k" 'force-kill-buffer)

(defun delete-line ()
  "Deletes a line, but preserves the kill-ring."
  (interactive)
  (if (not (equal (point) (point-max))) ; end of buffer
      (kill-line)
    (setq kill-ring (cdr kill-ring))))

(defun new-note () (interactive)
  (end-of-buffer)
  (backward-paragraph)
  (forward-paragraph)
  (delete-line) (delete-line) (delete-line) (delete-line)
  (newline) (newline))
(global-set-key "\M-n" 'new-note)

(global-set-key "\M-c" 'kill-ring-save)
(global-set-key "\M-v" 'yank)

(global-set-key "\M-w" 'capitalize-word)

(global-set-key "\M-q" 'save-buffers-kill-terminal)
(global-set-key "\M-`" "\C-xb")
(global-set-key "\M-g" 'goto-line)
(global-set-key [(meta up)] 'beginning-of-buffer)
(global-set-key [(meta down)] 'end-of-buffer)
(global-set-key "\C-x\C-o" 'other-window)
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\C-x\C-k" 'kill-this-buffer)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\M-m" 'switch-to-buffer)
(global-set-key (kbd "<escape>") 'save-buffer)
(global-set-key "\M-s" 'save-buffer)
(global-set-key "\C-\\" 'find-file)

(global-set-key "\M--" "\C-a-  -  -  -  -  -  -  -\C-o\C-a\C-n")
(global-set-key (kbd "M-0") nil)
(global-set-key "\M-i" 'ucs-insert)

(global-set-key [C-return] 'dabbrev-expand)
(global-set-key (kbd "<tab>") 'dabbrev-expand)

;(global-set-key (kbd "M-<escape>") 'kmacro-start-macro-or-insert-counter)
;(global-set-key (kbd "<escape>") 'kmacro-end-call-mouse)
;(global-set-key "\C-<right>" 'other-window) ;TODO 'windmove-* maybe?
;(global-set-key "\C-<right>" 'BACKWARDS-other-window)

(global-set-key "\M-r" 'query-replace-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

(global-set-key "\M-z" 'undo)

(defun transpose-paragraph () (interactive)
 (backward-paragraph)
 (kill-paragraph)
 (backward-paragraph)
 (yank))

(global-set-key "\M-T" 'transpose-paragraph)
