;;; -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  ;; :pin melpa-stable
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-cross-lines nil)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-search-case 'sensitive)
  (setq evil-ex-search-vim-style-regexp t)
  :config
  (evil-mode 1)
  (add-hook 'git-commit-mode-hook #'evil-insert-state)
  (general-nmap "M-s" #'evil-window-split)
  (general-nmap "M-v" #'evil-window-vsplit)
  (general-nmap "M-c" #'delete-window)
  (general-nmap "M-=" #'balance-windows)
  (general-nmap "M-j" #'evil-window-down)
  (general-nmap "M-k" #'evil-window-up)
  (general-nmap "M-h" #'evil-window-left)
  (general-nmap "M-l" #'evil-window-right)
  (general-nmap :keymaps 'text-mode-map "[ p" #'evil-unimpaired/paste-above)
  (general-nmap :keymaps 'text-mode-map "] p" #'evil-unimpaired/paste-below)
  (general-nmap :keymaps 'text-mode-map "[ b" #'switch-to-prev-buffer)
  (general-nmap :keymaps 'text-mode-map "] b" #'switch-to-next-buffer))

(use-package evil-surround
  ;; :pin melpa-stable
  :config
  (global-evil-surround-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-evil)