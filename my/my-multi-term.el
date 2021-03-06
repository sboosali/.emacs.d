(provide 'my-multi-term)
(require 'my-functions)

(setq multi-term-program "/run/current-system/sw/bin/bash") ; NixOS


;; (when (equal system-type 'darwin)
;;   (set-exec-path-from-shell-path))

;; describe-variable term-bind-key-alist
;; (("C-c C-c" . term-interrupt-subjob)
;;  ("C-p" . previous-line)
;;  ("C-n" . next-line)
;;  ("C-s" . isearch-forward)
;;  ("C-r" . isearch-backward)
;;  ("C-m" . term-send-raw)
;;  ("M-f" . term-send-forward-word)
;;  ("M-b" . term-send-backward-word)
;;  ("M-o" . term-send-backspace)
;;  ("M-p" . term-send-up)
;;  ("M-n" . term-send-down)
;;  ("M-M" . term-send-forward-kill-word)
;;  ("M-N" . term-send-backward-kill-word)
;;  ("M-r" . term-send-reverse-search-history)
;;  ("M-," . term-send-input)
;;  ("M-." . comint-dynamic-complete))

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

;; describe-variable term-unbind-key-list
;; "C-z"
;; "C-x"
;; "C-c"
;; "C-h"
;; "C-y"
;; "<ESC>"

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

