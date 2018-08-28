;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TERM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; the `term` builtin-package.
;;
;; `term-mode` *is* a terminal-emulator;
;; i.e. most emacs keybindings don't work,
;; nor do most emacs editing/navigation behaviors.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package term
  :bind
  (("C-c t" . term)
   :map term-mode-map
   ("<kp-prior>" . term-send-up)
   ("<kp-next>"  . term-send-down)
   ("TAB"        . self-insert-command)))
   ;; ^ "Keys defined by global-set-key are shadowed by any local binding."

;;(global-unset-key (kbd "<tab>"))
;;(define-key term-mode-map (kbd "TAB") 'self-insert-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `(kbd "<tab>")` versus `(kbd "TAB")`:
;; 
;; By default, Emacs translates <tab> (i.e. the single key) to TAB (i.e. `C-i`). But you can bind <tab> and TAB to different actions. In particular, in ASCII, C-i and <TAB> are the same character. If the terminal can distinguish between them, Emacs conveys the distinction to Lisp programs by representing: C-i as the integer 9 (ASCII `\9`); and <TAB> as the symbol `'tab`.
;; 
 
;; See
;;     - https://emacs.stackexchange.com/a/9634/2239
;;
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-term)