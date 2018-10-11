;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Keybindings
;;
;; global keybindings for builtin-commands (mostly).
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "TAB") #'dabbrev-expand)
;(global-set-key (kbd "TAB") #'dabbrev-completion)

(global-set-key (kbd "<backtab>") #'dabbrev-completion)

;; ^ `<backtab>' is (translated from?) `<S-TAB>'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<print>")  #'kill-ring-save)
(global-set-key (kbd "<insert>") #'yank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f1>")  #'dabbrev-completion)
(global-set-key (kbd "<f2>")  #'isearch-forward-regexp)

;; <f3> is 'kmacro-start-macro-or-insert-counter
;; <f4> is 'kmacro-start-macro-or-insert-counter

(global-set-key (kbd "<f5>")  #'list-buffers)
(global-set-key (kbd "<f6>")  #'xah-prior-user-buffer)
(global-set-key (kbd "<f7>")  #'xah-next-user-buffer) 
(global-set-key (kbd "<f8>")  nil)

(global-set-key (kbd "<f9>")  #'undo)
(global-set-key (kbd "<f10>") #'keyboard-quit)

(global-set-key (kbd "<f11>") #'pp-eval-expression) 
(global-set-key (kbd "<f12>") #'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-a") #'mark-whole-buffer-buffer)
(global-set-key (kbd "M-r") #'query-replace-regexp)

(global-set-key (kbd "M-<up>")   #'beginning-of-buffer)
(global-set-key (kbd "M-<down>") #'end-of-buffer)

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-M-m") 'maximize-frame)

(global-set-key (kbd "C-x f")   #'find-file)
(global-set-key (kbd "C-x C-f") #'find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(global-set-key (kbd "<apps>") 'list-buffers)
;(global-set-key (kbd "<menu>") 'list-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; On `TAB' vs `<tab>':
;;
;; `(kbd "TAB")`, *not* `(kbd "<tab>")`.
;;
;; this distinction is necessary to support tab-as-emacs-completion in all buffers and by default (including `shell-mode`),
;; while still supporting tab-as-bash-completion in a terminal buffer (e.g. `term-mode`).
;;
;; globally, "<tab>" always becomes "TAB" ("translated from"), 
;; then "TAB" becomes `dabbrev-expand` ("is bound to").
;;
;; locally, in `term-mode-map` (see `sboo-term`) "<tab>" always becomes "TAB" ("translated from"),
;; then "TAB" becomes `self-insert-command` ("is bound to").
;;

;;; `dabbrev-expand' vs `dabbrev-completion'
;;
;; See `sboo-completion.el'
;;
;; Alternatives:
;; - (global-set-key (kbd "TAB") #'dabbrev-expand)
;; - (global-set-key (kbd "TAB") #'dabbrev-completion)
;;
;; 

;;; `C-g'
;;
;;     M-: (global-key-binding (kbd "C-g"))
;;     keyboard-quit
;;
;;

;;; Links:
;;
;; - https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;; - https://emacs.stackexchange.com/questions/2461/how-can-i-simulate-an-arbitary-key-event-from-elisp
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-keybindings)