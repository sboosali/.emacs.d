;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Global Keybindings:
;;
;; global keybindings for builtin-commands (mostly).
;;
;; 

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Single-Character Keybindings...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "TAB") #'dabbrev-expand)
;(global-set-key (kbd "TAB") #'dabbrev-completion)

(global-set-key (kbd "<backtab>") #'dabbrev-completion)

;; ^ `<backtab>' is (translated from?) `<S-TAB>'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<print>")  #'kill-ring-save)
(global-set-key (kbd "<insert>") #'yank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function-Key Keybindings (`<f_>')...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f1>")  #'dabbrev-completion)
(global-set-key (kbd "<f2>")  #'isearch-forward-regexp)

;; <f3> is 'kmacro-start-macro-or-insert-counter
;; <f4> is 'kmacro-start-macro-or-insert-counter

(global-set-key (kbd "<f7>")  #'list-buffers)
                                        ;TODO fix keyboard layout
;; (global-set-key (kbd "<f5>")  #'list-buffers)
;; (global-set-key (kbd "<f6>")  #'xah-prior-user-buffer)
;; (global-set-key (kbd "<f7>")  #'xah-next-user-buffer) 
;; (global-set-key (kbd "<f8>")  nil)

(global-set-key (kbd "<f9>")  #'undo)
(global-set-key (kbd "<f10>") #'keyboard-quit)

(global-set-key (kbd "<f11>") #'pp-eval-expression) 
(global-set-key (kbd "<f12>") #'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Meta Keybindings (`M-')...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-a") #'mark-whole-buffer-buffer)
(global-set-key (kbd "M-r") #'query-replace-regexp)

(global-set-key (kbd "M-<up>")   #'beginning-of-buffer)
(global-set-key (kbd "M-<down>") #'end-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Control Keybindings (`C-')...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-o") #'other-window)
(global-set-key (kbd "C-;") #'comment-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Control+Meta Keybindings (`C-M-')...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-M-m") #'maximize-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Super (`s-') Keybindings...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Management/Navigation for Buffers/Windows/Frames

(global-set-key (kbd "s-o") #'other-window)       ;TODO;
(global-set-key (kbd "s-s") #'sboo-launch-shell)
(global-set-key (kbd "s-t") #'sboo-launch-term)
(global-set-key (kbd "s-h") #'sboo-split-window-left-right)

;;; Inserting Unicode characters

(global-set-key (kbd "s-,") #'sboo-insert-angle-quote-left)
(global-set-key (kbd "s-.") #'sboo-insert-angle-quote-right)
(global-set-key (kbd "s-=") #'sboo-insert-triple-equals-sign)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "User" Keybindings...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; `C-c...' is reserved for users

;(define-key sboo-map (kbd "")   #')

;; ^ Template: (define-key sboo-map (kbd "")   #')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other Keybindings...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; `ctl-x-map'

(global-set-key (kbd "C-x f")   #'find-file)
(global-set-key (kbd "C-x C-f") #'find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(global-set-key (kbd "<apps>") 'list-buffers)
;(global-set-key (kbd "<menu>") 'list-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;; `*-map'
;;
;; `overriding-local-map'
;; `search-map'
;; `isearch-mode-map'
;; `query-replace-map'
;; `text-mode-map'
;; `comint-mode-map'
;; `flymake-mode-map'
;; `'
;; `'
;;
;; `helm-M-x'
;; `helm-comp-read-map'
;; `snippet-mode-map'
;; `company-active-map'
;; `markdown-mode-mouse-map'


;;; toolbar
;;
;; `tool-bar-map'
;; `compilation-mode-tool-bar-map'
;; `'
;; `'
;; `'

;;; Links:
;;
;; - https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;; - https://emacs.stackexchange.com/questions/2461/how-can-i-simulate-an-arbitary-key-event-from-elisp
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-keybindings)