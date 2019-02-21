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

;; ^ Alternatives:
;;
;;   * #'isearch-forward-regexp
;;   * #'sboo-search
;;   * 

;; <f3> is 'kmacro-start-macro-or-insert-counter
;; <f4> is 'kmacro-start-macro-or-insert-counter

(global-set-key (kbd "<f7>")  #'list-buffers)
                                        ;TODO fix keyboard layout
;; (global-set-key (kbd "<f5>")  #'list-buffers)
;; (global-set-key (kbd "<f6>")  #'xah-prior-user-buffer)
;; (global-set-key (kbd "<f7>")  #'xah-next-user-buffer) 
;; (global-set-key (kbd "<f8>")  nil)
(global-set-key (kbd "<f8>")  'compile)                     ;; 

(global-set-key (kbd "<f9>")  #'undo)
(global-set-key (kbd "<f10>") #'keyboard-quit)

(global-set-key (kbd "<f11>") #'pp-eval-expression) 
(global-set-key (kbd "<f12>") #'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Meta Keybindings (`M-')...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-a") #'mark-whole-buffer-buffer)
(global-set-key (kbd "M-r") #'query-replace-regexp)
(global-set-key (kbd "M-g") #'goto-line)

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
;;; KeyPad (`kp-' Keybindings...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO; (add-to-list 'display-buffer-alist '("\\*Flycheck errors\\*" display-buffer-same-window (inhibit-same-window)))

(defun sboo-flycheck ()
  (interactive)
  (progn
    (delete-other-windows)
    (flycheck-list-errors)
    ()))

(global-set-key (kbd "<kp-divide>") #'sboo-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Super (`s-') Keybindings...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; « s-<FUNCTIONKEY> »

;;(global-set-key (kbd "s-<f1>")  #')
;;(global-set-key (kbd "s-<f2>")  #')
(global-set-key (kbd "s-<f3>")  #'sboo-kmacro-insert-counter-letter)
;;(global-set-key (kbd "s-<f4>")  #')
;;(global-set-key (kbd "s-<f5>")  #')
;;(global-set-key (kbd "s-<f6>")  #')
;;(global-set-key (kbd "s-<f7>")  #')
;;(global-set-key (kbd "s-<f8>")  #')
;;(global-set-key (kbd "s-<f9>")  #')
;;(global-set-key (kbd "s-<f10>") #')
;;(global-set-key (kbd "s-<f11>") #')
;;(global-set-key (kbd "s-<f12>") #')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; « s-<NUMBER> »

(global-set-key (kbd "s-1") #'delete-other-windows)
(global-set-key (kbd "s-2") #'split-window-below)
(global-set-key (kbd "s-3") #'split-window-right)
;;(global-set-key (kbd "s-4") #')
;;(global-set-key (kbd "s-5") #')
;;(global-set-key (kbd "s-6") #')
;;(global-set-key (kbd "s-7") #')
;;(global-set-key (kbd "s-8") #')
;;(global-set-key (kbd "s-9") #')
;;(global-set-key (kbd "s-0") #')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; « s-<LETTER> »

;;(global-set-key (kbd "s-a") #')
;;(global-set-key (kbd "s-b") #')
;;(global-set-key (kbd "s-c") #')
(global-set-key (kbd "s-d") #'dired)
(global-set-key (kbd "s-e") #'eval-last-sexp)
(global-set-key (kbd "s-f") #'find-file-at-point)
;;(global-set-key (kbd "s-g") #')
(global-set-key (kbd "s-h") #'sboo-split-window-left-right) ;TODO
;;(global-set-key (kbd "s-i") #')
;;(global-set-key (kbd "s-j") #')
;;(global-set-key (kbd "s-k") #')
;;(global-set-key (kbd "s-l") #')
;;(global-set-key (kbd "s-m") #')
;;(global-set-key (kbd "s-n") #')
(global-set-key (kbd "s-o") #'other-window)
(global-set-key (kbd "s-p") #'proced)
;;(global-set-key (kbd "s-q") #')
;;(global-set-key (kbd "s-r") #')
(global-set-key (kbd "s-s") #'sboo-launch-shell)
(global-set-key (kbd "s-t") #'sboo-launch-term)
;;(global-set-key (kbd "s-u") #')
;;(global-set-key (kbd "s-v") #')
(global-set-key (kbd "s-w") #'list-flycheck-errors)
;;(global-set-key (kbd "s-x") #')
;;(global-set-key (kbd "s-y") #')
;;(global-set-key (kbd "s-z") #')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; « s-<SYMBOL> »

;;(global-set-key (kbd "s-`")           #')
(global-set-key (kbd "s--")           #'text-scale-decrease)
(global-set-key (kbd "s-=")           #'text-scale-increase)
;;(global-set-key (kbd "s-<backspace>") #')
;;(global-set-key (kbd "s-TAB")         #'dabbrev-completion)
;;(global-set-key (kbd "s-[")           #')
;;(global-set-key (kbd "s-]")           #')
;;(global-set-key (kbd "s-\\")          #')
;;(global-set-key (kbd "s-;")           #')
;;(global-set-key (kbd "s-'")           #')
;;(global-set-key (kbd "s-RET")         #')
;;(global-set-key (kbd "s-,")           #')
;;(global-set-key (kbd "s-.")           #')
;;(global-set-key (kbd "s-/")           #')
;;(global-set-key (kbd "s-SPC")         #')
;;(global-set-key (kbd "s-<up>")        #')
;;(global-set-key (kbd "s-<down>")      #')
;;(global-set-key (kbd "s-<left>")      #')
;;(global-set-key (kbd "s-<right>")     #')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Management/Navigation for Buffers/Windows/Frames

;; (global-set-key (kbd "s-o") #'other-window)       ;TODO;
;; (global-set-key (kbd "s-s") #'sboo-launch-shell)
;; (global-set-key (kbd "s-t") #'sboo-launch-term)
;; (global-set-key (kbd "s-h") #'sboo-split-window-left-right)

;;; Inserting Unicode characters

(global-set-key (kbd "s-M-,") #'sboo-insert-angle-quote-left)
(global-set-key (kbd "s-M-.") #'sboo-insert-angle-quote-right)

(global-set-key (kbd "s-M--") #'sboo-insert-dash)
(global-set-key (kbd "s-M-=") #'sboo-insert-triple-equals-sign)
(global-set-key (kbd "s-M-0") #'sboo-insert-null)

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
;;; Local Keybindings...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; `isearch-mode-map'

(progn
  (define-key isearch-mode-map (kbd "<up>")    #'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<down>")  #'isearch-ring-advance)

  (define-key isearch-mode-map (kbd "<left>")  #'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<right>") #'isearch-repeat-forward)
  ())

  ;; ^ provides these (single-keypress) keybindings:
  ;; 
  ;; → 
  ;; next occurrence
  ;; 
  ;; ← 
  ;; previous occurrence
  ;; 
  ;; ↑
  ;; previous search term
  ;; 
  ;; ↓
  ;; next search term
  ;; 
  ;; Enter
  ;; exit isearch
  ;; 

;; ^ 
;;TODO minibuffer-local-isearch-map (kbd "<f2>") #'isearch-repeat-forward)
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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