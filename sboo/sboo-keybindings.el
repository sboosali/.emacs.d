;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Global Keybindings:
;;
;; global keybindings for builtin-commands (mostly).
;;
;; 

;;; Code:

;;==============================================;;
;;; Utilities & Commands =======================;;
;;==============================================;;

;; the Commands — will be bound in a keybinding (below)
;; the Utilities — may be used to define the keybindings (below) 

;;==============================================;;

;;----------------------------------------------;;

(defun sboo-jump ()

  "Jump to definition, `major-mode'-specific.

Links:

• URL `'

Related:

• `helm-etags-select'"

  (interactive)

  (let* ()

    (pcase major-mode

      ('emacs-lisp-mode
       (call-interactively #'xref-find-definitions))

      (_
       (helm-etags-select nil)))))

;;----------------------------------------------;;

(defun sboo-find ()

  "Search for references, `major-mode'-specific."

  (interactive)

  (let* ()

    (pcase major-mode

      ('emacs-lisp-mode
       (call-interactively #'xref-find-references))

      (_
       (helm-etags-select nil)))))

;;----------------------------------------------;;

(defun sboo-find-dwim ()

  "Search for definitions and references (Do-What-I-Mean)."

  (interactive)

  (let* ()

    (sboo-find)))

;;==============================================;;

(defun sboo-swap-parentheses-and-square-brackets ()
  
  "Swap « [ ] » with « (  ) ».

Calls:

• `keyboard-translate'.

Links:

• URL `https://www.cliki.net/Editing+Lisp+Code+with+Emacs'."

  (interactive)
  
  (keyboard-translate ?\( ?\[)
  (keyboard-translate ?\[ ?\()
  
  (keyboard-translate ?\) ?\])
  (keyboard-translate ?\] ?\)))

;;----------------------------------------------;;

(defun sboo-swap-semicolon-and-colon ()
  
  "Swap « ; » with « : ».

Calls:

• `keyboard-translate'.

Links:

• URL `https://www.cliki.net/Editing+Lisp+Code+with+Emacs'."

  (interactive)

  (keyboard-translate ?\: ?\;)
  (keyboard-translate ?\; ?\:))

;;----------------------------------------------;;

(defun sboo-swap-double-quote-with-single-quote ()
  
  "Swap « ' » with « \" ».

Calls:

• `keyboard-translate'.

Links:

• URL `https://www.cliki.net/Editing+Lisp+Code+with+Emacs'."

  (interactive)

  (keyboard-translate ?\" ?\')
  (keyboard-translate ?\' ?\"))

;;==============================================;;

(defun sboo-isearch-dabbrev-expand ()

 "`dabbrev-expand' for the (text in the) search minibuffer.
"

 (interactive)

 (progn

   ;; (with-current-buffer (current-minibuffer) )
   (dabbrev-expand 0) ;;TODO

   ()))
 
;; ^ 
;; 
;; 
;;

;;==============================================;;

(defun sboo-switch-to-previous-buffer ()

  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers.

Calls `switch-to-buffer'.

Links:

• URL `https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/'"

  (interactive)

  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;==============================================;;

(defvar sboo-events-C-g (listify-key-sequence "\C-g")

  "« C-g », as a list of events.")

;;----------------------------------------------;;

(defun sboo-press-C-g ()

  "Press « C-g » (i.e. `keyboard-quit').

Why add events to the event loop, and not just the invoke the command itself?
See URL `https://emacs.stackexchange.com/questions/2461/how-can-i-simulate-an-arbitary-key-event-from-elisp'."

  (interactive)

  (setq unread-command-events sboo-events-C-g))

;; ^ 
;;
;; 

;;==============================================;;

(defun sboo-next-definition ()

  "Jump ahead to the next haskell definition.

Wraps `forward-thing'."

  (interactive)

  (forward-thing 'defun +1))

;; ^ 


(defun sboo-prior-definition ()

  "Jump back to the prior haskell definition.

Wraps `forward-thing'."

  (interactive)

  (forward-thing 'defun -1))

;;----------------------------------------------;;

;;==============================================;;
;;; Key Translations ===========================;;
;;==============================================;;

(sboo-swap-parentheses-and-square-brackets)
(sboo-swap-semicolon-and-colon)
(sboo-swap-double-quote-with-single-quote)

;;==============================================;;
;;; Unset ======================================;;
;;==============================================;;

(global-unset-key (kbd "C-x c"))

;;==============================================;;
;;; Single-Character Keybindings...
;;==============================================;;

(global-set-key (kbd "TAB") #'dabbrev-expand)
;(global-set-key (kbd "TAB") #'dabbrev-completion)

(global-set-key (kbd "<backtab>") #'dabbrev-completion)

;; ^ `<backtab>' is (translated from?) `<S-TAB>'.

;;==============================================;;

(global-set-key (kbd "<print>")  #'kill-ring-save)
(global-set-key (kbd "<insert>") #'yank)

(global-set-key (kbd "<pause>")  #'sboo-press-C-g)

;;==============================================;;
;;; Function-Key Keybindings (`<f_>')...
;;==============================================;;

(global-set-key (kbd "<f1>")  #'dabbrev-completion)
(global-set-key (kbd "<f2>")  #'helm-swoop)

;; ^ Alternatives:
;;
;;   * #'isearch-forward-regexp
;;   * #'sboo-search
;;   * #'helm-swoop

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

;;==============================================;;
;;; Meta Keybindings (`M-')...
;;==============================================;;

(global-set-key (kbd "M-a") #'mark-whole-buffer-buffer)
(global-set-key (kbd "M-r") #'query-replace-regexp)
(global-set-key (kbd "M-g") #'goto-line)

(global-set-key (kbd "M-`") #'sboo-switch-to-previous-buffer)
(global-set-key (kbd "M-~") #'sboo-switch-to-previous-buffer)

(global-set-key (kbd "M-.") #'sboo-jump)

(global-set-key (kbd "M-<up>")   #'beginning-of-buffer)
(global-set-key (kbd "M-<down>") #'end-of-buffer)

;;==============================================;;
;;; Control Keybindings (`C-')...
;;==============================================;;

(global-set-key (kbd "C-o") #'other-window)
(global-set-key (kbd "C-;") #'comment-region)

;;==============================================;;
;;; Control+Meta Keybindings (`C-M-')...
;;==============================================;;

(global-set-key (kbd "C-M-m") #'maximize-frame)

;;==============================================;;
;;; KeyPad (`kp-' Keybindings...
;;==============================================;;

(global-set-key (kbd "<kp-left>")  #'previous-buffer)
(global-set-key (kbd "<kp-right>") #'next-buffer)

(global-set-key (kbd "<kp-up>")    #'sboo-prior-definition)
(global-set-key (kbd "<kp-down>")  #'sboo-next-definition)

(global-set-key (kbd "<kp-begin>") #'sboo-find)

;;(global-set-key (kbd "<kp-home>")   #')
(global-set-key (kbd "<kp-end>")   #'delete-other-windows)

(global-set-key (kbd "<kp-prior>")    #'flycheck-previous-error)
(global-set-key (kbd "<kp-next>")     #'flycheck-next-error)

;;(global-set-key (kbd "<kp-insert>")   #')
;;(global-set-key (kbd "<kp-delete>")   #')
;;(global-set-key (kbd "<kp-enter>")    #')
;;(global-set-key (kbd "<kp-add")       #')
;;(global-set-key (kbd "<kp-subtract>") #')
;;(global-set-key (kbd "<kp-multiply>") #')
;;(global-set-key (kbd "<kp-divide>")   #')
;;(global-set-key (kbd "<kp-")   #')

;;==============================================;;
;;; Super (`s-') Keybindings...
;;==============================================;;

;;==============================================;;
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

;;==============================================;;
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

;;==============================================;;
;; « s-<LETTER> »

;;(global-set-key (kbd "s-a") #')
;;(global-set-key (kbd "s-b") #')
(global-set-key (kbd "s-c") #'sboo-insert-char)
(global-set-key (kbd "s-d") #'xref-find-definitions)
(global-set-key (kbd "s-e") #'eval-last-sexp)
(global-set-key (kbd "s-f") #'describe-function)
;;(global-set-key (kbd "s-g") #')
(global-set-key (kbd "s-h") #'helm-command-prefix)
(global-set-key (kbd "s-i") #'imenu)
;;(global-set-key (kbd "s-j") #')
(global-set-key (kbd "s-k") #'describe-key)
;;(global-set-key (kbd "s-l") #')
;;(global-set-key (kbd "s-m") #')
;;(global-set-key (kbd "s-n") #')
(global-set-key (kbd "s-o") #'find-file-at-point) ;MNEMONIC: "Open file". ;OLD: other-window.
(global-set-key (kbd "s-p") #'proced)
;;(global-set-key (kbd "s-q") #')
(global-set-key (kbd "s-r") #'xref-find-references)
(global-set-key (kbd "s-s") #'sboo-launch-shell)
(global-set-key (kbd "s-t") #'sboo-launch-term)
;;(global-set-key (kbd "s-u") #')
(global-set-key (kbd "s-v") #'describe-variable)
(global-set-key (kbd "s-w") #'list-flycheck-errors)
;;(global-set-key (kbd "s-x") #')
;;(global-set-key (kbd "s-y") #')
;;(global-set-key (kbd "s-z") #')

;;----------------------------------------------;;

;;(global-set-key (kbd "s-d") #'dired)

;;==============================================;;
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

;;==============================================;;

;;; Management/Navigation for Buffers/Windows/Frames

;; (global-set-key (kbd "s-o") #'other-window)       ;TODO;
;; (global-set-key (kbd "s-s") #'sboo-launch-shell)
;; (global-set-key (kbd "s-t") #'sboo-launch-term)
;; (global-set-key (kbd "s-h") #'sboo-split-window-left-right)

;;; Inserting Unicode characters

(global-set-key (kbd "s-M-a") #'sboo-insert-)
;; (global-set-key (kbd "s-M-b") #'sboo-insert-)
;; (global-set-key (kbd "s-M-c") #'sboo-insert-)
;; (global-set-key (kbd "s-M-d") #'sboo-insert-)
;; (global-set-key (kbd "s-M-e") #'sboo-insert-)
;; (global-set-key (kbd "s-M-f") #'sboo-insert-)
;; (global-set-key (kbd "s-M-g") #'sboo-insert-)
;; (global-set-key (kbd "s-M-h") #'sboo-insert-)
;; (global-set-key (kbd "s-M-i") #'sboo-insert-)
;; (global-set-key (kbd "s-M-j") #'sboo-insert-)
;; (global-set-key (kbd "s-M-k") #'sboo-insert-)
;; (global-set-key (kbd "s-M-l") #'sboo-insert-)
;; (global-set-key (kbd "s-M-m") #'sboo-insert-)
;; (global-set-key (kbd "s-M-n") #'sboo-insert-)
(global-set-key (kbd "s-M-o") #'sboo-insert-bullet)
;; (global-set-key (kbd "s-M-p") #'sboo-insert-)
;; (global-set-key (kbd "s-M-q") #'sboo-insert-)
;; (global-set-key (kbd "s-M-r") #'sboo-insert-)
;; (global-set-key (kbd "s-M-s") #'sboo-insert-)
;; (global-set-key (kbd "s-M-t") #'sboo-insert-)
;; (global-set-key (kbd "s-M-u") #'sboo-insert-)
;; (global-set-key (kbd "s-M-v") #'sboo-insert-)
;; (global-set-key (kbd "s-M-w") #'sboo-insert-)
;; (global-set-key (kbd "s-M-x") #'sboo-insert-)
;; (global-set-key (kbd "s-M-y") #'sboo-insert-)
;; (global-set-key (kbd "s-M-z") #'sboo-insert-)

(global-set-key (kbd "s-M-0") #'sboo-insert-null)
(global-set-key (kbd "s-M-1") #'sboo-insert-circled-1)
(global-set-key (kbd "s-M-2") #'sboo-insert-circled-2)
(global-set-key (kbd "s-M-3") #'sboo-insert-circled-3)
(global-set-key (kbd "s-M-4") #'sboo-insert-circled-4)
(global-set-key (kbd "s-M-5") #'sboo-insert-circled-5)
(global-set-key (kbd "s-M-6") #'sboo-insert-circled-6)
(global-set-key (kbd "s-M-7") #'sboo-insert-circled-7)
(global-set-key (kbd "s-M-8") #'sboo-insert-circled-8)
(global-set-key (kbd "s-M-9") #'sboo-insert-circled-9)

(global-set-key (kbd "s-M-,") #'sboo-insert-angle-quote-left)
(global-set-key (kbd "s-M-.") #'sboo-insert-angle-quote-right)
(global-set-key (kbd "s-M--") #'sboo-insert-dash)
(global-set-key (kbd "s-M-=") #'sboo-insert-triple-equals-sign)

;;(global-set-key (kbd "s-M-") #'sboo-insert-)

;;==============================================;;
;;; "User" Keybindings...
;;==============================================;;

;;; `C-c...' is reserved for users

;(define-key sboo-map (kbd "")   #')

;; ^ Template: (define-key sboo-map (kbd "")   #')

;;==============================================;;
;;; Other Keybindings...
;;==============================================;;

;;; `ctl-x-map'

(global-set-key (kbd "C-x f")   #'find-file)
(global-set-key (kbd "C-x C-f") #'find-file)

;;==============================================;;

;(global-set-key (kbd "<apps>") 'list-buffers)
;(global-set-key (kbd "<menu>") 'list-buffers)

;;==============================================;;
;;; Local Keybindings...
;;==============================================;;

;;; `minibuffer-local-map'

(let ((*MAP* minibuffer-local-map))

  (define-key *MAP* (kbd "<tab>")   #'dabbrev-expand)
  (define-key *MAP* (kbd "TAB")     #'dabbrev-expand)

  ())

;; `minibuffer-local-map':
;;
;; Default keymap to use when reading from the minibuffer.

;;==============================================;;

;;; `isearch-mode-map'

(let ((*MAP* isearch-mode-map))

;  (define-key *MAP* (kbd "<f2>")    #'sboo-isearch)

  (define-key *MAP* (kbd "<up>")    #'isearch-ring-retreat)
  (define-key *MAP* (kbd "<down>")  #'isearch-ring-advance)

  (define-key *MAP* (kbd "<left>")  #'isearch-repeat-backward)
  (define-key *MAP* (kbd "<right>") #'isearch-repeat-forward)

  (define-key *MAP* (kbd "<tab>")   #'dabbrev-expand)
  (define-key *MAP* (kbd "TAB")     #'dabbrev-expand)

  ;; (define-key *MAP* (kbd "<tab>")   #'sboo-isearch-dabbrev-expand)

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

;;==============================================;;

;;; `compilation-minor-mode-map'

(when (require 'compile nil t)

      (let ((*MAP* compilation-minor-mode-map))

        (define-key *MAP* (kbd "<kp-next>")  #'compilation-next-error)
        (define-key *MAP* (kbd "<kp-prior>") #'compilation-previous-error)

        ;; (define-key *MAP* (kbd "<kp-down>")  #'compilation-next-file)
        ;; (define-key *MAP* (kbd "<kp-up>")    #'compilation-previous-file)

        ()))

  ;; ^ provides these (single-keypress) keybindings:
  ;; 

;; `compilation-minor-mode-map' is a parent of `compilation-mode-map'.

;;==============================================;;
;;; Notes --------------------------------------;;
;;==============================================;;

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

;;==============================================;;
(provide 'sboo-keybindings)