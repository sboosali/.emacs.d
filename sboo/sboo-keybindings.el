;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; Global Keybindings:
;;
;; global keybindings for builtin-commands (mostly).
;;
;; 

;; Sections:
;; the Commands — will be bound in a keybinding (below)
;; the Utilities — may be used to define the keybindings (below) 

;;==============================================;;
;;; Code:

;; (defmacro sboo-key (keys-to-bind command-to-be-bound &optional keymap-to-bind-in)

;;   (if (fboundp #'bind-key)

;;       (bind-key keys-to-bind command-to-be-bound keymap-to-bind-in)

;;     (if keymap-to-bind-in

;;         `(define-key keymap-to-bind-in ,(kbd keys-to-bind) ,(function command-to-be-bound))

;;       `(global-set-key ,(kbd keys-to-bind) ,(function command-to-be-bound)))

;;     ;;(define-key (current-global-map)

;;     (define-key keymap-to-bind-in (kbd keys-to-bind) (function command-to-be-bound))

;;     (global-set-key (kbd keys-to-bind) (function command-to-be-bound))))

;; M-: (sboo-key "<s>-c" sboo-insert-char)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-key-compile

  (kbd "<print>")

  "Key to invoke `compile'.")

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-dired ()

  "Wraps `dired'."

  (interactive)

  (let* ((DIRECTORY default-directory)  ; or (file-name-directory (buffer-file-name))
         )

    (dired DIRECTORY)))

;;----------------------------------------------;;

(defun sboo-find-file ()

  "Wraps `find-file-at-point'."

  (interactive)

  (let* (
         )

    (find-file-at-point)))

;;==============================================;;

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

;;==============================================;;

(defun sboo-yas-insert-snippet ()

  "Unconditional `yas-insert-snippet'."

  (interactive)

  (yas-insert-snippet :unconditional))

;;==============================================;;

(defvar-local sboo-insert-function

  nil

  "A reference to a `functionp'.

(For `sboo-insert'.)")

;;----------------------------------------------;;

(defun sboo-insert ()

  "Context-Sensitive `insert'.

For example:

• Calls `sgml-tag' in HTML Mode(s).

Bound to... \\[sbo-insert]

Calls `sboo-insert-function'."

  (interactive)

  (if (bound-and-true-p sboo-insert-function)
      (let* ((VALUE (symbol-value 'sboo-insert-function))
             )
        (cl-typecase VALUE

          (command (call-interactively VALUE))

          (function (funcall VALUE))

          ;;(symbol (... (symbol-value VALUE)))

          (string (insert VALUE))

          (t ())))
    (message "`sboo-insert-function' not locally-defined.")))

;;----------------------------------------------;;

(defun sboo-insert-function-default ()

  "A default `sboo-insert-function'.

Returns:

• `sboo-yas-insert-snippet'.

"

  (interactive)

  (cond

   ((featurep 'yasnippet)

    (if (fboundp #'sboo-yas-insert-snippet)
          #'sboo-yas-insert-snippet
      #'yas-insert-snippet))

   (t nil)))

                                        ;sboo-yas-insert-snippet


;;==============================================;;
;; DWIM Editing

;; each Editing Command:
;;
;; • edits the region, if `use-region-p'.
;; • edits a default Text Object (mostly `word'), otherwise.

;;TODO (sboo-defmarco-region-dwim edit-dwim edit-region edit-object)

;; Define a `*-dwim' wrapper for each of these Editing Command pairs:
;;
;; `upcase-word'
;; `upcase-region'
;;
;; `downcase-word'
;; `downcase-region'
;;
;; `capitalize-word'
;; `capitalize-region'
;;

;;TODO;; LOL these already exist!

;;----------------------------------------------;;
;; `kill-region' or `kill-line'.

(defun sboo-cut-dwim (&optional argument)

  "Do-What-I-Mean: `kill-region' or `kill-line'.

Inputs:

• ARGUMENT — an `integerp'.
  Prefix-Argument. Passthru to `kill-line'."

  (interactive "*p")

  (if (use-region-p)
      (kill-region (region-beginning) (region-end))

    (kill-line argument)))

;;----------------------------------------------;;

(defun sboo-copy-dwim (&optional argument)

  "Do-What-I-Mean: `copy-region' or `copy-buffer'.

Inputs:

• ARGUMENT — an `integerp'.
  Prefix-Argument. Passthru to `'."

  (interactive "*p")

  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))

    (copy-line argument)))

;;==============================================;;
;;; Key Translations ===========================;;
;;==============================================;;

;; (unless t
;;   ;; ^ whether these keyboard translations have already been configured globally
;;   ;;   (e.g. via « xmodmap »)
;;   (sboo-swap-parentheses-and-square-brackets)
;;   (sboo-swap-semicolon-and-colon)
;;   (sboo-swap-double-quote-with-single-quote)
;;   ())

;;==============================================;;
;;; Unset ======================================;;
;;==============================================;;

(progn

  (global-unset-key (kbd "C-x c"))

  ())

;;==============================================;;

(progn

  (global-unset-key (kbd "C-0"))
  (global-unset-key (kbd "C-1"))
  (global-unset-key (kbd "C-2"))
  (global-unset-key (kbd "C-3"))
  (global-unset-key (kbd "C-4"))
  (global-unset-key (kbd "C-5"))
  (global-unset-key (kbd "C-6"))
  (global-unset-key (kbd "C-7"))
  (global-unset-key (kbd "C-8"))
  (global-unset-key (kbd "C-9"))

  ())

;;----------------------------------------------;;

(progn

  (global-unset-key (kbd "M-0"))
  (global-unset-key (kbd "M-1"))
  (global-unset-key (kbd "M-2"))
  (global-unset-key (kbd "M-3"))
  (global-unset-key (kbd "M-4"))
  (global-unset-key (kbd "M-5"))
  (global-unset-key (kbd "M-6"))
  (global-unset-key (kbd "M-7"))
  (global-unset-key (kbd "M-8"))
  (global-unset-key (kbd "M-9"))

  ())

;;==============================================;;
;;; Single-Character Keybindings...
;;==============================================;;

(global-set-key (kbd "TAB") #'dabbrev-expand)
;(global-set-key (kbd "TAB") #'dabbrev-completion)

(global-set-key (kbd "<backtab>") #'dabbrev-completion)

;; ^ `<backtab>' is (translated from?) `<S-TAB>'.

;;==============================================;;

(global-set-key sboo-key-compile      #'compile)
(global-set-key (kbd "<Scroll_Lock>") #'compile)
(global-set-key (kbd "<pause>")       #'sboo-press-C-g)

(global-set-key (kbd "<insert>") #'yank)
;;(global-set-key (kbd "<delete>") #'sboo-undefined)

;;==============================================;;
;;; Function-Key Keybindings (`<f_>')...
;;==============================================;;

(global-set-key (kbd "<f1>")  #'dabbrev-completion)
(global-set-key (kbd "<f2>")  #'helm-swoop)
;; <f3> is 'kmacro-start-macro-or-insert-counter
;; <f4> is 'kmacro-start-macro-or-insert-counter

;; ^ Alternatives:
;;
;;   * #'isearch-forward-regexp
;;   * #'sboo-search
;;   * #'helm-swoop

;;----------------------------------------------;;

;;(global-set-key (kbd "<f5>") nil)          ;; « xfce4 » sends « C-c » on « F5 », i.e. Copy / User-Keymap.
;;(global-set-key (kbd "<f6>") nil)          ;; « xfce4 » sends « C-x » on « F6 », i.e. Cut / Extra-Keymap.
(global-set-key (kbd "<f7>") #'list-buffers)
;;(global-set-key (kbd "<f8>") nil)          ;; « xfce4 » sends « C-v » on « F8 », i.e. Paste.

;;----------------------------------------------;;

(global-set-key (kbd "<f9>")  #'undo)
(global-set-key (kbd "<f10>") #'keyboard-quit)
(global-set-key (kbd "<f11>") #'pp-eval-expression) 
(global-set-key (kbd "<f12>") #'execute-extended-command)

;;==============================================;;
;;; Meta Keybindings (`M-')...
;;==============================================;;

(global-set-key (kbd "M-a") #'mark-whole-buffer)

(global-set-key (kbd "M-r")   #'query-replace-regexp) ; Find/Replace with Regex Queries.
(global-set-key (kbd "M-S-r") #'query-replace)        ; Find/Replace with Fixed Strings.

(global-set-key (kbd "M-g") #'goto-line)

(global-set-key (kbd "M-l") #'downcase-dwim)   ; Shadows: `downcase-word'.
(global-set-key (kbd "M-c") #'capitalize-dwim) ; Shadows: `capitalize-word'.
(global-set-key (kbd "M-u") #'upcase-dwim)     ; Shadows: `upcase-word'.

(global-set-key (kbd "M-`") #'sboo-switch-to-previous-buffer)
(global-set-key (kbd "M-~") #'sboo-switch-to-previous-buffer)

(global-set-key (kbd "M-:") #'comment-dwim) ; Shadows: `eval-expression'.

;; ^ Thus, both « M-; » and « M-S-; » are identical.
;;   NOTE shadows `eval-expression', but `eval-expression' has been bound to « <f11> ».

(global-set-key (kbd "M-.") #'sboo-jump)

(global-set-key (kbd "M-<up>")   #'beginning-of-buffer)
(global-set-key (kbd "M-<down>") #'end-of-buffer)

(global-set-key (kbd "<M-prior>") #'xah-backward-block)
(global-set-key (kbd "<M-next>")  #'xah-forward-block)

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

(global-set-key (kbd "<kp-home>")  #'set-mark-command)
(global-set-key (kbd "<kp-end>")   #'delete-other-windows)

(global-set-key (kbd "<kp-prior>")    #'flycheck-previous-error)
(global-set-key (kbd "<kp-next>")     #'flycheck-next-error)

;;(global-set-key (kbd "<kp-insert>")   #')
;;(global-set-key (kbd "<kp-delete>")   #')
;;(global-set-key (kbd "<kp-enter>")    #')
;;(global-set-key (kbd "<kp-add")       #')

;;(global-set-key (kbd "<kp-divide>")   #')
(global-set-key (kbd "<kp-multiply>") #'sboo-dired)
(global-set-key (kbd "<kp-subtract>") #'sboo-find-file)

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

(global-set-key (kbd "s-!") #'delete-other-windows)
(global-set-key (kbd "s-@") #'split-window-below)
(global-set-key (kbd "s-#") #'split-window-right)
;;(global-set-key (kbd "s-$") #')
;;(global-set-key (kbd "s-%") #')
;;(global-set-key (kbd "s-^") #')
;;(global-set-key (kbd "s-&") #')
;;(global-set-key (kbd "s-*") #')
;;(global-set-key (kbd "s-(") #')
;;(global-set-key (kbd "s-)") #')

;;==============================================;;
;; « s-<LETTER> »

;; `sboo-*-keymap's
;;
;; URL `http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/'
;; URL `https://bendersteed.gitlab.io/post/rediscovering-vanilla-emacs-text-editing/'
;;
;;

;;----------------------------------------------;;

(progn
  (define-prefix-command 'sboo-text-keymap)

;;(define-key sboo-text-keymap (kbd "a") #')
;;(define-key sboo-text-keymap (kbd "b") #')
;;(define-key sboo-text-keymap (kbd "c") #')
;;(define-key sboo-text-keymap (kbd "d") #')
;;(define-key sboo-text-keymap (kbd "e") #')
;;(define-key sboo-text-keymap (kbd "f") #')
;;(define-key sboo-text-keymap (kbd "g") #')
;;(define-key sboo-text-keymap (kbd "h") #')
  (define-key sboo-text-keymap (kbd "i") #'indent-region) ; [I]ndent
;;(define-key sboo-text-keymap (kbd "j") #')
;;(define-key sboo-text-keymap (kbd "k") #')
;;(define-key sboo-text-keymap (kbd "l") #')
;;(define-key sboo-text-keymap (kbd "m") #')
;;(define-key sboo-text-keymap (kbd "n") #')
;;(define-key sboo-text-keymap (kbd "o") #')
;;(define-key sboo-text-keymap (kbd "p") #')
;;(define-key sboo-text-keymap (kbd "q") #')
;;(define-key sboo-text-keymap (kbd "r") #')
  (define-key sboo-text-keymap (kbd "s") #'sort-lines) ; [S]ort
;;(define-key sboo-text-keymap (kbd "t") #')
;;(define-key sboo-text-keymap (kbd "u") #')
;;(define-key sboo-text-keymap (kbd "v") #')
;;(define-key sboo-text-keymap (kbd "w") #')
;;(define-key sboo-text-keymap (kbd "x") #')
;;(define-key sboo-text-keymap (kbd "y") #')
;;(define-key sboo-text-keymap (kbd "z") #')

  #'sboo-text-keymap)

;;----------------------------------------------;;

(progn
  (define-prefix-command 'sboo-paths-keymap)

  ;; ^ aliases and /or keyboard shortcuts for frequently-visited files and/or directories .

;;(define-key sboo-paths-keymap (kbd "a") "")
;;(define-key sboo-paths-keymap (kbd "b") "")
;;(define-key sboo-paths-keymap (kbd "c") "")
;;(define-key sboo-paths-keymap (kbd "d") "")
  (define-key sboo-paths-keymap (kbd "e") "~/.emacs.d" )
;;(define-key sboo-paths-keymap (kbd "f") "")
;;(define-key sboo-paths-keymap (kbd "g") "")
;;(define-key sboo-paths-keymap (kbd "h") "")
;;(define-key sboo-paths-keymap (kbd "i") "")
;;(define-key sboo-paths-keymap (kbd "j") "")
;;(define-key sboo-paths-keymap (kbd "k") "")
;;(define-key sboo-paths-keymap (kbd "l") "")
;;(define-key sboo-paths-keymap (kbd "m") "")
;;(define-key sboo-paths-keymap (kbd "n") "")
;;(define-key sboo-paths-keymap (kbd "o") "")
;;(define-key sboo-paths-keymap (kbd "p") "")
;;(define-key sboo-paths-keymap (kbd "q") "")
;;(define-key sboo-paths-keymap (kbd "r") "")
;;(define-key sboo-paths-keymap (kbd "s") "")
;;(define-key sboo-paths-keymap (kbd "t") "")
;;(define-key sboo-paths-keymap (kbd "u") "")
;;(define-key sboo-paths-keymap (kbd "v") "")
;;(define-key sboo-paths-keymap (kbd "w") "")
;;(define-key sboo-paths-keymap (kbd "x") "")
;;(define-key sboo-paths-keymap (kbd "y") "")
;;(define-key sboo-paths-keymap (kbd "z") "")

  #'sboo-paths-keymap)

;;----------------------------------------------;;

(progn
  (define-prefix-command 'sboo-mode-keymap)

;;(define-key sboo-mode-keymap (kbd "a") #')
;;(define-key sboo-mode-keymap (kbd "b") #')
;;(define-key sboo-mode-keymap (kbd "c") #')
;;(define-key sboo-mode-keymap (kbd "d") #')
;;(define-key sboo-mode-keymap (kbd "e") #')
;;(define-key sboo-mode-keymap (kbd "f") #')
;;(define-key sboo-mode-keymap (kbd "g") #')
;;(define-key sboo-mode-keymap (kbd "h") #')
;;(define-key sboo-mode-keymap (kbd "i") #')
;;(define-key sboo-mode-keymap (kbd "j") #')
;;(define-key sboo-mode-keymap (kbd "k") #')
;;(define-key sboo-mode-keymap (kbd "l") #')
;;(define-key sboo-mode-keymap (kbd "m") #')
  (define-key sboo-mode-keymap (kbd "n") #'sboo-prog-new)
;;(define-key sboo-mode-keymap (kbd "o") #')
;;(define-key sboo-mode-keymap (kbd "p") #')
;;(define-key sboo-mode-keymap (kbd "q") #')
;;(define-key sboo-mode-keymap (kbd "r") #')
;;(define-key sboo-mode-keymap (kbd "s") #')
;;(define-key sboo-mode-keymap (kbd "t") #')
;;(define-key sboo-mode-keymap (kbd "u") #')
;;(define-key sboo-mode-keymap (kbd "v") #')
;;(define-key sboo-mode-keymap (kbd "w") #')
;;(define-key sboo-mode-keymap (kbd "x") #')
;;(define-key sboo-mode-keymap (kbd "y") #')
;;(define-key sboo-mode-keymap (kbd "z") #')

  #'sboo-mode-keymap)

;;----------------------------------------------;;

(progn
  (define-prefix-command 'sboo-mark-keymap)

  (define-key sboo-mark-keymap (kbd "a") #'mark-beginning-of-buffer)
  (define-key sboo-mark-keymap (kbd "b") #'mark-whole-buffer)
;;(define-key sboo-mark-keymap (kbd "c") #'mark-)
  (define-key sboo-mark-keymap (kbd "d") #'mark-defun)
  (define-key sboo-mark-keymap (kbd "e") #'mark-end-of-buffer)
;;(define-key sboo-mark-keymap (kbd "f") #'mark-)
;;(define-key sboo-mark-keymap (kbd "g") #'mark-)
;;(define-key sboo-mark-keymap (kbd "h") #'mark-)
;;(define-key sboo-mark-keymap (kbd "i") #'mark-)
;;(define-key sboo-mark-keymap (kbd "j") #'mark-)
;;(define-key sboo-mark-keymap (kbd "k") #'mark-)
;;(define-key sboo-mark-keymap (kbd "l") #'mark-)
;;(define-key sboo-mark-keymap (kbd "m") #'mark-)
;;(define-key sboo-mark-keymap (kbd "n") #'mark-)
;;(define-key sboo-mark-keymap (kbd "o") #'mark-)
  (define-key sboo-mark-keymap (kbd "p") #'mark-paragraph)
;;(define-key sboo-mark-keymap (kbd "q") #'mark-)
;;(define-key sboo-mark-keymap (kbd "r") #'mark-)
;;(define-key sboo-mark-keymap (kbd "s") #'mark-)
;;(define-key sboo-mark-keymap (kbd "t") #'mark-)
;;(define-key sboo-mark-keymap (kbd "u") #'mark-)
;;(define-key sboo-mark-keymap (kbd "v") #'mark-)
  (define-key sboo-mark-keymap (kbd "w") #'mark-word)
  (define-key sboo-mark-keymap (kbd "x") #'mark-sexp)
;;(define-key sboo-mark-keymap (kbd "y") #'mark-)
;;(define-key sboo-mark-keymap (kbd "z") #'mark-)

  #'sboo-mark-keymap)

;;----------------------------------------------;;

  (progn

  (global-set-key (kbd "s-a") #'sboo-copy-buffer-contents)  ; "copy All"
;;(global-set-key (kbd "s-b") #')
  (global-set-key (kbd "s-c") #'sboo-insert-char)           ; "Character"
  (global-set-key (kbd "s-d") #'xref-find-definitions)      ; "Definitions"
  (global-set-key (kbd "s-e") #'sboo-eval)                  ; "Eval"
  (global-set-key (kbd "s-f") #'describe-function)          ; "Function"
;;(global-set-key (kbd "s-g") #')
  (global-set-key (kbd "s-h") #'helm-apropos)               ; [H]elp
  (global-set-key (kbd "s-i") #'sboo-insert)                ; [I]nsert
;;(global-set-key (kbd "s-j") #')
  (global-set-key (kbd "s-k") #'sboo-mark-keymap)           ; « mar[K]-* » commands.
  (global-set-key (kbd "s-l") #'align-regexp)               ; a"L"ign
  (global-set-key (kbd "s-m") #'sboo-mode-keymap)           ;[M]ode-specific commands.
;;(global-set-key (kbd "s-n") #')
  (global-set-key (kbd "s-o") #'find-file-at-point)         ;MNEMONIC: "Open file". ;OLD: other-window.
  (global-set-key (kbd "s-p") #'helm-show-kill-ring)        ; [P]aste from History.
;;(global-set-key (kbd "s-q") #')
  (global-set-key (kbd "s-r") #'xref-find-references)       ; "References"
  (global-set-key (kbd "s-s") #'sboo-launch-shell)          ; "Shell"
  (global-set-key (kbd "s-t") #'sboo-text-keymap)           ; "Text"
;;(global-set-key (kbd "s-u") #')
  (global-set-key (kbd "s-v") #'describe-variable)          ; "Variable"
  (global-set-key (kbd "s-w") #'list-flycheck-errors)       ; "Warnings & errors"
;;(global-set-key (kbd "s-x") #')
  (global-set-key (kbd "s-y") #'sboo-yas-insert-snippet)    ; [Y]asnippet
;;(global-set-key (kbd "s-z") #')

  ())

;;----------------------------------------------;;

;;(global-set-key (kbd "s-d") #'dired)
;;(global-set-key (kbd "s-h") #'helm-command-prefix)        ; "Helm"
;;(global-set-key (kbd "s-i") #'imenu)
;;(global-set-key (kbd "s-k") #'describe-key)               ; "Key"
;;(global-set-key (kbd "s-t") #'sboo-launch-term)           ; "Terminal"
;;(global-set-key (kbd "s-p") #'proced)

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

(global-set-key (kbd "s-M-'")  #'sboo-insert-left-double-quotation-mark)
(global-set-key (kbd "s-M-\"") #'sboo-insert-right-double-quotation-mark)

(global-set-key (kbd "s-M-<right>") #'sboo-insert-double-right-arrow)

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

;;; `minibuffer-local-filename-completion-map'

(dolist (*MAP* (list minibuffer-local-filename-completion-map
                     minibuffer-local-completion-map
                     minibuffer-local-must-match-map

                     ido-file-completion-map
                     ido-file-dir-completion-map
                     ))

  (define-key *MAP* (kbd "<kp-left>")  #'backward-char)
  (define-key *MAP* (kbd "<kp-right>") #'forward-char)

  ())

;; ^ `minibuffer-local-filename-completion-map':
;;
;; >Local keymap for minibuffer input with completion for filenames.
;;
;; Merged with either:
;;
;; • ‘minibuffer-local-completion-map’ — for `find-file'.
;; • ‘minibuffer-local-must-match-map’ — for `find-file-existing'.

;;==============================================;;

;;; TODO `helm-*-map'

(dolist (*MAP* (list helm-generic-files-map
                     helm-find-files-map
                     helm-read-file-map
                     ))

  (define-key *MAP* (kbd "<kp-left>")  #'backward-char)
  (define-key *MAP* (kbd "<kp-right>") #'forward-char)

  ())

;; ^ `helm-generic-files-map':
;;
;; `helm-generic-files-map' corresponds with (?) `minibuffer-local-filename-completion-map'
;;
;;
;;

;;----------------------------------------------;;

;; TODO `helm-*-map':
;;
;; • `helm-etags-map'
;; • `helm-swoop-edit-map'
;; • `helm-eval-expression-map'
;; • `helm-multi-swoop-edit-map'
;; • `helm-pdfgrep-map'
;; • `helm-map'
;; • `helm-M-x-map'
;; • `helm-grep-map'
;; • `helm-swoop-map'
;; • `helm-moccur-map'
;; • `helm-locate-map'
;; • `helm-buffer-map'
;; • `helm-command-map'
;; • `helm-read-file-map'
;; • `helm-kill-ring-map'
;; • `helm-wikipedia-map'
;; • `helm-comp-read-map'
;; • `helm-grep-mode-map'
;; • `helm-major-mode-map'
;; • `helm-find-files-map'
;; • `helm-multi-swoop-map'
;; • `helm-moccur-mode-map'
;; • `helm--minor-mode-map'
;; • `helm-ff-lynx-style-map'
;; • `helm--remap-mouse-mode'
;; • `helm-generic-files-map'
;; • `helm--remap-mouse-mode-map'
;; • `helm--remap-mouse-mode-hook'
;; • `helm-multi-swoop-buffers-map'
;; • `helm-buffers-ido-virtual-map'
;; • `helm-search-suggest-action-google-maps-url'
;;

;;==============================================;;

(progn

  (defun sboo-set-run-key-to-eval-buffer ()
    (local-set-key sboo-key-compile #'eval-buffer)
    (local-set-key (kbd "<s>-p")    #'eval-buffer)
    ())

  (add-hook 'emacs-lisp-mode-hook
            #'sboo-set-run-key-to-eval-buffer))

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

;; <f*>
;;
;; (global-set-key (kbd "<f5>")  #'list-buffers)
;; (global-set-key (kbd "<f6>")  #'xah-prior-user-buffer)
;; (global-set-key (kbd "<f7>")  #'xah-next-user-buffer) 
;;

;; <*>
;;
;; (global-set-key (kbd "<print>")  #'kill-ring-save)

;;; Links:
;;
;; - https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;; - https://emacs.stackexchange.com/questions/2461/how-can-i-simulate-an-arbitary-key-event-from-elisp
;;

;;==============================================;;
(provide 'sboo-keybindings)