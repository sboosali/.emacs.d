;;; sboo-keybindings.el --- Personal keybinding -*- lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Personal `global-map' configuration.
;;
;; Sections:
;;
;; • the Commands  — will be bound in a keybinding (below).
;; • the Utilities — may be used to define the keybindings (below).
;;
;; In this file's keybindings, the keys are bound to:
;;
;; • Builtin Commands (most)  — i.e. defined by Emacs.
;; • Personal Commands (some) — i.e. defined in this file.
;; • Other Commands (some)    — i.e. undefined (until later, they are defined by external packages).
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile 
  (require 'cl-lib))

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

(eval-when-compile

  (cl-defmacro defun-dired (name directory &key doc)

    "Define a command which launches `dired' at DIRECTORY.

• NAME — (unquoted) `symbolp'.
  the function name.
• DIRECTORY — a `stringp'.
  a directory filepath.
• DOC — a `stringp'.
  the function documentation.
  (defaults to the empty string.)

Output:

• an `interactive' `defun' declaration.

Example:

• M-: (pp-macroexpand-expression (defun-dired sboo-dired-emacs \"~/.emacs.d\"))

     ⇒ (defun sboo-dired-emacs ()
     ⇒   \"Launch `dired' at directory « ~/emacs.d ».\"
     ⇒   (interactive)
     ⇒   (dired \"~/emacs.d\"))

Related:

• `dired'"

    (declare (indent 1) (doc-string 3))

    (let* ((NAME     name)
           (DIRECTORY directory)

           (DOCSTRING (or doc
                          (format "Launch `dired' at directory « %s »." DIRECTORY)))
           )

      `(defun ,NAME ()
         ,DOCSTRING
         (interactive)
         (dired ,DIRECTORY)))))

;; ^ e.g. `defun-dired':
;;
;; M-: (pp-macroexpand-expression '(defun-dired sboo-dired-emacs "~/emacs.d"))
;;
;;   ⇒ (defun sboo-dired-emacs ()
;;   ⇒   "Launch `dired' at directory « ~/emacs.d »."
;;   ⇒   (interactive)
;;   ⇒   (dired "~/emacs.d"))
;;

;;----------------------------------------------;;

(eval-when-compile

  (cl-defmacro defun-insert-char (name char &key doc)

    "Define a command which `insert-char's (Unicode Character) CHAR.

• NAME — (unquoted) `symbolp'.
  the function name.
• CHAR — a `characterp'.
  a Unicode Character.
• DOC — a `stringp'.
  the function documentation.
  (defaults to the empty string.)

Output:

• an `interactive' `defun' declaration.

Example:

• M-: (pp-macroexpand-expression '(defun-insert-char sboo-insert-❶ ?❶))

   ⇒ (defun sboo-insert-❶ ()
   ⇒   \"Insert « ❶ » (a.k.a. “INVERSE CIRCLED DIGIT ONE”).\"
   ⇒   (interactive)
   ⇒   (insert-char ?❶))

Related:

• `insert-char'"

    (declare (indent 1) (doc-string 3))

    (let* ((NAME       name)
           (CHAR-VALUE char)
           (CHAR-NAME  (get-char-code-property CHAR-VALUE 'name))

           (DOCSTRING (or doc
                          (format "Insert « %c » (a.k.a. “%s”)." CHAR-VALUE CHAR-NAME)))
           )

      `(defun ,NAME ()
         ,DOCSTRING
         (interactive)
         (insert-char ,CHAR-VALUE)))))

;; ^ e.g. `defun-insert-char':
;;
;; M-: (pp-macroexpand-expression '(defun-insert-char sboo-insert-❶ ?❶))
;;
;;   ⇒ (defun sboo-insert-❶ ()
;;   ⇒   "Insert « ❶ » (a.k.a. “INVERSE CIRCLED DIGIT ONE”)."
;;   ⇒   (interactive)
;;   ⇒   (insert-char ?❶))
;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-key-compile

  (kbd "<print>")

  "Key to invoke `compile'.")

;;----------------------------------------------;;
;; Autoloads -----------------------------------;;
;;----------------------------------------------;;

(autoload 'zap-up-to-char "misc"
  "Kill up to (but not including) the ARG-th occurrence of CHAR.")

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

(defun sboo-page-backward (&optional count)

  "Move to the prior Page-Break (i.e. « ^L »).

Related:

• `forward-page'"

  (interactive "p")

  (let* ((COUNT (* -1 count))
         )

    (forward-page COUNT)))

;; ^ NOTE « (interactive "p") »:
;;
;;  • is the Numeric-Prefix-Argument — i.e. the `prefix-numeric-value' of `current-prefix-arg'.

;;----------------------------------------------;;

(defun sboo-page-forward (&optional count)

  "Move to the next Page-Break (i.e. « ^L »).

Related:

• `forward-page'"

  (interactive "p")

  (forward-page count))

;;TODO or next comment-section. 

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

;;----------------------------------------------;;

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

(progn

  (define-key key-translation-map (kbd "H-4") (kbd "◇")) ; WHITE DIAMOND

  ())

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

(global-set-key (kbd "TAB") #'dabbrev-expand) ; Shadows `indent-according-to-mode'.
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

;; ChromeBook:

(global-set-key (kbd "<XF86Reload>")  #'execute-extended-command)
;(global-set-key (kbd "<XF86Back>")    #')
;(global-set-key (kbd "<XF86Forward>") #')

;;==============================================;;

;;; Function-Key Keybindings (`<f_>')...

;;==============================================;;

(unless (eq #'dabbrev-completion (global-key-binding (kbd "<f1>")))
  (global-set-key (kbd "<f1>") #'dabbrev-completion))     ; « F1 » is the “Complete Key”. Overriden by « (use-package ??? ...) ».

(unless (eq #'helm-swoop (global-key-binding (kbd "<f2>")))
  (global-set-key (kbd "<f2>") #'isearch-forward-regexp)) ; « F2 » is the “Search Key”. Overriden by « (use-package helm-swoop ...) ».

;; <f3> is 'kmacro-start-macro-or-insert-counter
;; <f4> is 'kmacro-start-macro-or-insert-counter

;; ^ Alternatives:
;;
;;   * #'isearch-forward-regexp
;;   * #'sboo-search
;;   * #'helm-swoop

;;----------------------------------------------;;

(global-set-key (kbd "<f5>") #'cua-set-mark) ; a.k.a. « C-a ».
(global-set-key (kbd "<f6>") #'helm-recentf) ;
(global-set-key (kbd "<f7>") #'list-buffers) ; a.k.a. « C-x b ».
(global-set-key (kbd "<f8>") #'find-file)    ; a.k.a. « C-x f ».

;;----------------------------------------------;;

(global-set-key (kbd "<f9>")  #'undo)
(global-set-key (kbd "<f10>") #'keyboard-quit)
(global-set-key (kbd "<f11>") #'pp-eval-expression) 
(global-set-key (kbd "<f12>") #'execute-extended-command)

;;==============================================;;

;;; Meta Keybindings (`M-*')...

;;==============================================;;

(progn

  (global-set-key (kbd "M-a") #'mark-whole-buffer)          ; Select [A]ll.
;;(global-set-key (kbd "M-b") #')
  (global-set-key (kbd "M-c") #'capitalize-dwim)            ; Shadows: `capitalize-word'.
;;(global-set-key (kbd "M-d") #')
;;(global-set-key (kbd "M-e") #')
;;(global-set-key (kbd "M-f") #')
  (global-set-key (kbd "M-g") #'goto-line)                  ; [G]o to Line.
;;(global-set-key (kbd "M-h") #')
;;(global-set-key (kbd "M-i") #')
;;(global-set-key (kbd "M-j") #')
;;(global-set-key (kbd "M-k") #')
  (global-set-key (kbd "M-l") #'downcase-dwim)              ; Shadows: `downcase-word'.
;;(global-set-key (kbd "M-m") #')
;;(global-set-key (kbd "M-n") #')
;;(global-set-key (kbd "M-o") #')
  (global-set-key (kbd "M-p") #'projectile-find-file-dwim)  ; [P]roject Search.
;;(global-set-key (kbd "M-q") #')
  (global-set-key (kbd "M-r") #'query-replace-regexp)       ; [R]eplace (Find/Replace with Regex Queries).
;;(global-set-key (kbd "M-s") #')
;;(global-set-key (kbd "M-t") #')
  (global-set-key (kbd "M-u") #'upcase-dwim)                ; Shadows: `upcase-word'.
;;(global-set-key (kbd "M-v") #')
;;(global-set-key (kbd "M-w") #')
;;(global-set-key (kbd "M-x") #')
;;(global-set-key (kbd "M-y") #')
;;(global-set-key (kbd "M-z") #')

  ())

(global-set-key (kbd "S-M-r") #'query-replace)        ; [R]eplace (Find/Replace with Regex Strings).

(global-set-key (kbd "M-`") #'sboo-switch-to-previous-buffer)
(global-set-key (kbd "M-~") #'sboo-switch-to-previous-buffer)

(global-set-key (kbd "C-:") #'eval-expression)

;; (global-set-key (kbd "M-:") #'comment-dwim) ; Shadows: `eval-expression'.
;;
;; ^ Thus, both « M-; » and « M-S-; » are identical.
;;   NOTE shadows `eval-expression', but `eval-expression' has been bound to « <f11> ».

(global-set-key (kbd "M-.") #'sboo-jump)

(global-set-key (kbd "M-<up>")   #'beginning-of-buffer)
(global-set-key (kbd "M-<down>") #'end-of-buffer)

(global-set-key (kbd "<M-prior>") #'xah-backward-block)
(global-set-key (kbd "<M-next>")  #'xah-forward-block)

;;----------------------------------------------;;

(global-set-key (kbd "M-=") #'count-words)

;; ^ `count-words' is like “count-words-dwim” w.r.t. `count-words-region'.
;;
;;   • URL `http://pragmaticemacs.com/emacs/count-words-in-region-or-buffer'
;;

;;==============================================;;

;;; Control (`C-*') Keybindings...

;;==============================================;;

(progn

;;(global-set-key (kbd "C-a") #')
;;(global-set-key (kbd "C-b") #')
;;(global-set-key (kbd "C-c") #')
;;(global-set-key (kbd "C-d") #')
;;(global-set-key (kbd "C-e") #')
;;(global-set-key (kbd "C-f") #')
;;(global-set-key (kbd "C-g") #')
;;(global-set-key (kbd "C-h") #')
;;(global-set-key (kbd "C-i") #')
;;(global-set-key (kbd "C-j") #')
;;(global-set-key (kbd "C-k") #')
;;(global-set-key (kbd "C-l") #')
;;(global-set-key (kbd "C-m") #')
;;(global-set-key (kbd "C-n") #')
  (global-set-key (kbd "C-o") #'find-file-at-point) ; [O]pen File.
;;(global-set-key (kbd "C-p") #')
;;(global-set-key (kbd "C-q") #')
;;(global-set-key (kbd "C-r") #')
;;(global-set-key (kbd "C-s") #')
;;(global-set-key (kbd "C-t") #')
;;(global-set-key (kbd "C-u") #')
;;(global-set-key (kbd "C-v") #')
;;(global-set-key (kbd "C-w") #')
;;(global-set-key (kbd "C-x") #')
;;(global-set-key (kbd "C-y") #')
;;(global-set-key (kbd "C-z") #')

  ())

(progn

  (global-set-key (kbd "C-;") #'comment-region)

  ())

(progn
  (global-set-key (kbd "<C-prior>") #'sboo-page-backward) ; Overrides: `scroll-right'.
  (global-set-key (kbd "<C-next>")  #'sboo-page-forward)) ; Overrides: `scroll-left'.

(progn
  (global-set-key (kbd "<C-up>")   #'backward-paragraph)
  (global-set-key (kbd "<C-down>") #'forward-paragraph))

;;==============================================;;

;;; Control+Meta (`C-M-*') Keybindings...

;;==============================================;;

(progn

  (global-set-key (kbd "C-M-<tab>")   #'indent-relative)          ; Because we've shadowed it, by binding « TAB » to Tab-Completion.
  (global-set-key (kbd "C-M-S-<tab>") #'indent-according-to-mode) ;

;;(global-set-key (kbd "C-M-a") #'sboo-)
;;(global-set-key (kbd "C-M-b") #'sboo-)
;;(global-set-key (kbd "C-M-c") #'sboo-)
;;(global-set-key (kbd "C-M-d") #'sboo-)
;;(global-set-key (kbd "C-M-e") #'sboo-)
;;(global-set-key (kbd "C-M-f") #'sboo-)
;;(global-set-key (kbd "C-M-g") #'sboo-)
;;(global-set-key (kbd "C-M-h") #'sboo-)
;;(global-set-key (kbd "C-M-i") #'sboo-)
;;(global-set-key (kbd "C-M-j") #'sboo-)
;;(global-set-key (kbd "C-M-k") #'sboo-)
;;(global-set-key (kbd "C-M-l") #'sboo-)
  (global-set-key (kbd "C-M-m") #'maximize-frame)
;;(global-set-key (kbd "C-M-n") #'sboo-)
;;(global-set-key (kbd "C-M-o") #'sboo-)
;;(global-set-key (kbd "C-M-p") #'sboo-)
;;(global-set-key (kbd "C-M-q") #'sboo-)
;;(global-set-key (kbd "C-M-r") #'sboo-)
;;(global-set-key (kbd "C-M-s") #'sboo-)
;;(global-set-key (kbd "C-M-t") #'sboo-)
;;(global-set-key (kbd "C-M-u") #'sboo-)
;;(global-set-key (kbd "C-M-v") #'sboo-)
;;(global-set-key (kbd "C-M-w") #'sboo-)
;;(global-set-key (kbd "C-M-x") #'sboo-)
;;(global-set-key (kbd "C-M-y") #'sboo-)
  (global-set-key (kbd "C-M-z") #'zap-up-to-char)

  ())

;;==============================================;;

;;; KeyPad (`kp-*') Keybindings...

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

;;; Hyper (`H-*') Keybindings...
      
;;==============================================;;

(progn

  (defun-insert-char sboo-insert-❶ ?❶)
  (defun-insert-char sboo-insert-❷ ?❷)
  (defun-insert-char sboo-insert-❸ ?❸)
  (defun-insert-char sboo-insert-❹ ?❹)
  (defun-insert-char sboo-insert-❺ ?❺)
  (defun-insert-char sboo-insert-❻ ?❻)
  (defun-insert-char sboo-insert-❼ ?❼)
  (defun-insert-char sboo-insert-❽ ?❽)
  (defun-insert-char sboo-insert-❾ ?❾)

  ())

;;----------------------------------------------;;

(progn
  ;; Insert Unicode Characters...

  (define-key key-translation-map (kbd "H-o") (kbd "•")) ; BULLET
  (define-key key-translation-map (kbd "H-v") (kbd "◇")) ; WHITE DIAMOND

  (define-key key-translation-map (kbd "H-1") (kbd "❶")) ; INVERSE CIRCLED DIGIT ONE
  (define-key key-translation-map (kbd "H-2") (kbd "❷")) ; INVERSE CIRCLED DIGIT TWO
  (define-key key-translation-map (kbd "H-3") (kbd "❸")) ; INVERSE CIRCLED DIGIT THREE
  (define-key key-translation-map (kbd "H-4") (kbd "❹")) ; INVERSE CIRCLED DIGIT FOUR
  (define-key key-translation-map (kbd "H-5") (kbd "❺")) ; INVERSE CIRCLED DIGIT FIVE
  (define-key key-translation-map (kbd "H-6") (kbd "❻")) ; INVERSE CIRCLED DIGIT SIX
  (define-key key-translation-map (kbd "H-7") (kbd "❼")) ; INVERSE CIRCLED DIGIT SEVEN
  (define-key key-translation-map (kbd "H-8") (kbd "❽")) ; INVERSE CIRCLED DIGIT EIGHT
  (define-key key-translation-map (kbd "H-9") (kbd "❾")) ; INVERSE CIRCLED DIGIT NINE

  (define-key key-translation-map (kbd "s-1") (kbd "❶")) ; INVERSE CIRCLED DIGIT ONE
  (define-key key-translation-map (kbd "s-2") (kbd "❷")) ; INVERSE CIRCLED DIGIT TWO
  (define-key key-translation-map (kbd "s-3") (kbd "❸")) ; INVERSE CIRCLED DIGIT THREE
  (define-key key-translation-map (kbd "s-4") (kbd "❹")) ; INVERSE CIRCLED DIGIT FOUR
  (define-key key-translation-map (kbd "s-5") (kbd "❺")) ; INVERSE CIRCLED DIGIT FIVE
  (define-key key-translation-map (kbd "s-6") (kbd "❻")) ; INVERSE CIRCLED DIGIT SIX
  (define-key key-translation-map (kbd "s-7") (kbd "❼")) ; INVERSE CIRCLED DIGIT SEVEN
  (define-key key-translation-map (kbd "s-8") (kbd "❽")) ; INVERSE CIRCLED DIGIT EIGHT
  (define-key key-translation-map (kbd "s-9") (kbd "❾")) ; INVERSE CIRCLED DIGIT NINE

  ())

;;==============================================;;

;;; Super (`s-*') Keybindings...

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
;; Links:
;;
;;   • URL `http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/'
;;   • URL `https://bendersteed.gitlab.io/post/rediscovering-vanilla-emacs-text-editing/'
;;

;;----------------------------------------------;;

(progn

  (defvar sboo-launch-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for launchning applications/programs.

its “Prefix Command” is bound to « \\[sboo-launch-keymap] ».

\\{sboo-launch-keymap}")   
                           
  (define-prefix-command 'sboo-launch-keymap nil "Σ Run")

;;(define-key sboo-launch-keymap (kbd "a") #')
;;(define-key sboo-launch-keymap (kbd "b") #')
;;(define-key sboo-launch-keymap (kbd "c") #')
 (define-key sboo-launch-keymap (kbd "d") #'sboo-paths-keymap)
;;(define-key sboo-launch-keymap (kbd "e") #')
;;(define-key sboo-launch-keymap (kbd "f") #')
;;(define-key sboo-launch-keymap (kbd "g") #')
;;(define-key sboo-launch-keymap (kbd "h") #')
;;(define-key sboo-launch-keymap (kbd "i") #')
;;(define-key sboo-launch-keymap (kbd "j") #')
;;(define-key sboo-launch-keymap (kbd "k") #')
;;(define-key sboo-launch-keymap (kbd "l") #')
;;(define-key sboo-launch-keymap (kbd "m") #')
;;(define-key sboo-launch-keymap (kbd "n") #')
;;(define-key sboo-launch-keymap (kbd "o") #')
  (define-key sboo-launch-keymap (kbd "p") #'helm-top) ; to[P].
;;(define-key sboo-launch-keymap (kbd "q") #')
;;(define-key sboo-launch-keymap (kbd "r") #')
  (define-key sboo-launch-keymap (kbd "s") #'shell)          ; [S]hell.
  (define-key sboo-launch-keymap (kbd "t") #'sboo-ansi-term) ; [T]erminal.
;;(define-key sboo-launch-keymap (kbd "u") #')
;;(define-key sboo-launch-keymap (kbd "v") #')
  (define-key sboo-launch-keymap (kbd "w") #'eww)
;;(define-key sboo-launch-keymap (kbd "x") #')
;;(define-key sboo-launch-keymap (kbd "y") #')
;;(define-key sboo-launch-keymap (kbd "z") #')

  #'sboo-launch-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-edit-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for editing/manipulating text.

its “Prefix Command” is bound to « \\[sboo-edit-keymap] ».

\\{sboo-edit-keymap}")

  ;; [E]diting Functions"
  (define-prefix-command 'sboo-edit-keymap nil "Σ Edit")

;;(define-key sboo-edit-keymap (kbd "a") #')
;;(define-key sboo-edit-keymap (kbd "b") #')
;;(define-key sboo-edit-keymap (kbd "c") #')
;;(define-key sboo-edit-keymap (kbd "d") #')
  (define-key sboo-edit-keymap (kbd "e") #'sboo-edit-indirect-dwim)
  (define-key sboo-edit-keymap (kbd "f") #'fill-dwim)
;;(define-key sboo-edit-keymap (kbd "g") #')
;;(define-key sboo-edit-keymap (kbd "h") #')
  (define-key sboo-edit-keymap (kbd "i") #'indent-dwim)
;;(define-key sboo-edit-keymap (kbd "j") #')
;;(define-key sboo-edit-keymap (kbd "k") #')
;;(define-key sboo-edit-keymap (kbd "l") #')
;;(define-key sboo-edit-keymap (kbd "m") #')
;;(define-key sboo-edit-keymap (kbd "n") #')
;;(define-key sboo-edit-keymap (kbd "o") #')
;;(define-key sboo-edit-keymap (kbd "p") #')
;;(define-key sboo-edit-keymap (kbd "q") #')
;;(define-key sboo-edit-keymap (kbd "r") #')
;;(define-key sboo-edit-keymap (kbd "s") #')
;;(define-key sboo-edit-keymap (kbd "t") #')
;;(define-key sboo-edit-keymap (kbd "u") #')
;;(define-key sboo-edit-keymap (kbd "v") #')
;;(define-key sboo-edit-keymap (kbd "w") #')
;;(define-key sboo-edit-keymap (kbd "x") #')
;;(define-key sboo-edit-keymap (kbd "y") #')
;;(define-key sboo-edit-keymap (kbd "z") #')

  #'sboo-edit-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-navigate-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for navigation.

its “Prefix Command” is bound to « \\[sboo-navigate-keymap] ».

\\{sboo-navigate-keymap}")

  (define-prefix-command 'sboo-navigate-keymap nil "Σ Nav")

;;(define-key sboo-navigate-keymap (kbd "a") #')
;;(define-key sboo-navigate-keymap (kbd "b") #')
;;(define-key sboo-navigate-keymap (kbd "c") #')
;;(define-key sboo-navigate-keymap (kbd "d") #')
;;(define-key sboo-navigate-keymap (kbd "e") #')
;;(define-key sboo-navigate-keymap (kbd "f") #')
;;(define-key sboo-navigate-keymap (kbd "g") #')
;;(define-key sboo-navigate-keymap (kbd "h") #')
;;(define-key sboo-navigate-keymap (kbd "i") #')
;;(define-key sboo-navigate-keymap (kbd "j") #')
;;(define-key sboo-navigate-keymap (kbd "k") #')
;;(define-key sboo-navigate-keymap (kbd "l") #')
;;(define-key sboo-navigate-keymap (kbd "m") #')
;;(define-key sboo-navigate-keymap (kbd "n") #')
;;(define-key sboo-navigate-keymap (kbd "o") #')
  (define-key sboo-navigate-keymap (kbd "p <up>")   #'sboo-page-backward) ; [P]age-Breaks. (c.f.`page-delimiter').
  (define-key sboo-navigate-keymap (kbd "p <down>") #'sboo-page-forward)  ; [P]age-Breaks. (c.f.`page-delimiter').
;;(define-key sboo-navigate-keymap (kbd "q") #')
;;(define-key sboo-navigate-keymap (kbd "r") #')
;;(define-key sboo-navigate-keymap (kbd "s") #')
;;(define-key sboo-navigate-keymap (kbd "t") #')
;;(define-key sboo-navigate-keymap (kbd "u") #')
;;(define-key sboo-navigate-keymap (kbd "v") #')
;;(define-key sboo-navigate-keymap (kbd "w") #')
;;(define-key sboo-navigate-keymap (kbd "x") #')
;;(define-key sboo-navigate-keymap (kbd "y") #')
;;(define-key sboo-navigate-keymap (kbd "z") #')

  #'sboo-navigate-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-search-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for editor/browser search.

its “Prefix Command” is bound to « \\[sboo-search-keymap] ».

\\{sboo-search-keymap}")

  (define-prefix-command 'sboo-search-keymap nil "Σ Search")

;;(define-key sboo-search-keymap (kbd "a") #')
;;(define-key sboo-search-keymap (kbd "b") #')
;;(define-key sboo-search-keymap (kbd "c") #')
;;(define-key sboo-search-keymap (kbd "d") #')
;;(define-key sboo-search-keymap (kbd "e") #')
;;(define-key sboo-search-keymap (kbd "f") #')
;;(define-key sboo-search-keymap (kbd "g") #')
;;(define-key sboo-search-keymap (kbd "h") #')
;;(define-key sboo-search-keymap (kbd "i") #')
;;(define-key sboo-search-keymap (kbd "j") #')
;;(define-key sboo-search-keymap (kbd "k") #')
;;(define-key sboo-search-keymap (kbd "l") #')
;;(define-key sboo-search-keymap (kbd "m") #')
;;(define-key sboo-search-keymap (kbd "n") #')
;;(define-key sboo-search-keymap (kbd "o") #')
  (define-key sboo-search-keymap (kbd "p") #'projectile-grep)
;;(define-key sboo-search-keymap (kbd "q") #')
;;(define-key sboo-search-keymap (kbd "r") #')
;;(define-key sboo-search-keymap (kbd "s") #')
;;(define-key sboo-search-keymap (kbd "t") #')
;;(define-key sboo-search-keymap (kbd "u") #')
;;(define-key sboo-search-keymap (kbd "v") #')
;;(define-key sboo-search-keymap (kbd "w") #')
;;(define-key sboo-search-keymap (kbd "x") #')
;;(define-key sboo-search-keymap (kbd "y") #')
;;(define-key sboo-search-keymap (kbd "z") #')

  #'sboo-search-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-mark-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for marking/selecting/highlighting text.

its “Prefix Command” is bound to « \\[sboo-mark-keymap] ».

\\{sboo-mark-keymap}")


  (define-prefix-command 'sboo-mark-keymap nil "Σ Mark")

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

  (defvar sboo-insert-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for inserting snippets/characters.

its “Prefix Command” is bound to « \\[sboo-insert-keymap] ».

\\{sboo-insert-keymap}")

  (define-prefix-command 'sboo-insert-keymap nil "Σ Insert")

;;(define-key sboo-insert-keymap (kbd "a") #')
;;(define-key sboo-insert-keymap (kbd "b") #')

  (define-key sboo-insert-keymap (kbd "c h") #'sboo-comment-insert-header)
  (define-key sboo-insert-keymap (kbd "c 1") #'sboo-comment-insert-h1)
  (define-key sboo-insert-keymap (kbd "c 2") #'sboo-comment-insert-h2)
  (define-key sboo-insert-keymap (kbd "c 3") #'sboo-comment-insert-h3)
  (define-key sboo-insert-keymap (kbd "c !") #'sboo-comment-insert-h1)
  (define-key sboo-insert-keymap (kbd "c @") #'sboo-comment-insert-h2)
  (define-key sboo-insert-keymap (kbd "c #") #'sboo-comment-insert-h3)

;;(define-key sboo-insert-keymap (kbd "d") #')
;;(define-key sboo-insert-keymap (kbd "e") #')
;;(define-key sboo-insert-keymap (kbd "f") #')
;;(define-key sboo-insert-keymap (kbd "g") #')
;;(define-key sboo-insert-keymap (kbd "h") #')
;;(define-key sboo-insert-keymap (kbd "i") #')
;;(define-key sboo-insert-keymap (kbd "j") #')
;;(define-key sboo-insert-keymap (kbd "k") #')
;;(define-key sboo-insert-keymap (kbd "l") #')
;;(define-key sboo-insert-keymap (kbd "m") #')
;;(define-key sboo-insert-keymap (kbd "n") #')
;;(define-key sboo-insert-keymap (kbd "o") #')
;;(define-key sboo-insert-keymap (kbd "p") #')
;;(define-key sboo-insert-keymap (kbd "q") #')
;;(define-key sboo-insert-keymap (kbd "r") #')
;;(define-key sboo-insert-keymap (kbd "s") #')
;;(define-key sboo-insert-keymap (kbd "t") #')
;;(define-key sboo-insert-keymap (kbd "u") #')
;;(define-key sboo-insert-keymap (kbd "v") #')
;;(define-key sboo-insert-keymap (kbd "w") #')
;;(define-key sboo-insert-keymap (kbd "x") #')
  (define-key sboo-insert-keymap (kbd "y") #'sboo-yas-insert-snippet)
;;(define-key sboo-insert-keymap (kbd "z") #')

  #'sboo-insert-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-buffer-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for buffer & file operations.

its “Prefix Command” is bound to « \\[sboo-buffer-keymap] ».

\\{sboo-buffer-keymap}")

  ;; [B]uffer (& File) Functions"
  (define-prefix-command 'sboo-buffer-keymap nil "Σ Buffer")

;;(define-key sboo-buffer-keymap (kbd "a") #')
;;(define-key sboo-buffer-keymap (kbd "b") #')
;;(define-key sboo-buffer-keymap (kbd "c") #')
;;(define-key sboo-buffer-keymap (kbd "d") #')
;;(define-key sboo-buffer-keymap (kbd "e") #')
;;(define-key sboo-buffer-keymap (kbd "f") #')
;;(define-key sboo-buffer-keymap (kbd "g") #')
;;(define-key sboo-buffer-keymap (kbd "h") #')
;;(define-key sboo-buffer-keymap (kbd "i") #')
;;(define-key sboo-buffer-keymap (kbd "j") #')
;;(define-key sboo-buffer-keymap (kbd "k") #')
;;(define-key sboo-buffer-keymap (kbd "l") #')
;;(define-key sboo-buffer-keymap (kbd "m") #')
;;(define-key sboo-buffer-keymap (kbd "n") #')
;;(define-key sboo-buffer-keymap (kbd "o") #')
;;(define-key sboo-buffer-keymap (kbd "p") #')
;;(define-key sboo-buffer-keymap (kbd "q") #')
  (define-key sboo-buffer-keymap (kbd "r") #'rename-file)
;;(define-key sboo-buffer-keymap (kbd "s") #')
;;(define-key sboo-buffer-keymap (kbd "t") #')
;;(define-key sboo-buffer-keymap (kbd "u") #')
;;(define-key sboo-buffer-keymap (kbd "v") #')
;;(define-key sboo-buffer-keymap (kbd "w") #')
;;(define-key sboo-buffer-keymap (kbd "x") #')
;;(define-key sboo-buffer-keymap (kbd "y") #')
;;(define-key sboo-buffer-keymap (kbd "z") #')

  #'sboo-buffer-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-paths-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for frequently-visited filepaths.

its “Prefix Command” is bound to « \\[sboo-paths-keymap] ».

\\{sboo-paths-keymap}")

  (define-prefix-command 'sboo-paths-keymap nil "Σ Paths")

  ;; ^ aliases and /or keyboard shortcuts for frequently-visited files and/or directories .

  (defun-dired sboo-dired-emacs         "~/.emacs.d")
  (defun-dired sboo-dired-haskell       "~/haskell")
  (defun-dired sboo-dired-configuration "~/configuration")

;;(define-key sboo-paths-keymap (kbd "a") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "b") #'sboo-dired-)
  (define-key sboo-paths-keymap (kbd "c") #'sboo-dired-configuration)
;;(define-key sboo-paths-keymap (kbd "d") #'sboo-dired-)
  (define-key sboo-paths-keymap (kbd "e") #'sboo-dired-emacs)
;;(define-key sboo-paths-keymap (kbd "f") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "g") #'sboo-dired-)
  (define-key sboo-paths-keymap (kbd "h") #'sboo-dired-haskell)
;;(define-key sboo-paths-keymap (kbd "i") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "j") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "k") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "l") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "m") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "n") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "o") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "p") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "q") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "r") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "s") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "t") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "u") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "v") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "w") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "x") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "y") #'sboo-dired-)
;;(define-key sboo-paths-keymap (kbd "z") #'sboo-dired-)

  #'sboo-paths-keymap)

;;----------------------------------------------;;

(progn

  (defvar sboo-mode-keymap

    (make-sparse-keymap)

    "Personal `keymapp' for mode-specific commands.

its “Prefix Command” is bound to « \\[sboo-mode-keymap] ».

\\{sboo-mode-keymap}")

  (define-prefix-command 'sboo-mode-keymap nil "Σ Mode-Specific")

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

  (global-set-key (kbd "s-a") #'sboo-copy-buffer-contents)  ; copy [A]ll.
  (global-set-key (kbd "s-b") #'sboo-buffer-keymap)         ; "[B]uffer(/file) Functions"
  (global-set-key (kbd "s-c") #'helm-ucs)                   ; U[C]S (Unicode Character Set)
  (global-set-key (kbd "s-d") #'xref-find-definitions)      ; "Definitions"
  (global-set-key (kbd "s-e") #'sboo-edit-keymap)           ; "[E]diting Functions"
  (global-set-key (kbd "s-f") #'describe-function)          ; "Function"
;;(global-set-key (kbd "s-g") #')
  (global-set-key (kbd "s-h") #'helm-apropos)               ; [H]elp
  (global-set-key (kbd "s-i") #'sboo-insert-keymap)         ; [I]nsertion Commands.
;;(global-set-key (kbd "s-j") #')
  (global-set-key (kbd "s-k") #'sboo-mark-keymap)           ; Mar[K] Commands (i.e. « mar[K]-* »).
  (global-set-key (kbd "s-l") #'align-regexp)               ; a[L]ign
  (global-set-key (kbd "s-m") #'sboo-mode-keymap)           ; [M]ode-specific commands.
  (global-set-key (kbd "s-n") #'sboo-navigate-keymap)       ; [N]avigation Commands.
  (global-set-key (kbd "s-o") #'find-file-at-point)         ; [O]pen file.
  (global-set-key (kbd "s-p") #'helm-show-kill-ring)        ; [P]aste from History.
;;(global-set-key (kbd "s-q") #')
  (global-set-key (kbd "s-r") #'sboo-launch-keymap)         ; "[R]un" (a.k.a "launch stuff")
  (global-set-key (kbd "s-s") #'sboo-search-keymap)         ; [S]earch Commands.
  (global-set-key (kbd "s-t") #'sboo-edit-keymap)           ; [T]ext Editing.
;;(global-set-key (kbd "s-u") #')
  (global-set-key (kbd "s-v") #'describe-variable)          ; "Variable"
  (global-set-key (kbd "s-w") #'list-flycheck-errors)       ; "Warnings & errors"
;;(global-set-key (kbd "s-x") #')
  (global-set-key (kbd "s-y") #'sboo-yas-insert-snippet)    ; [Y]asnippet
;;(global-set-key (kbd "s-z") #')

  (global-set-key (kbd "s-:") #'eval-dwim)                  ; c.f. « M-: »

  ())

;;----------------------------------------------;;

;;(global-set-key (kbd "s-c") #'sboo-insert-char)           ; "Character"
;;(global-set-key (kbd "s-d") #'dired)
;;(global-set-key (kbd "s-e") #'eval-dwim)                  ; "Eval"
;;(global-set-key (kbd "s-h") #'helm-command-prefix)        ; "Helm"
;;(global-set-key (kbd "s-i") #'sboo-insert)                ; [I]nsert
;;(global-set-key (kbd "s-i") #'imenu)
;;(global-set-key (kbd "s-k") #'describe-key)               ; "Key"
;;(global-set-key (kbd "s-r") #'xref-find-references)       ; "References"
;;(global-set-key (kbd "s-s") #'sboo-launch-shell)          ; "Shell"
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
;; Mode-Local Keybindings ======================;;
;;==============================================;;

;;; `special-mode-map'

(let ((MAP special-mode-map))

  (define-key MAP ":" #'other-window)

  'special-mode-map)

;; ^ Links:
;;
;;   • URL `https://emacs.stackexchange.com/questions/28289/how-to-define-a-key-in-special-mode'
;;   • URL `https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Keymaps.html'
;;

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
                     ))

  (define-key *MAP* (kbd "<kp-left>")  #'backward-char)
  (define-key *MAP* (kbd "<kp-right>") #'forward-char))

;; ^ `minibuffer-local-filename-completion-map':
;;
;; >Local keymap for minibuffer input with completion for filenames.
;;
;; Merged with either:
;;
;; • ‘minibuffer-local-completion-map’ — for `find-file'.
;; • ‘minibuffer-local-must-match-map’ — for `find-file-existing'.

;;==============================================;;

(with-eval-after-load 'ido

  (dolist (*MAP* (list ido-file-completion-map
                       ido-file-dir-completion-map
                       ))

    (define-key *MAP* (kbd "<kp-left>")  #'backward-char)
    (define-key *MAP* (kbd "<kp-right>") #'forward-char)))

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


;;; `toolbar':
;;
;; `tool-bar-map'
;; `compilation-mode-tool-bar-map'
;; `'
;; `'
;; `'

;;; Links:
;;
;;  • URL `https://www.masteringemacs.org/article/mastering-key-bindings-emacs'
;;  • URL `https://emacs.stackexchange.com/questions/2461/how-can-i-simulate-an-arbitary-key-event-from-elisp'
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

;;; sboo-keybindings.el ends here