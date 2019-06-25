;;; sboo-init-keybindings.el --- Personal keybinding -*- lexical-binding: t -*-

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

    (declare (debug nil)
             (indent 1)
             (doc-string 3))

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

(defvar sboo-key/keyboard-quit (kbd "<f10>")

  "“Universal Key” to invoke a Quit Command.

To be bound to:

• `keyboard-quit'
• `quit'

In some `major-mode's, this key can be bound to a similar (but
different) command. For example, `*-quit' in some `*-mode-map'.")

;;----------------------------------------------;;

(defvar sboo-key/copy (kbd "<print>")

  "“Universal Key” to invoke a Clipboard Copy.

To be bound to:

• `copy-dwim'
• `clipboard-kill-ring-save'
• `kill-ring-save'")

;;----------------------------------------------;;

(defvar sboo-key/paste (kbd "<insert>")

  "“Universal Key” to invoke a Clipboard Paste.

To be bound to:

• `clipboard-yank'
• `x-clipboard-yank'
• `yank'")

;;----------------------------------------------;;

(defvar sboo-key/jump (kbd "<Scroll_Lock>")

  "“Universal Key” to invoke a Jump Command.

To be bound to:

• `sboo-jump'
• `xref-find-definitions'")

;;----------------------------------------------;;

(defvar sboo-key/compile (kbd "<kp-delete>")

  "“Universal Key” to invoke `sboo-compile'.

To be bound to:

• `sboo-compile'
• `compile'
• `recompile'

In some `major-mode's, this key can be bound to a similar (but
different) command. For example, `recompile' in
`compilation-mode-map'.")

;;----------------------------------------------;;
;; Autoloads -----------------------------------;;
;;----------------------------------------------;;

(autoload 'zap-up-to-char "misc"
  "Kill up to (but not including) the ARG-th occurrence of CHAR.")

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(progn

  ;;--------------------------;;

  (unless (fboundp #'forward-defun)
    (defun forward-defun (&optional count)
      (interactive "P")
      (let* ((COUNT (or count +1)))
        (forward-thing 'defun COUNT))))

  (unless (fboundp #'backward-defun)
    (defun backward-defun (&optional count)
      (interactive "P")
      (let* ((COUNT (* -1 (or count +1))))
        (forward-thing 'defun COUNT))))

  ;;--------------------------;;

  (defvar-local sboo-forward-defun-function #'forward-defun)

  (defvar-local sboo-backward-defun-function #'backward-defun)

  ;;--------------------------;;

  (defun sboo-forward-defun ()

    "Invokes `sboo-forward-defun-function'."

    (interactive)

    (call-interactively sboo-forward-defun-function))

  (defun sboo-backward-defun ()

    "Invokes `sboo-backward-defun-function'."

    (interactive)

    (call-interactively sboo-backward-defun-function))

  ;;--------------------------;;

  ())

;;==============================================;;

(cl-defun sboo-prior-buffer (&optional count &key predicate)

  "Switch back to the COUNT-nth prior buffer.

Inputs:

• COUNT — an optional `integerp'.
  Defaults to +1.

• PREDICATE — an optional `functionp' from `bufferp' to `booleanp'.
  Defaults to #‘sboo-user-buffer-p’.

When called interactively:

• COUNT is the (Raw) Prefix Argument.

Effects:

• Changes the ‘current-buffer’.
• Preserves the ‘selected-window’.

Related:

• Calls ‘previous-buffer’."

  (interactive "P")

  (let* ((PREDICATE (or predicate #'sboo-user-buffer-p))
         )

    (previous-buffer)

    (while (not (funcall PREDICATE (current-buffer)))
      (previous-buffer))))

;;----------------------------------------------;;

(cl-defun sboo-next-buffer (&optional count &key predicate)

  "Switch to the COUNT-nth next buffer.

Inputs: same as `sboo-prior-buffer'.

Related:

• Calls ‘next-buffer’."

  (interactive "P")

  (let* ((PREDICATE (or predicate #'sboo-user-buffer-p))
         )

    (next-buffer)

    (while (not (funcall PREDICATE (current-buffer)))
      (next-buffer))))

;; (sboo-next-buffer)
;; (sboo-previous-buffer)

;;----------------------------------------------;;

(defun sboo-user-buffer-p (&optional buffer)

  "Whether the user wants to see the BUFFER.

Inputs:

• BUFFER — an optional `bufferp'.
  Defaults to `current-buffer'."

  (let* ((BUFFER (or buffer (current-buffer))))

    (and (buffer-live-p buffer)
         (buffer-file-name buffer))))

;;==============================================;;

(cl-defun sboo-last-buffer (&optional count &key predicate)

  "Switch back to the COUNT-nth prior buffer.

Inputs:

• COUNT — an optional `integerp'.
  Defaults to +1.

• PREDICATE — an optional `functionp' from `bufferp' to `booleanp'.
  Defaults to #‘buffer-file-name’.

When called interactively:

• COUNT is the (Raw) Prefix Argument.

Effects:

• Switches the ‘current-buffer’."

  (interactive "P")

  (let* ((COUNT     (prefix-numeric-value (or count +1)))
         (PREDICATE (or predicate #'buffer-file-name))
         )

    (let* ((INDEX         (min 0 (- COUNT 1)))
           (ALL-BUFFERS   (buffer-list))
           (OTHER-BUFFERS (cdr ALL-BUFFERS))
           (LIVE-BUFFERS  (seq-filter #'buffer-live-p OTHER-BUFFERS))
           (GOOD-BUFFERS  (seq-filter PREDICATE LIVE-BUFFERS))
           (PRIOR-BUFFER  (seq-elt GOOD-BUFFERS INDEX))
           )

      (switch-to-buffer PRIOR-BUFFER nil :force-same-window))))

;;==============================================;;

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

   (t nil))) ;sboo-yas-insert-snippet

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

  ;; accidental `kill-emacs' is frustrating:

  (global-unset-key (kbd "C-x c"))

  ;; `suspend-frame' is annoying:

  (global-unset-key (kbd "C-x z"))
  (global-unset-key (kbd "C-M-z"))
  (global-unset-key (kbd "C-x C-z"))

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

;;; Named Keybindings (‘sboo-key/*’):

(progn

  (global-set-key sboo-key/compile        #'compile)
  (global-set-key sboo-key/jump           #'sboo-jump)
  (global-set-key sboo-key/keyboard-quit  #'keyboard-quit)

  ())

;;==============================================;;

;;; Single-Character Keybindings (‘TAB’, ‘RET’, ‘<XF86*>’, etc)...

;;==============================================;;

(global-set-key (kbd "TAB") #'dabbrev-expand) ; Shadows `indent-according-to-mode'.
;(global-set-key (kbd "TAB") #'dabbrev-completion)

(global-set-key (kbd "<backtab>") #'dabbrev-completion)

;; ^ `<backtab>' is (translated from?) `<S-TAB>'.

;;==============================================;;

(when (fboundp #'sboo-find-uri-at-point)
  (global-set-key (kbd "S-<return>") #'sboo-find-uri-at-point))

;; ^ 

;;==============================================;;

(progn

  (global-set-key (kbd "<prior>") #'sboo-backward-defun) ; a.k.a. « Page Up ». 
  (global-set-key (kbd "<next>")  #'sboo-forward-defun)  ; a.k.a. « Page Down ». 

  ;;(global-set-key (kbd "<delete>") #'sboo-undefined)
  ())

;;----------------------------------------------;;

(progn

  (global-set-key sboo-key/copy
                  (cond
                    ((fboundp #'copy-dwim)                 #'copy-dwim)
                    ((fboundp #'clipboard-kill-ring-save) #'clipboard-kill-ring-save)
                    (_                                        #'yank)))

  (global-set-key sboo-key/paste #'clipboard-yank)

  ())

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

;;(global-set-key (kbd "<f9>")  #')                           ; Reserved for ‘helm-command-prefix-key’.
;;(global-set-key (kbd "<f10>") #')                          ; Reserved for ‘keyboard-quit’.
(global-set-key (kbd "<f11>")           #'pp-eval-expression)       ;
(global-set-key (kbd "<f12>")           #'execute-extended-command) ;

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

  (global-set-key (kbd "C-#") #'comment-dwim) ; Mnemonic: “#” is a ubiquitous comment delimiter.
  (global-set-key (kbd "C-3") #'comment-dwim) ; Mnemonic: “<S-3>” and “<#>” are the same key.

  ())

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
  (global-set-key (kbd "C-o") #'other-window)
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

  (global-set-key (kbd "C-;") #'comment-dwim)
  (global-set-key (kbd "C-:") #'comment-dwim)

  ())

(progn
  (global-set-key (kbd "<C-prior>") #'sboo-page-backward) ; Overrides: `scroll-right'.
  (global-set-key (kbd "<C-next>")  #'sboo-page-forward)) ; Overrides: `scroll-left'.

(progn

  (global-set-key (kbd "C-<tab>")         #'sboo-next-buffer)
  (global-set-key (kbd "S-C-<tab>")       #'sboo-prior-buffer)
  (global-set-key (kbd "<C-iso-lefttab>") #'sboo-prior-buffer)

  (eval-when-compile
    (ignore-errors
      (bind-key* "C-<tab>"         #'sboo-next-buffer)
      (bind-key* "S-C-<tab>"       #'sboo-prior-buffer)
      (bind-key* "<C-iso-lefttab>" #'sboo-prior-buffer)))

  ())

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

(progn
  (global-set-key (kbd "<kp-left>")  #'previous-buffer)
  (global-set-key (kbd "<kp-right>") #'next-buffer))

(progn
  (global-set-key (kbd "<kp-up>")    #'sboo-prior-definition)
  (global-set-key (kbd "<kp-down>")  #'sboo-next-definition))

(progn
  (global-set-key (kbd "<kp-home>")  #'set-mark-command)
  (global-set-key (kbd "<kp-end>")   #'delete-other-windows))

(progn
  (global-set-key (kbd "<kp-prior>")    #'flycheck-previous-error)
  (global-set-key (kbd "<kp-next>")     #'flycheck-next-error))

(progn
  (global-set-key (kbd "<kp-begin>") #'sboo-find))

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

;;----------------------------------------------;;

(progn

  ;;--------------------------;;

  (define-key key-translation-map (kbd "s-M-a") (kbd "α"))
  ;; (define-key key-translation-map (kbd "s-M-b") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-c") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-d") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-e") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-f") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-g") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-h") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-i") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-j") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-k") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-l") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-m") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-n") (kbd ""))
  (define-key key-translation-map (kbd "s-M-o") (kbd "•"))
  ;; (define-key key-translation-map (kbd "s-M-p") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-q") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-r") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-s") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-t") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-u") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-v") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-w") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-x") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-y") (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-z") (kbd ""))

  ;;--------------------------;;

  ;; (define-key key-translation-map (kbd "s-M-`")         (kbd ""))
  (define-key key-translation-map (kbd "s-M-1")         (kbd "①"))
  (define-key key-translation-map (kbd "s-M-2")         (kbd "②"))
  (define-key key-translation-map (kbd "s-M-3")         (kbd "③"))
  (define-key key-translation-map (kbd "s-M-4")         (kbd "④"))
  (define-key key-translation-map (kbd "s-M-5")         (kbd "⑤"))
  (define-key key-translation-map (kbd "s-M-6")         (kbd "⑥"))
  (define-key key-translation-map (kbd "s-M-7")         (kbd "⑦"))
  (define-key key-translation-map (kbd "s-M-8")         (kbd "⑧"))
  (define-key key-translation-map (kbd "s-M-9")         (kbd "⑨"))
  (define-key key-translation-map (kbd "s-M-0")         (kbd "∅"))
  (define-key key-translation-map (kbd "s-M-\-")        (kbd "—"))
  (define-key key-translation-map (kbd "s-M-=")         (kbd "≡"))
  (define-key key-translation-map (kbd "s-M-[")         (kbd "【"))
  ;; (define-key key-translation-map (kbd "s-M-{")         (kbd ""))
  (define-key key-translation-map (kbd "s-M-]")         (kbd "】"))
  ;; (define-key key-translation-map (kbd "s-M-}")         (kbd ""))
  ;; (define-key key-translation-map (kbd "s-M-\\")        (kbd ""))
  (define-key key-translation-map (kbd "s-M-|")         (kbd "▮"))
  (define-key key-translation-map (kbd "s-M-'")         (kbd "”"))
  (define-key key-translation-map (kbd "s-M-\"")        (kbd "“"))
  (define-key key-translation-map (kbd "s-M-,")         nil)            ; (See `sboo-insert-angle-quote-left' below).
  (define-key key-translation-map (kbd "s-M-.")         nil)            ; (See `sboo-insert-angle-quote-right' below).
  ;; (define-key key-translation-map (kbd "s-M-/")         (kbd ""))
  (define-key key-translation-map (kbd "s-M-<up>")      (kbd "↑"))
  (define-key key-translation-map (kbd "s-M-<down>")    (kbd "↓"))
  (define-key key-translation-map (kbd "s-M-<left>")    (kbd "←"))
  (define-key key-translation-map (kbd "s-M-<right>")   (kbd "→"))

  ;;--------------------------;;

  (define-key key-translation-map (kbd "s-M-S-<right>") (kbd "↪"))

;; (define-key key-translation-map (kbd "s-M-")         (kbd ""))

  ;;--------------------------;;

  (define-key global-map          (kbd "s-M-,")         #'sboo-insert-angle-quote-left)
  (define-key global-map          (kbd "s-M-.")         #'sboo-insert-angle-quote-right)

  ())

;; (define-key key-translation-map (kbd "s-M-[")         (kbd "【"))
;; (define-key key-translation-map (kbd "s-M-]")         (kbd "】"))
;; (define-key key-translation-map (kbd "s-M-[")         (kbd "⌞"))
;; (define-key key-translation-map (kbd "s-M-]")         (kbd "⌝"))

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

(progn

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
  (global-set-key (kbd "s-(") #'sboo-backward-defun)
  (global-set-key (kbd "s-)") #'sboo-forward-defun)

  ())

;;==============================================;;
;; « s-<LETTER> »

(require 'sboo-keymaps nil :no-error)

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
  (global-set-key (kbd "s-m") #'compile)                      ; co[M]pile.
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
;;(global-set-key (kbd "s-m") #'sboo-mode-keymap)           ; [M]ode-specific commands.

;;==============================================;;
;; « s-<SYMBOL> »

(progn

  (global-set-key (kbd "s-<up>")
                  (or (and (fboundp #'sboo-backward-defun) #'sboo-backward-defun)
                      (and (fboundp #'backward-defun)      #'backward-defun)
                      #'backward-paragraph))

  (global-set-key (kbd "s-<down>")
                  (or (and (fboundp #'sboo-forward-defun) #'sboo-forward-defun)
                      (and (fboundp #'forward-defun)      #'forward-defun)
                      #'forward-paragraph))

  ;;(global-set-key (kbd "s-`")           #')
  (global-set-key (kbd "s--")           #'text-scale-decrease)
  (global-set-key (kbd "s-=")           #'text-scale-increase)
  ;;(global-set-key (kbd "s-<backspace>") #')
  ;;(global-set-key (kbd "s-TAB")         #'dabbrev-completion)

  (global-set-key (kbd "s-[")     ; Mnemonic: « C-x [ » is the default keybinding.
                  (if (fboundp #'sboo-backward-page-or-header)
                      #'sboo-backward-page-or-header
                    #'backward-page))

  (global-set-key (kbd "s-]")     ; Mnemonic: « C-x ] » is the default keybinding.
                  (if (fboundp #'sboo-forward-page-or-header)
                      #'sboo-forward-page-or-header
                    #'forward-page))

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

  ())

;;----------------------------------------------;;

;;; Management/Navigation for Buffers/Windows/Frames

;; (global-set-key (kbd "s-o") #'other-window)       ;TODO;
;; (global-set-key (kbd "s-s") #'sboo-launch-shell)
;; (global-set-key (kbd "s-t") #'sboo-launch-term)
;; (global-set-key (kbd "s-h") #'sboo-split-window-left-right)

;;----------------------------------------------;;

;; (Old:)
;;
;; (global-set-key (kbd "s-[")           #'backward-page) ; Mnemonic: « C-x [ » is the default keybinding.
;; (global-set-key (kbd "s-]")           #'forward-page)  ; Mnemonic: « C-x ] » is the default keybinding.

;;----------------------------------------------;;

;;; Inserting Unicode characters

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
;;; Toolbar “Key”-bindings...
;;==============================================;;

(with-demoted-errors "[sboo-init-keybindings] %s"
;;(tool-bar-enable-clipboard)
  ())

;;==============================================;;
;;; Menubar “Key”-bindings...
;;==============================================;;

(with-demoted-errors "[sboo-init-keybindings] %s"
  (menu-bar-enable-clipboard)
  ())

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

  (dolist (*MAP* (list ido-file-completion-map ido-file-dir-completion-map))

    (define-key *MAP* (kbd "<kp-left>")  #'backward-char)
    (define-key *MAP* (kbd "<kp-right>") #'forward-char)))

;;==============================================;;

(progn

  (defun sboo-set-run-key-to-eval-buffer ()
    "Bind `sboo-key/compile' (&al) to `eval-buffer'."
    (let ((COMMAND #'eval-buffer)
          (KEYS    (list sboo-key/compile (kbd "s-m"))))
      (dolist (KEY KEYS)
        (local-set-key KEY COMMAND))
      `(,COMMAND . ,KEYS)))

  (add-hook 'emacs-lisp-mode-hook #'sboo-set-run-key-to-eval-buffer))

;;==============================================;;

;;; ‹compilation-[minor-]mode-map›s:

(when (require 'compile nil t)

  (let ((*MAP* compilation-mode-map))

    (define-key *MAP* sboo-key/compile #'recompile)

    ())

  (let ((*MAP* compilation-minor-mode-map))

    (define-key *MAP* (kbd "<kp-next>")  #'compilation-next-error)
    (define-key *MAP* (kbd "<kp-prior>") #'compilation-previous-error)

    ;; (define-key *MAP* (kbd "<kp-down>")  #'compilation-next-file)
    ;; (define-key *MAP* (kbd "<kp-up>")    #'compilation-previous-file)

    ())

  'compile)

;; ^ Notes:
;;
;; • `compilation-minor-mode-map' is a parent of `compilation-mode-map'.
;; • 
;;

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

;;; sboo-init-keybindings.el ends here