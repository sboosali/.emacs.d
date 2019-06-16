;;; sboo-commands.el --- Personal ‘commandp’s -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 13 Jun 2019
;; License: GPL-3.0-or-later

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

;;; Commentary:

;; Personal ‘interactive’ ‘commandp’s.
;;
;; (This `feature' is "pure" (it has declarations only, no settings)).
;;
;; Most commands are namespaced under `sboo-` or `xah-`
;; (as are their non-`interactive' utilities).
;;
;; See my `define-graceful-command` macro, for conveniently defining commands with fallbacks
;; (when external packages haven't been installed and/or can't be loaded).
;;
;;

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(eval-when-compile
  (require 'rx)
  (require 'pcase))

;;----------------------------------------------;;

(progn
  (require 'shell)
  (require 'seq)
  (require 'cl-lib))

;;==============================================;;

;; personal:

(progn
  (require 'sboo-conditions)
  (require 'sboo-utilities))

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

(eval-when-compile

  (defmacro defun-dwim (name active inactive &optional docstring)

    "Declare a command named NAME, which calls ACTIVE when `use-region-p' and INACTIVE otherwise.

Inputs:

• NAME — (unquoted) `symbolp'.
  the function name.
• ACTIVE — (unquoted) `symbolp'.
  its `symbol-value' should be a `commandp'.
• INACTIVE — (unquoted) `symbolp'.
  its `symbol-value' should be a `commandp'.
• DOCSTRING — a `stringp'.
  the function documentation.

Output:

• a `defun' declaration.

Example:

• M-: (pp-macroexpand-expression (quote (defun-dwim eval-dwim eval-region eval-last-sexp \"`eval-region' or `eval-last-sexp'.\")))
    ⇒ (defun eval-dwim ()
    ⇒   \"`eval-region' or `eval-last-sexp'.\"
    ⇒   (interactive)
    ⇒   (if (use-region-p)
    ⇒       (call-interactively #'eval-region)
    ⇒     (call-interactively #'eval-last-sexp)))

Links:

• URL `https://www.emacswiki.org/emacs/DoWhatIMean'

Notes:

• “DWIM” abbreviates “Do-What-I-Mean”."

    (declare (debug (&define name name name &optional stringp))
             (doc-string 4)
             (indent     4))

    (let* ((NAME     name)
           (ACTIVE   active)
           (INACTIVE inactive)

           (DOCSTRING (or docstring
                          (format "Call `%s' when a region is active, otherwise call `%s'." active inactive)))
           )

      `(defun ,NAME ()
         ,DOCSTRING
         (interactive)
         (if (use-region-p)
             (call-interactively (function ,ACTIVE))
           (call-interactively (function ,INACTIVE)))))))

;; e.g. `defun-dwim':
;;
;; M-: (pp-macroexpand-expression '(defun-dwim eval-dwim eval-region eval-last-sexp "DWIM: `eval-region' or `eval-last-sexp'."))
;;
;;   ⇒ (defun eval-dwim ()
;;   ⇒   "DWIM: `eval-region' or `eval-last-sexp'."
;;   ⇒   (interactive)
;;   ⇒   (if (use-region-p)
;;   ⇒       (call-interactively #'eval-region)
;;   ⇒     (call-interactively #'eval-last-sexp)))
;;
;; M-: (format "DWIM: call `%s' when `use-region-p', otherwise call `%s'." #'eval-region #'eval-last-sexp)
;;   ⇒ "DWIM: call `eval-region' when `use-region-p', otherwise call `eval-last-sexp'."
;;
;;

;; ^ NOTES
;;
;; • `use-region-p':
;;
;;   Return `t' if ① the region is active and ② it is appropriate to act on it.
;;
;; • « (declare (debug (...)) ...) »:
;;
;;   • "(debug (&define ...))" — a `defun'-like form.
;;   • "(debug (... symbolp ...))" — an unquoted(?) `symbolp'.
;;

;;----------------------------------------------;;

(defmacro define-graceful-command (Name ExternalCommand BuiltinCommand &optional DocString)

  `(defun ,Name ()

     ,DocString

     (interactive) ;;TODO must expose, e.g. for helm-find-files, (interactive "P") ;; use cl-defun for :doc and :interactive

     (let ((*command* (function ,ExternalCommand)))

    (if (and (commandp *command*)
             (fboundp  *command*))

      (call-interactively *command*)

     (call-interactively (function ,BuiltinCommand))))))

;; ^ `defalias' for commands with graceful degradation.
;;
;; Wraps `defun' and `call-interactively'.
;;
;; NOTE (function f) is like #'f
;;
;; NOTE the predicates succeed even when command is marked with `autoload'.
;;

;;----------------------------------------------;;
;; DWIM ----------------------------------------;;
;;----------------------------------------------;;

(with-demoted-errors "[Warning] %s" (defun-dwim eval-dwim   eval-region   eval-last-sexp))
(with-demoted-errors "[Warning] %s" (defun-dwim kill-dwim   kill-region   kill-line))
(with-demoted-errors "[Warning] %s" (defun-dwim fill-dwim   fill-region   fill-paragraph))
(with-demoted-errors "[Warning] %s" (defun-dwim indent-dwim indent-region sboo-indent-defun-or-buffer))

;;----------------------------------------------;;
;; Thing at point ------------------------------;;
;;----------------------------------------------;;

(defun sboo-indent-defun-or-buffer ()

  "Indent the `defun' currently-surrounding `point', or the `current-buffer'."

  (interactive)

  (let* ((BOUNDS (bounds-of-thing-at-point 'defun))
         )

    (if BOUNDS
        (indent-region (car BOUNDS) (cdr BOUNDS))
      (indent-region (point-min) (point-max)))))

;;----------------------------------------------;;

(defun sboo-kill-thing-at-point (thing)

  "Kill the `thing-at-point' (for the given kind of THING).

Inputs:

• THING — a `symbolp'.

Effects:

• CLipboard — modify the `kill-ring'."

  (interactive (list
                (sboo-read-thing :prompt "Thing")
                ))

  (cl-check-type thing symbol "a “thing”, a `symbolp'.")

  (let* ((BOUNDS (bounds-of-thing-at-point thing))
         )

    (if BOUNDS
        (kill-region (car BOUNDS) (cdr BOUNDS))
      (error "No « %s » at point" thing))))

;;----------------------------------------------;;
;; Insertion -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-insert-open-parenthesis ()

  "Insert « \"(\" »."

  (interactive)

  (insert "("))

(defun sboo-insert-close-parenthesis ()

  "Insert « \")\" »."

  (interactive)

  (insert ")"))

;;----------------------------------------------;;

(defun sboo-insert-open-square-bracket ()

  "Insert « \"[\" »."

  (interactive)

  (insert "["))

(defun sboo-insert-close-square-bracket ()

  "Insert « \"]\" »."

  (interactive)

  (insert "]"))

;;----------------------------------------------;;

(defun sboo-insert-open-curly-brace ()

  "Insert « \"}\" »."

  (interactive)

  (insert "{"))

(defun sboo-insert-close-curly-brace ()

  "Insert « \"}\" »."

  (interactive)

  (insert "}"))

;;----------------------------------------------;;
;; Cycle Through (& Toggle Between) User Buffers
;;----------------------------------------------;;

;; Problems with `next-buffer' / `previous-buffer':
;;
;; (1) you cannot hold down the key for the command to repeat.
;;
;; (2) they will go thru many buffers user are not interested in cycling thru.
;;

;; See:
;;     - http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html
;;

;;----------------------------------------------;;

(defun xah-next-user-buffer ()

  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"

  (interactive)

  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

;;----------------------------------------------;;

(defun xah-prior-user-buffer ()

  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"

  (interactive)

  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

;;----------------------------------------------;;

(defun xah-user-buffer-q ()

  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"

  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

;;----------------------------------------------;;

(defun sboo-toggle-buffer ()

  "Switch to the previously open buffer.

  Repeated invocations **toggle** between the two most recently open buffers.
  (c.f. the default behavior of repeated `other-buffer' invocations,
  which **cycle** through all open buffers).

  See http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
  "
  (interactive)

  (switch-to-buffer
    (other-buffer (current-buffer) 1)))


;;----------------------------------------------;;
;; Window Management,
;;----------------------------------------------;;

(defun sboo-split-window-left-right ();;TODO

  ""

  (interactive)

  (progn
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (switch-to-next-buffer)
    (other-window 1)
    ()))

;; ^
;;

;;----------------------------------------------;;
;; Launching -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-find-uri-at-point ()

  "Open the file or uri at `point'."

  (interactive)

  (let* ((URI  (ffap-url-at-point))
         (FILE (ffap-file-at-point))
         )

    (cond

      (URI  (progn
              (browse-url URI)
              URI))

      (FILE  (progn
              (find-file FILE)
              FILE))

      (t nil))))

;;----------------------------------------------;;

(defun sboo-clean-uri (uri)

  "Strip specific leading/trailing characters from URI.

Examples:

• M-: (sboo-clean-uri \"`https://www.gnu.org/software/emacs/manual/html_node/elisp/String-Basics.html'\")
    ↪ \"https://www.gnu.org/software/emacs/manual/html_node/elisp/String-Basics.html\""

  (when-let* ((URI uri)
              (TRIMMED (string-trim URI "[<`‘“ \t\n\r]+" "[>'’” \t\n\r]+"))
              )

    TRIMMED))

;; M-: (sboo-clean-uri "`https://www.gnu.org/software/emacs/manual/html_node/elisp/String-Basics.html'")
;;   ↪ "https://www.gnu.org/software/emacs/manual/html_node/elisp/String-Basics.html""

;;----------------------------------------------;;
;; Files
;;----------------------------------------------;;

;;----------------------------------------------;;

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((-path (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2)))))
    (if (string-match-p "\\`https?://" -path)
        (browse-url -path)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" -path)
            (progn
              (let (
                    (-fpath (match-string 1 -path))
                    (-line-num (string-to-number (match-string 2 -path))))
                (if (file-exists-p -fpath)
                    (progn
                      (find-file -fpath)
                      (goto-char 1)
                      (forward-line (1- -line-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" -fpath))
                      (find-file -fpath))))))
          (progn
            (if (file-exists-p -path)
                (find-file -path)
              (if (file-exists-p (concat -path ".el"))
                  (find-file (concat -path ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" -path))
                  (find-file -path ))))))))))

;; ^ this command will:
;; - open the file path under cursor, without confirmation;
;; - and jump to line number, if the path ends with ":<number>".

;;----------------------------------------------;;
;; Shell / Terminal.
;;----------------------------------------------;;

(defun sboo-launch-shell ();;TODO

  "Switch to (or create) a`shell-mode` buffer."

  (interactive)

  (let*
      ((NAME "*shell*"))
    (if (bufferp (get-buffer NAME))
        (switch-to-buffer (get-buffer NAME) nil 'force-same-window)
      (shell NAME))))

  ;; ^
  ;;
  ;; `shell`
  ;; '''Run an inferior shell, with I/O through BUFFER (which defaults to ‘*shell*’).
  ;; If BUFFER exists but shell process is not running, make new shell.
  ;; If BUFFER exists and shell process is running, just switch to BUFFER.'''
  ;;
  ;; `switch-to-buffer`
  ;; (switch-to-buffer BUFFER-OR-NAME &optional NORECORD FORCE-SAME-WINDOW)
  ;; '''Display buffer BUFFER-OR-NAME in the selected window.'''
  ;; NOTE `shell` doesn't have an option for the `'force-same-window` behavior.
  ;;

;;----------------------------------------------;;

(defun sboo-launch-term ()

  "Switch to (or create) a `term-mode` buffer."

  (interactive)

  (progn

    (term "/bin/bash") ;;TODO still prompts

    ()))

;;----------------------------------------------;;
;; Filesystem Navigation -----------------------;;
;;----------------------------------------------;;

(defun sboo-find-file ()

  "Wraps `ffap' (a.k.a. “find file at point”).

TODO handle “dir/dir/file.ext:line:column”."

  (interactive)

  (if (commandp #'ffap)

      (call-interactively #'ffap)

    (call-interactively #'find-file)))

;;----------------------------------------------;;
;; Text Motion ---------------------------------;;
;;----------------------------------------------;;

(defcustom sboo-page-regexp

  (rx-to-string `(or (regexp ,page-delimiter)))

  "Matches a “Form Feed” page \(i.e. «  »\).

a `regexpp'."

  :type 'regexp

  :safe #'stringp
  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-page-or-elisp-header-regexp

  (rx-to-string `(or (regexp ,page-delimiter)
                     (and bol ";;;" blank (char alnum))))

  "Matches a “Form Feed” page \(i.e. «  »\) or comment header \(e.g. « ;;; Code: »\).

a `regexpp'."

  :type 'regexp

  :safe #'stringp
  :group 'sboo)

;;==============================================;;

;;;###autoload
(cl-defun sboo-forward-page-or-header (&optional (count 1))

  "Move forward COUNT “Form Feed” pages \(i.e. «  »\) and/or comment headers \(e.g. « ;;; Code: »\).

Inputs:

• COUNT — an `integerp'.

Effects:

• Move `point' — to next `sboo-page-or-header-regexp'.

Related:

• ‘page-delimiter’
• ‘outline-regexp’"

  (interactive "P")

  (let* ((COUNT  (or count +1))
         (REGEXP (sboo-page-or-header-regexp))
         )
    (progn

      (re-search-forward REGEXP nil :no-error COUNT)
      (end-of-line)
      (recenter 0))))

;; Notes:
;;
;; • ‘re-search-forward’ — for NOERROR argument, « :no-error » ≠ « t ».

;;----------------------------------------------;;

;;;###autoload
(cl-defun sboo-backward-page-or-header (&optional (count 1))

  "Move backward COUNT “Form Feed” pages \(i.e. «  »\) and/or comment headers \(e.g. « ;;; Code: »\).

Inputs:

• COUNT — an `integerp'.

Effects:

• Move `point' — to prior `sboo-page-or-header-regexp'.

Related:

• Negates ‘sboo-forward-page-or-header’"

  (interactive "P")

  (let* ((COUNT  (or count +1))
         (REGEXP (sboo-page-or-header-regexp))
         )
    (progn

      (beginning-of-line)
      (re-search-backward REGEXP nil :no-error COUNT)
      (unless (bobp) (end-of-line))
      (recenter 0))))

;;==============================================;;

(defun sboo-page-or-header-regexp ()

  "Accessor for `sboo-page-or-header-regexp'."

  (let* ()

    (if (parent-mode-is-derived-p major-mode 'emacs-lisp-mode)
        sboo-page-or-header-regexp
      sboo-page-regexp)))

;;----------------------------------------------;;
;; Projectile.
;;----------------------------------------------;;

(defun sboo-projectile-find-file ()
  "`projectile-find-file'."
  (interactive)
  (projectile-find-file t))

;;----------------------------------------------;;

;; (defun sboo-projectile-grep ()
;;   (interactive)
;;   (projectile-grep))

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

(defun sboo-kmacro-insert-counter-letter ()

  "Insert a,b,c,… when `kmacro-counter' is 0,1,2,…."

  (interactive)

  (let* ((CHAR   (+ ?a kmacro-counter))
         (STRING (make-string 1 CHAR))
         )
    
    (insert STRING)
    (kmacro-add-counter +1)))

;;----------------------------------------------;;
;; Unicode insertion.
;;----------------------------------------------;;

(defun sboo-insert-angle-quote-left ()

  "`insert' \"«\", the \"LEFT-POINTING DOUBLE ANGLE QUOTATION MARK\" Unicode character,
  with spacing."
  (interactive)

  (insert "« "))

;;----------------------------------------------;;

(defun sboo-insert-angle-quote-right ()

  "`insert' \"»\", the \"RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK\" Unicode character,
  with spacing."
  (interactive)

  (insert " »"))

;;----------------------------------------------;;

(defun sboo-insert-triple-equals-sign ()

  "`insert' \"≡ \", the \"IDENTICAL TO\" Unicode character,
  with spacing."
  (interactive)

  (insert "≡ "))

;;----------------------------------------------;;

(defun sboo-insert-bullet ()

  "`insert' \"•\", the \"BULLET\" Unicode character."
  (interactive)

  (insert "•"))

;;----------------------------------------------;;

(defun sboo-insert-black-circle ()

  "`insert' \"●\", the \"BLACK CIRCLE\" Unicode character."
  (interactive)

  (insert "●"))

;;----------------------------------------------;;

(defun sboo-insert-dash ()

  "`insert' \"—\", the \"EM DASH\" Unicode character."
  (interactive)

  (insert "—"))

;;----------------------------------------------;;

(defun sboo-insert-null ()

  "`insert' \"∅\", the \"EMPTY SET\" Unicode character."
  (interactive)

  (insert "∅"))

;;----------------------------------------------;;

(defun sboo-insert-circled-1 ()
  "`insert' \"①\", the \"CIRCLED DIGIT ONE\" Unicode character."
  (interactive)
  (insert "①"))

;;----------------------------------------------;;

(defun sboo-insert-circled-2 ()
  "`insert' \"②\", the \"CIRCLED DIGIT TWO\" Unicode character."
  (interactive)
  (insert "②"))

;;----------------------------------------------;;

(defun sboo-insert-circled-3 ()
  "`insert' \"③\", the \"CIRCLED DIGIT THREE\" Unicode character."
  (interactive)
  (insert "③"))

;;----------------------------------------------;;

(defun sboo-insert-double-right-arrow ()
  "`insert' \"⇒\", the \"RIGHTWARDS DOUBLE ARROW\" Unicode character."
  (interactive)
  (insert "⇒"))

;;----------------------------------------------;;

(defun sboo-insert-left-double-quotation-mark ()
  "`insert' ?“, the \"LEFT DOUBLE QUOTATION MARK\" Unicode character."
  (interactive)
  (insert-char ?“))

;;----------------------------------------------;;

(defun sboo-insert-right-double-quotation-mark ()
  "`insert' ?”, the \"RIGHT DOUBLE QUOTATION MARK\" Unicode character."
  (interactive)
  (insert-char ?”))

;;----------------------------------------------;;

;; (defun sboo-insert- ()
;;   "`insert' \"\", the \"\" Unicode character."
;;   (interactive)
;;   (insert ""))

;; M-: (string-join (split-string (downcase "RIGHTWARDS DOUBLE ARROW")) "-")
;;     "rightwards-double-arrow"

;; M-: (string-join (split-string (upcase "rightwards-double-arrow") "[-]+" :omit-nulls) " ")
;;     "RIGHTWARDS DOUBLE ARROW"

;;----------------------------------------------;;
;; Commands that Gracefully Degrade ;;;;;;;;;;;;;;
;;----------------------------------------------;;

;(defun sboo-M-x (PrefixArgument)
;
 ;    "Invoke `helm-M-x', if bound."
;
 ;    (interactive "P")
  ;
   ;  (if (commandp #'helm-M-x)
    ;     (helm-M-x PrefixArgument)
     ;    (execute-extended-command)))

;;;(defalias sboo-M-x helm-M-x)
;;;(define-graceful-command sboo-M-x helm-M-x execute-extended-command)

;;  "Try `helm-M-x', fallback to `execute-extended-command'.
;;  (When `helm' isn't loaded/installed, this command falls back
;;  to the standard-library command upon which that package improves.)

;;----------------------------------------------;;

(defun sboo-buffers-list (PrefixArgument)

     "Invoke `helm-buffers-list', if bound."

     (interactive "P")

     (if (commandp #'helm-buffers-list)
         (helm-buffers-list PrefixArgument)
         (ibuffer)))

;;;(defalias sboo-buffers-list helm-buffers-list)
;;;(define-graceful-command sboo-buffers-list helm-buffers-list list-buffers)

;;----------------------------------------------;;

(define-graceful-command sboo-search
          xah-search-current-word
          isearch-forward-regexp)

;; ^
;; i.e. fallback to `isearch-forward'.
;;
;; alternatives:
;;
;; - xah-search-current-word
;; - isearch-forward
;; - isearch-forward-regexp
;;

;;----------------------------------------------;;

(defun sboo-guess-open-command ()

  "Guess an external program to open file with (e.g. `open', `xdg-open')."

  (let ((--command-open     "open")
        (--command-xdg-open "xdg-open"))

    (pcase platform

      ('platform-windnows --command-open)
      ('platform-apple    --command-open)

      ('platform-linux   (let ((--xdg-open (executable-find --command-xdg-open)))
                           ;; ^ if xdg-open is installed
                           (cond
                            (--xdg-open --xdg-open)
                            (t          --command-open))))

      ('platform-unknown --command-open))))

;;           (read-shell-command "Open current file with: "))

;;----------------------------------------------;;

(defun sboo-open-with (&optional FILENAME)

  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)

  (let ((_COMMAND_ (sboo-guess-open-command))

        (_FILENAME_ (or FILENAME buffer-file-name)))

    (when _FILENAME_
      (shell-command (concat
                      _COMMAND_ ;;TODO mk lazy
                      " "
                      _FILENAME_)))))


;;----------------------------------------------;;

(defun sboo-kill-whitespace ()

  "Kill all whitespace between two (non-whitespace) characters.

Related:

• `sboo-kill-whitespace'"

  (interactive "*")

  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))

;;==============================================;;
;; Copying...

;;TODO https://stackoverflow.com/questions/22434484/emacs-modes-highlighting-uses-of-variable-under-point

;;----------------------------------------------;;

(cl-defun sboo-copy-object (object &key format-specifier)

  "Copy OBJECT (to the clipboard), after formatting via FORMAT.

Inputs:

• OBJECT           — any printable object (in particular, `stringp', `integerp', `consp', `vectorp', `hash-table-p', etc).
• FORMAT-SPECIFIER — a `stringp'. a Format-Specifier.

Output:

• a `stringp'. The

Example:

• M-: (sboo-copy-object )
    ⇒ "

  (interactive)

  (when-let* ((FORMAT (or format-specifier
                          "%s"))
              (OBJECT (if t object nil))
              (TEXT   (format FORMAT OBJECT))
              )

    (sboo-copy-string TEXT)
    (message TEXT)
    TEXT))

;; e.g.
;;
;; M-: (sboo-copy-object "xyz" :format-specifier "%s")
;;   ; xyz
;;   ⇒ "xyz"
;;
;; M-: (sboo-copy-object "xyz" :format-specifier "%S")
;;   ; "xyz"
;;   ⇒ "\"xyz\""
;;
;; M-: (sboo-copy-object ?a :format-specifier "%d")
;;   ; 97
;;   ⇒ "97"
;;
;; M-: (sboo-copy-object ?a :format-specifier "%c")
;;   ; a
;;   ⇒ "a"
;;

;; ^ `when-let*':
;;
;;   (when-let SPEC &rest BODY)
;;
;; >Bind variables according to SPEC and conditionally eval BODY.
;; >Each binding is evaluated in turn, and evaluation stops if a
;; >binding value is nil.  If all are non-nil, the value of the last
;; >form in BODY is returned.
;;
;;

;;----------------------------------------------;;

(defun sboo-copy-string (text)

  "Copy TEXT (to the clipboard).

Inputs:

• TEXT — a `stringp' or `characterp'."

  (interactive)

  (let* ((TEXT (pcase text

                 ((pred stringp)    text)
                 ((pred characterp) (string text))

                 (_ nil)))
         )

    (when (and (stringp text) (not (string-empty-p text)))

      (with-temp-buffer

        (insert TEXT)
        (sboo-copy-buffer-contents)
        TEXT))))

;; `kill-ring-save'

;;----------------------------------------------;;

(defun sboo-copy-buffer-contents ()

  "Copy the whole buffer (to the clipboard)."

  (interactive)

  (kill-ring-save (point-min) (point-max)))

;; `kill-ring-save'
;; `buffer-substring-no-properties'

;;==============================================;;

(defun sboo-read-coding-system ()

  "Relevant completion for (known, good) Coding-Systems.

Output:

• a `coding-system-p'."

  (let* ((CHOICES (sboo-list-coding-systems))
         )

    (completing-read "Coding-System: " CHIOCES)))

;;----------------------------------------------;;

(defun sboo-list-coding-systems ()

  "Relevant completion for (known, good) Coding-Systems.

Output:

• a `coding-system-p'."

  (let* ((CODING-SYSTEMS (sort-coding-systems (coding-system-list 'base-only)))
         )

    CODING-SYSTEMS))

;;----------------------------------------------;;

(defun sboo-set-coding-system (coding-system)

  "`set-buffer-file-coding-system' to CODING-SYSTEM."

  (interactive (list
                (sboo-read-coding-system)))

  (set-buffer-file-coding-system coding-system))

;;----------------------------------------------;;

(defun sboo-get-coding-system ()

  "Return (and print) the current Coding-System.

Output:

• a `coding-system-p'."

  (let* ((CODING-SYSTEM )
         )

    (when (called-interactively-p)
      (message "%S" CODING-SYSTEM))

    CODING-SYSTEM))

;;----------------------------------------------;;

(defun sboo-get-coding-system-info (&optional coding-system)

  "Information about CODING-SYSTEM.

Input:

• a `coding-system-p'.
  Defaults to the `current-buffer's coding system, prompting for completion.

Output:

• a plist.

Examples:

• M-: (sboo-get-coding-system-info 'utf-8)
    ⇒ '(:system utf-8 :type utf-8 :charsets (unicode))

Related:

• `describe-coding-system'"

  (interactive "zCoding System: ")
  ;(interactive (list (sboo-read-coding-system)))

  (let* ((CODING-SYSTEM   (or coding-system buffer-file-coding-system))
         (CODING-TYPE     (coding-system-type         CODING-SYSTEM))
         (CODING-CHARSETS (coding-system-charset-list CODING-SYSTEM))

         (PLIST `(:system ,CODING-SYSTEM :type ,CODING-TYPE :charsets ,CODING-CHARSETS))
        )

    (when (called-interactively-p)
      (message "%S" PLIST))

    PLIST))

;;==============================================;;

(defun sboo-set-input-method-TeX ()

  "Set the `input-method' to « TeX ».

e.g. with `M-x set-input-method RET TeX RET`, typing `\xi` inputs `ξ`."

  (interactive)

  (set-input-method "TeX" t))

;;----------------------------------------------;;

(defun sboo-reset-input-method ()

  "Reset the `input-method' (to the default).

Note:

• `current-input-method' is nil (originally).
• `default-input-method' is \"TeX\".
"

  (interactive)

  (set-input-method nil t))

;;==============================================;;
;; TODO widgets...

;; M-: (widget-choose "Browser" '(("Firefox" . firefox) ("Google Chrome" . google-chrome)))
;;

(with-demoted-errors "[Warning] %s"

;;(widget-checklist-add-item)

  ())

;;==============================================;;

(defun sboo-custom-groups-p (symbol)

  "Get all Customization Groups.

Output:

• a `listp' of `symbolp's.
  Everything defined by `defgroup'.
  An average `length' is 75.

Notes:

• `defgroup' sets the `custom-group' property of the `symbolp')."

  (let* ((GROUPS )
         )

    (mapatoms (lambda (*SYMBOL*)
                (let ((GROUP (get *SYMBOL* 'custom-group))
                      )
                  (when GROUP
                    (push *SYMBOL* GROUPS)))))
    GROUPS))

;;----------------------------------------------;;

(defun sboo-commands-list ()

  "Get all `commandp's (Interactive Commands).

Output:

• a `listp' of `symbolp's.
  Everything defined by `defun' with `interactive'.
  An average `length' is 5,000."

  (let* ((COMMANDS )
         )

    (mapatoms (lambda (*SYMBOL*)
                (let ((COMMAND? (commandp *SYMBOL*)))
                  (when COMMAND?
                    (push *SYMBOL* COMMANDS)))))
    COMMANDS))

;;----------------------------------------------;;

(defun sboo-modes-list ()

  "Get all modes (both `major-mode's and `minor-mode's).

Output:

• a `listp' of `symbolp's.
  Everything defined by `define-major-mode' and `define-minor-mode'.
  An average `length' is 500."

  (let* ((MODES )
         )

    (mapatoms (lambda (*SYMBOL*)
                (let ((MODE? (or (get *SYMBOL* 'mode-class)
                                 (and (string-suffix-p "-mode" (symbol-name *SYMBOL*))
                                      (commandp *SYMBOL*)))))
                  (when MODE?
                    (push *SYMBOL* MODES)))))
    MODES))

;; ^ Notes:
;;
;; M-: (and (string-suffix-p "-mode" (symbol-name 'haskell-mode)) (commandp 'haskell-mode))
;;   ⇒ t
;;

;;----------------------------------------------;;

(defun sboo-special-modes-list ()

  "Get all `special-mode'\'s.

Output:

• A `Listp' Of `Symbolp'S.
  Every `major-mode' which inherits from `special-mode'.
  An average `length' is 30."

  (let* ((MODES)
         )

    (mapatoms (lambda (*SYMBOL*)
                (let ((MODE? (or (eq 'special (get *SYMBOL* 'mode-class))
                                 (derived-mode-p *SYMBOL* 'special-mode))))
                  (when MODE?
                    (push *SYMBOL* MODES)))))
    MODES))

;; ^ e.g.:
;;
;; M-: (sboo-special-modes-list)
;;   ⇒ '(emacs-lisp-mode rmail-mode Custom-mode wdired-mode magit-mode ibuffer-mode Info-edit-mode magit-process-mode image-mode compilation-mode tabulated-list-mode magit-diff-mode occur-mode edebug-eval-mode comint-mode shell-mode messages-buffer-mode helm-major-mode grep-mode special-mode eww-mode completion-list-mode help-mode debugger-mode dired-mode Info-mode prog-mode bookmark-bmenu-mode edmacro-mode magit-status-mode)
;;

;; ^ Notes:
;;
;; M-: (get 'dired-mode 'mode-class)
;;   ⇒ 'special
;;
;; M-: (provided-mode-derived-p 'dired-mode '(special-mode))
;;   ⇒ nil
;;
;; 

;;----------------------------------------------;;

(defun sboo-custom-groups-list ()

  "Get all `groupp's (Customization Groups).

Output:

• a `listp' of `symbolp's.
  Everything defined by `defgroup'.
  An average `length' is 75.

Notes:

• `defgroup' sets the `custom-group' property of the `symbolp')."

  (let* ((GROUPS )
         )

    (mapatoms (lambda (*SYMBOL*)
                (let ((GROUP (get *SYMBOL* 'custom-group))
                      )
                  (when GROUP
                    (push *SYMBOL* GROUPS)))))
    GROUPS))

;;----------------------------------------------;;

(defun sboo-read-custom-group ()

  "Read a Customization Group (from the user).

Output:

• a `symbolp'.
  a Customization Group (defined via `defgroup').

Notes:

• `defgroup' sets the `custom-group' property of the `symbolp')."

  (interactive)

  (let* ((CANDIDATES (sboo-custom-groups-list))
         (STRING     (completing-read "Customization Group: " CANDIDATES nil 'confirm "sboo-"))
         (SYMBOL     (intern-soft STRING))
         )

    SYMBOL))

;;==============================================;;

(defcustom sboo-spdx-license-alist

  '(

     (0BSD                                  . ("BSD Zero Clause License"                                    t))
     (AAL                                   . ("Attribution Assurance License"                                                nil))
     (Abstyles                              . ("Abstyles License"                                                             nil))
     (Adobe-2006                            . ("Adobe Systems Incorporated Source Code License Agreement"                     nil))
     (Adobe-Glyph                           . ("Adobe Glyph List License"                                                     nil))
     (ADSL                                  . ("Amazon Digital Services License"                                              nil))
     (AFL-1.1                               . ("Academic Free License v1.1"                                 t))
     (AFL-1.2                               . ("Academic Free License v1.2"                                 t))
     (AFL-2.0                               . ("Academic Free License v2.0"                                 t))
     (AFL-2.1                               . ("Academic Free License v2.1"                                 t))
     (AFL-3.0                               . ("Academic Free License v3.0"                                 t))
     (Afmparse                              . ("Afmparse License"                                                             nil))
     (AGPL-1.0-only                         . ("Affero General Public License v1.0 only"                                      nil))
     (AGPL-1.0-or-later                     . ("Affero General Public License v1.0 or later"                                  nil))
     (AGPL-3.0-only                         . ("GNU Affero General Public License v3.0 only"                t))
     (AGPL-3.0-or-later                     . ("GNU Affero General Public License v3.0 or later"            t))
     (Aladdin                               . ("Aladdin Free Public License"                                                  nil))
     (AMDPLPA                               . ("AMD's plpa_map.c License"                                                     nil))
     (AML                                   . ("Apple MIT License"                                                            nil))
     (AMPAS                                 . ("Academy of Motion Picture Arts and Sciences BSD"                              nil))
     (ANTLR-PD                              . ("ANTLR Software Rights Notice"                                                 nil))
     (Apache-1.0                            . ("Apache License 1.0"                                         t))
     (Apache-1.1                            . ("Apache License 1.1"                                         t))
     (Apache-2.0                            . ("Apache License 2.0"                                         t))
     (APAFML                                . ("Adobe Postscript AFM License"                                                 nil))
     (APL-1.0                               . ("Adaptive Public License 1.0"                                                  nil))
     (APSL-1.0                              . ("Apple Public Source License 1.0"                                              nil))
     (APSL-1.1                              . ("Apple Public Source License 1.1"                                              nil))
     (APSL-1.2                              . ("Apple Public Source License 1.2"                                              nil))
     (APSL-2.0                              . ("Apple Public Source License 2.0"                            t))
     (Artistic-1.0-cl8                      . ("Artistic License 1.0 w/clause 8"                                              nil))
     (Artistic-1.0-Perl                     . ("Artistic License 1.0 (Perl)"                                                  nil))
     (Artistic-1.0                          . ("Artistic License 1.0"                                                         nil))
     (Artistic-2.0                          . ("Artistic License 2.0"                                       t))
     (Bahyph                                . ("Bahyph License"                                                               nil))
     (Barr                                  . ("Barr License"                                                                 nil))
     (Beerware                              . ("Beerware License"                                                             nil))
     (BitTorrent-1.0                        . ("BitTorrent Open Source License v1.0"                                          nil))
     (BitTorrent-1.1                        . ("BitTorrent Open Source License v1.1"                        t))
     (Borceux                               . ("Borceux license"                                                              nil))
     (BSD-1-Clause                          . ("BSD 1-Clause License"                                                         nil))
     (BSD-2-Clause-FreeBSD                  . ("BSD 2-Clause FreeBSD License"                               t))
     (BSD-2-Clause-NetBSD                   . ("BSD 2-Clause NetBSD License"                                                  nil))
     (BSD-2-Clause-Patent                   . ("BSD-2-Clause Plus Patent License"                                             nil))
     (implified. ("BSD                         2-Clause "" License" 'BSD-2-Clause                                             nil))
     (BSD-3-Clause-Attribution              . ("BSD with attribution"                                                         nil))
     (BSD-3-Clause-Clear                    . ("BSD 3-Clause Clear License"                                 t))
     (BSD-3-Clause-LBNL                     . ("Lawrence Berkeley National Labs BSD variant license"                          nil))
     (BSD-3-Clause-No-Nuclear-License-2014  . ("BSD 3-Clause No Nuclear License 2014"                                         nil))
     (BSD-3-Clause-No-Nuclear-License       . ("BSD 3-Clause No Nuclear License"                                              nil))
     (BSD-3-Clause-No-Nuclear-Warranty      . ("BSD 3-Clause No Nuclear Warranty"                                             nil))
     (ew. ("BSD                                3-Clause "" or "Revised" License" 'BSD-3-Clause              t))
     (BSD-4-Clause-UC                       . ("BSD-4-Clause (University of California-Specific)"                             nil))
     (riginal. ("BSD                           4-Clause "" or "Old" License" 'BSD-4-Clause                  t))
     (BSD-Protection                        . ("BSD Protection License"                                                       nil))
     (BSD-Source-Code                       . ("BSD Source Code Attribution"                                                  nil))
     (BSL-1.0                               . ("Boost Software License 1.0"                                 t))
     (bzip2-1.0.5                           . ("bzip2 and libbzip2 License v1.0.5"                                            nil))
     (bzip2-1.0.6                           . ("bzip2 and libbzip2 License v1.0.6"                                            nil))
     (Caldera                               . ("Caldera License"                                                              nil))
     (CATOSL-1.1                            . ("Computer Associates Trusted Open Source License 1.1"                          nil))
     (CC-BY-1.0                             . ("Creative Commons Attribution 1.0 Generic"                                     nil))
     (CC-BY-2.0                             . ("Creative Commons Attribution 2.0 Generic"                                     nil))
     (CC-BY-2.5                             . ("Creative Commons Attribution 2.5 Generic"                                     nil))
     (CC-BY-3.0                             . ("Creative Commons Attribution 3.0 Unported"                                    nil))
     (CC-BY-4.0                             . ("Creative Commons Attribution 4.0 International"             t))
     (CC-BY-NC-1.0                          . ("Creative Commons Attribution Non Commercial 1.0 Generic"                      nil))
     (CC-BY-NC-2.0                          . ("Creative Commons Attribution Non Commercial 2.0 Generic"                      nil))
     (CC-BY-NC-2.5                          . ("Creative Commons Attribution Non Commercial 2.5 Generic"                      nil))
     (CC-BY-NC-3.0                          . ("Creative Commons Attribution Non Commercial 3.0 Unported"                     nil))
     (CC-BY-NC-4.0                          . ("Creative Commons Attribution Non Commercial 4.0 International"                nil))
     (CC-BY-NC-ND-1.0                       . ("Creative Commons Attribution Non Commercial No Derivatives 1.0 Generic"       nil))
     (CC-BY-NC-ND-2.0                       . ("Creative Commons Attribution Non Commercial No Derivatives 2.0 Generic"       nil))
     (CC-BY-NC-ND-2.5                       . ("Creative Commons Attribution Non Commercial No Derivatives 2.5 Generic"       nil))
     (CC-BY-NC-ND-3.0                       . ("Creative Commons Attribution Non Commercial No Derivatives 3.0 Unported"      nil))
     (CC-BY-NC-ND-4.0                       . ("Creative Commons Attribution Non Commercial No Derivatives 4.0 International" nil))
     (CC-BY-NC-SA-1.0                       . ("Creative Commons Attribution Non Commercial Share Alike 1.0 Generic"          nil))
     (CC-BY-NC-SA-2.0                       . ("Creative Commons Attribution Non Commercial Share Alike 2.0 Generic"          nil))
     (CC-BY-NC-SA-2.5                       . ("Creative Commons Attribution Non Commercial Share Alike 2.5 Generic"          nil))
     (CC-BY-NC-SA-3.0                       . ("Creative Commons Attribution Non Commercial Share Alike 3.0 Unported"         nil))
     (CC-BY-NC-SA-4.0                       . ("Creative Commons Attribution Non Commercial Share Alike 4.0 International"    nil))
     (CC-BY-ND-1.0                          . ("Creative Commons Attribution No Derivatives 1.0 Generic"                      nil))
     (CC-BY-ND-2.0                          . ("Creative Commons Attribution No Derivatives 2.0 Generic"                      nil))
     (CC-BY-ND-2.5                          . ("Creative Commons Attribution No Derivatives 2.5 Generic"                      nil))
     (CC-BY-ND-3.0                          . ("Creative Commons Attribution No Derivatives 3.0 Unported"                     nil))
     (CC-BY-ND-4.0                          . ("Creative Commons Attribution No Derivatives 4.0 International"                nil))
     (CC-BY-SA-1.0                          . ("Creative Commons Attribution Share Alike 1.0 Generic"                         nil))
     (CC-BY-SA-2.0                          . ("Creative Commons Attribution Share Alike 2.0 Generic"                         nil))
     (CC-BY-SA-2.5                          . ("Creative Commons Attribution Share Alike 2.5 Generic"                         nil))
     (CC-BY-SA-3.0                          . ("Creative Commons Attribution Share Alike 3.0 Unported"                        nil))
     (CC-BY-SA-4.0                          . ("Creative Commons Attribution Share Alike 4.0 International" t))
     (CC0-1.0                               . ("Creative Commons Zero v1.0 Universal"                       t))
     (CDDL-1.0                              . ("Common Development and Distribution License 1.0"            t))
     (CDDL-1.1                              . ("Common Development and Distribution License 1.1"                              nil))
     (CDLA-Permissive-1.0                   . ("Community Data License Agreement Permissive 1.0"                              nil))
     (CDLA-Sharing-1.0                      . ("Community Data License Agreement Sharing 1.0"                                 nil))
     (CECILL-1.0                            . ("CeCILL Free Software License Agreement v1.0"                                  nil))
     (CECILL-1.1                            . ("CeCILL Free Software License Agreement v1.1"                                  nil))
     (CECILL-2.0                            . ("CeCILL Free Software License Agreement v2.0"                t))
     (CECILL-2.1                            . ("CeCILL Free Software License Agreement v2.1"                                  nil))
     (CECILL-B                              . ("CeCILL-B Free Software License Agreement"                   t))
     (CECILL-C                              . ("CeCILL-C Free Software License Agreement"                   t))
     (ClArtistic                            . ("Clarified Artistic License"                                 t))
     (CNRI-Jython                           . ("CNRI Jython License"                                                          nil))
     (CNRI-Python-GPL-Compatible            . ("CNRI Python Open Source GPL Compatible License Agreement"                     nil))
     (CNRI-Python                           . ("CNRI Python License"                                                          nil))
     (Condor-1.1                            . ("Condor Public License v1.1"                                 t))
     (copyleft-next-0.3.0                   . ("copyleft-next 0.3.0"                                                          nil))
     (copyleft-next-0.3.1                   . ("copyleft-next 0.3.1"                                                          nil))
     (CPAL-1.0                              . ("Common Public Attribution License 1.0"                      t))
     (CPL-1.0                               . ("Common Public License 1.0"                                  t))
     (CPOL-1.02                             . ("Code Project Open License 1.02"                                               nil))
     (Crossword                             . ("Crossword License"                                                            nil))
     (CrystalStacker                        . ("CrystalStacker License"                                                       nil))
     (CUA-OPL-1.0                           . ("CUA Office Public License v1.0"                                               nil))
     (Cube                                  . ("Cube License"                                                                 nil))
     (curl                                  . ("curl License"                                                                 nil))
     (D-FSL-1.0                             . ("Deutsche Freie Software Lizenz"                                               nil))
     (diffmark                              . ("diffmark license"                                                             nil))
     (DOC                                   . ("DOC License"                                                                  nil))
     (Dotseqn                               . ("Dotseqn License"                                                              nil))
     (DSDP                                  . ("DSDP License"                                                                 nil))
     (dvipdfm                               . ("dvipdfm License"                                                              nil))
     (ECL-1.0                               . ("Educational Community License v1.0"                                           nil))
     (ECL-2.0                               . ("Educational Community License v2.0"                         t))
     (EFL-1.0                               . ("Eiffel Forum License v1.0"                                                    nil))
     (EFL-2.0                               . ("Eiffel Forum License v2.0"                                  t))
     (eGenix                                . ("eGenix.com Public License 1.1.0"                                              nil))
     (Entessa                               . ("Entessa Public License v1.0"                                                  nil))
     (EPL-1.0                               . ("Eclipse Public License 1.0"                                 t))
     (EPL-2.0                               . ("Eclipse Public License 2.0"                                 t))
     (ErlPL-1.1                             . ("Erlang Public License v1.1"                                                   nil))
     (EUDatagrid                            . ("EU DataGrid Software License"                               t))
     (EUPL-1.0                              . ("European Union Public License 1.0"                                            nil))
     (EUPL-1.1                              . ("European Union Public License 1.1"                          t))
     (EUPL-1.2                              . ("European Union Public License 1.2"                          t))
     (Eurosym                               . ("Eurosym License"                                                              nil))
     (Fair                                  . ("Fair License"                                                                 nil))
     (Frameworx-1.0                         . ("Frameworx Open License 1.0"                                                   nil))
     (FreeImage                             . ("FreeImage Public License v1.0"                                                nil))
     (FSFAP                                 . ("FSF All Permissive License"                                 t))
     (FSFUL                                 . ("FSF Unlimited License"                                                        nil))
     (FSFULLR                               . ("FSF Unlimited License (with License Retention)"                               nil))
     (FTL                                   . ("Freetype Project License"                                   t))
     (GFDL-1.1-only                         . ("GNU Free Documentation License v1.1 only"                   t))
     (GFDL-1.1-or-later                     . ("GNU Free Documentation License v1.1 or later"               t))
     (GFDL-1.2-only                         . ("GNU Free Documentation License v1.2 only"                   t))
     (GFDL-1.2-or-later                     . ("GNU Free Documentation License v1.2 or later"               t))
     (GFDL-1.3-only                         . ("GNU Free Documentation License v1.3 only"                   t))
     (GFDL-1.3-or-later                     . ("GNU Free Documentation License v1.3 or later"               t))
     (Giftware                              . ("Giftware License"                                                             nil))
     (GL2PS                                 . ("GL2PS License"                                                                nil))
     (Glide                                 . ("3dfx Glide License"                                                           nil))
     (Glulxe                                . ("Glulxe License"                                                               nil))
     (gnuplot                               . ("gnuplot License"                                            t))
     (GPL-1.0-only                          . ("GNU General Public License v1.0 only"                                         nil))
     (GPL-1.0-or-later                      . ("GNU General Public License v1.0 or later"                                     nil))
     (GPL-2.0-only                          . ("GNU General Public License v2.0 only"                       t))
     (GPL-2.0-or-later                      . ("GNU General Public License v2.0 or later"                   t))
     (GPL-3.0-only                          . ("GNU General Public License v3.0 only"                       t))
     (GPL-3.0-or-later                      . ("GNU General Public License v3.0 or later"                   t))
     (gSOAP-1.3b                            . ("gSOAP Public License v1.3b"                                                   nil))
     (HaskellReport                         . ("Haskell Language Report License"                                              nil))
     (HPND                                  . ("Historical Permission Notice and Disclaimer"                t))
     (IBM-pibs                              . ("IBM PowerPC Initialization and Boot Software"                                 nil))
     (ICU                                   . ("ICU License"                                                                  nil))
     (IJG                                   . ("Independent JPEG Group License"                             t))
     (ImageMagick                           . ("ImageMagick License"                                                          nil))
     (iMatix                                . ("iMatix Standard Function Library Agreement"                 t))
     (Imlib2                                . ("Imlib2 License"                                             t))
     (Info-ZIP                              . ("Info-ZIP License"                                                             nil))
     (Intel-ACPI                            . ("Intel ACPI Software License Agreement"                                        nil))
     (Intel                                 . ("Intel Open Source License"                                  t))
     (Interbase-1.0                         . ("Interbase Public License v1.0"                                                nil))
     (IPA                                   . ("IPA Font License"                                           t))
     (IPL-1.0                               . ("IBM Public License v1.0"                                    t))
     (ISC                                   . ("ISC License"                                                t))
     (JasPer-2.0                            . ("JasPer License"                                                               nil))
     (JSON                                  . ("JSON License"                                                                 nil))
     (LAL-1.2                               . ("Licence Art Libre 1.2"                                                        nil))
     (LAL-1.3                               . ("Licence Art Libre 1.3"                                                        nil))
     (Latex2e                               . ("Latex2e License"                                                              nil))
     (Leptonica                             . ("Leptonica License"                                                            nil))
     (LGPL-2.0-only                         . ("GNU Library General Public License v2 only"                                   nil))
     (LGPL-2.0-or-later                     . ("GNU Library General Public License v2 or later"                               nil))
     (LGPL-2.1-only                         . ("GNU Lesser General Public License v2.1 only"                t))
     (LGPL-2.1-or-later                     . ("GNU Lesser General Public License v2.1 or later"            t))
     (LGPL-3.0-only                         . ("GNU Lesser General Public License v3.0 only"                t))
     (LGPL-3.0-or-later                     . ("GNU Lesser General Public License v3.0 or later"            t))
     (LGPLLR                                . ("Lesser General Public License For Linguistic Resources"                       nil))
     (Libpng                                . ("libpng License"                                                               nil))
     (libtiff                               . ("libtiff License"                                                              nil))
     (LiLiQ-P-1.1                           . ("Licence Libre du Québec – Permissive version 1.1"                             nil))
     (LiLiQ-R-1.1                           . ("Licence Libre du Québec – Réciprocité version 1.1"                            nil))
     (LiLiQ-Rplus-1.1                       . ("Licence Libre du Québec – Réciprocité forte version 1.1"                      nil))
     (Linux-OpenIB                          . ("Linux Kernel Variant of OpenIB.org license"                                   nil))
     (LPL-1.0                               . ("Lucent Public License Version 1.0"                                            nil))
     (LPL-1.02                              . ("Lucent Public License v1.02"                                t))
     (LPPL-1.0                              . ("LaTeX Project Public License v1.0"                                            nil))
     (LPPL-1.1                              . ("LaTeX Project Public License v1.1"                                            nil))
     (LPPL-1.2                              . ("LaTeX Project Public License v1.2"                          t))
     (LPPL-1.3a                             . ("LaTeX Project Public License v1.3a"                         t))
     (LPPL-1.3c                             . ("LaTeX Project Public License v1.3c"                                           nil))
     (MakeIndex                             . ("MakeIndex License"                                                            nil))
     (MirOS                                 . ("MirOS License"                                                                nil))
     (MIT-0                                 . ("MIT No Attribution"                                                           nil))
     (MIT-advertising                       . ("Enlightenment License (e16)"                                                  nil))
     (MIT-CMU                               . ("CMU License"                                                                  nil))
     (MIT-enna                              . ("enna License"                                                                 nil))
     (MIT-feh                               . ("feh License"                                                                  nil))
     (MIT                                   . ("MIT License"                                                t))
     (MITNFA                                . ("MIT +no-false-attribs license"                                                nil))
     (Motosoto                              . ("Motosoto License"                                                             nil))
     (mpich2                                . ("mpich2 License"                                                               nil))
     (MPL-1.0                               . ("Mozilla Public License 1.0"                                                   nil))
     (MPL-1.1                               . ("Mozilla Public License 1.1"                                 t))
     (MPL-2.0-no-copyleft-exception         . ("Mozilla Public License 2.0 (no copyleft exception)"                           nil))
     (MPL-2.0                               . ("Mozilla Public License 2.0"                                 t))
     (MS-PL                                 . ("Microsoft Public License"                                   t))
     (MS-RL                                 . ("Microsoft Reciprocal License"                               t))
     (MTLL                                  . ("Matrix Template Library License"                                              nil))
     (Multics                               . ("Multics License"                                                              nil))
     (Mup                                   . ("Mup License"                                                                  nil))
     (NASA-1.3                              . ("NASA Open Source Agreement 1.3"                                               nil))
     (Naumen                                . ("Naumen Public License"                                                        nil))
     (NBPL-1.0                              . ("Net Boolean Public License v1"                                                nil))
     (NCSA                                  . ("University of Illinois/NCSA Open Source License"            t))
     (Net-SNMP                              . ("Net-SNMP License"                                                             nil))
     (NetCDF                                . ("NetCDF license"                                                               nil))
     (Newsletr                              . ("Newsletr License"                                                             nil))
     (NGPL                                  . ("Nethack General Public License"                                               nil))
     (NLOD-1.0                              . ("Norwegian Licence for Open Government Data"                                   nil))
     (NLPL                                  . ("No Limit Public License"                                                      nil))
     (Nokia                                 . ("Nokia Open Source License"                                  t))
     (NOSL                                  . ("Netizen Open Source License"                                t))
     (Noweb                                 . ("Noweb License"                                                                nil))
     (NPL-1.0                               . ("Netscape Public License v1.0"                               t))
     (NPL-1.1                               . ("Netscape Public License v1.1"                               t))
     (NPOSL-3.0                             . ("Non-Profit Open Software License 3.0"                                         nil))
     (NRL                                   . ("NRL License"                                                                  nil))
     (NTP                                   . ("NTP License"                                                                  nil))
     (OCCT-PL                               . ("Open CASCADE Technology Public License"                                       nil))
     (OCLC-2.0                              . ("OCLC Research Public License 2.0"                                             nil))
     (ODbL-1.0                              . ("ODC Open Database License v1.0"                             t))
     (ODC-By-1.0                            . ("Open Data Commons Attribution License v1.0"                                   nil))
     (OFL-1.0                               . ("SIL Open Font License 1.0"                                  t))
     (OFL-1.1                               . ("SIL Open Font License 1.1"                                  t))
     (OGL-UK-1.0                            . ("Open Government Licence v1.0"                                                 nil))
     (OGL-UK-2.0                            . ("Open Government Licence v2.0"                                                 nil))
     (OGL-UK-3.0                            . ("Open Government Licence v3.0"                                                 nil))
     (OGTSL                                 . ("Open Group Test Suite License"                                                nil))
     (OLDAP-1.1                             . ("Open LDAP Public License v1.1"                                                nil))
     (OLDAP-1.2                             . ("Open LDAP Public License v1.2"                                                nil))
     (OLDAP-1.3                             . ("Open LDAP Public License v1.3"                                                nil))
     (OLDAP-1.4                             . ("Open LDAP Public License v1.4"                                                nil))
     (OLDAP-2.0.1                           . ("Open LDAP Public License v2.0.1"                                              nil))
     (OLDAP-2.0                             . ("Open LDAP Public License v2.0 (or possibly 2.0A and 2.0B)"                    nil))
     (OLDAP-2.1                             . ("Open LDAP Public License v2.1"                                                nil))
     (OLDAP-2.2.1                           . ("Open LDAP Public License v2.2.1"                                              nil))
     (OLDAP-2.2.2                           . ("Open LDAP Public License 2.2.2"                                               nil))
     (OLDAP-2.2                             . ("Open LDAP Public License v2.2"                                                nil))
     (OLDAP-2.3                             . ("Open LDAP Public License v2.3"                              t))
     (OLDAP-2.4                             . ("Open LDAP Public License v2.4"                                                nil))
     (OLDAP-2.5                             . ("Open LDAP Public License v2.5"                                                nil))
     (OLDAP-2.6                             . ("Open LDAP Public License v2.6"                                                nil))
     (OLDAP-2.7                             . ("Open LDAP Public License v2.7"                              t))
     (OLDAP-2.8                             . ("Open LDAP Public License v2.8"                                                nil))
     (OML                                   . ("Open Market License"                                                          nil))
     (OpenSSL                               . ("OpenSSL License"                                            t))
     (OPL-1.0                               . ("Open Public License v1.0"                                                     nil))
     (OSET-PL-2.1                           . ("OSET Public License version 2.1"                                              nil))
     (OSL-1.0                               . ("Open Software License 1.0"                                  t))
     (OSL-1.1                               . ("Open Software License 1.1"                                  t))
     (OSL-2.0                               . ("Open Software License 2.0"                                  t))
     (OSL-2.1                               . ("Open Software License 2.1"                                  t))
     (OSL-3.0                               . ("Open Software License 3.0"                                  t))
     (PDDL-1.0                              . ("ODC Public Domain Dedication & License 1.0"                                   nil))
     (PHP-3.0                               . ("PHP License v3.0"                                                             nil))
     (PHP-3.01                              . ("PHP License v3.01"                                          t))
     (Plexus                                . ("Plexus Classworlds License"                                                   nil))
     (PostgreSQL                            . ("PostgreSQL License"                                                           nil))
     (psfrag                                . ("psfrag License"                                                               nil))
     (psutils                               . ("psutils License"                                                              nil))
     (Python-2.0                            . ("Python License 2.0"                                         t))
     (Qhull                                 . ("Qhull License"                                                                nil))
     (QPL-1.0                               . ("Q Public License 1.0"                                       t))
     (Rdisc                                 . ("Rdisc License"                                                                nil))
     (RHeCos-1.1                            . ("Red Hat eCos Public License v1.1"                                             nil))
     (RPL-1.1                               . ("Reciprocal Public License 1.1"                                                nil))
     (RPL-1.5                               . ("Reciprocal Public License 1.5"                                                nil))
     (RPSL-1.0                              . ("RealNetworks Public Source License v1.0"                    t))
     (RSA-MD                                . ("RSA Message-Digest License"                                                   nil))
     (RSCPL                                 . ("Ricoh Source Code Public License"                                             nil))
     (Ruby                                  . ("Ruby License"                                               t))
     (SAX-PD                                . ("Sax Public Domain Notice"                                                     nil))
     (Saxpath                               . ("Saxpath License"                                                              nil))
     (SCEA                                  . ("SCEA Shared Source License"                                                   nil))
     (Sendmail-8.23                         . ("Sendmail License 8.23"                                                        nil))
     (Sendmail                              . ("Sendmail License"                                                             nil))
     (SGI-B-1.0                             . ("SGI Free Software License B v1.0"                                             nil))
     (SGI-B-1.1                             . ("SGI Free Software License B v1.1"                                             nil))
     (SGI-B-2.0                             . ("SGI Free Software License B v2.0"                           t))
     (SimPL-2.0                             . ("Simple Public License 2.0"                                                    nil))
     (SISSL-1.2                             . ("Sun Industry Standards Source License v1.2"                                   nil))
     (SISSL                                 . ("Sun Industry Standards Source License v1.1"                 t))
     (Sleepycat                             . ("Sleepycat License"                                          t))
     (SMLNJ                                 . ("Standard ML of New Jersey License"                          t))
     (SMPPL                                 . ("Secure Messaging Protocol Public License"                                     nil))
     (SNIA                                  . ("SNIA Public License 1.1"                                                      nil))
     (Spencer-86                            . ("Spencer License 86"                                                           nil))
     (Spencer-94                            . ("Spencer License 94"                                                           nil))
     (Spencer-99                            . ("Spencer License 99"                                                           nil))
     (SPL-1.0                               . ("Sun Public License v1.0"                                    t))
     (SugarCRM-1.1.3                        . ("SugarCRM Public License v1.1.3"                                               nil))
     (SWL                                   . ("Scheme Widget Library (SWL) Software License Agreement"                       nil))
     (TCL                                   . ("TCL/TK License"                                                               nil))
     (TCP-wrappers                          . ("TCP Wrappers License"                                                         nil))
     (TMate                                 . ("TMate Open Source License"                                                    nil))
     (TORQUE-1.1                            . ("TORQUE v2.5+ Software License v1.1"                                           nil))
     (TOSL                                  . ("Trusster Open Source License"                                                 nil))
     (TU-Berlin-1.0                         . ("Technische Universitaet Berlin License 1.0"                                   nil))
     (TU-Berlin-2.0                         . ("Technische Universitaet Berlin License 2.0"                                   nil))
     (Unicode-DFS-2015                      . ("Unicode License Agreement - Data Files and Software (2015)"                   nil))
     (Unicode-DFS-2016                      . ("Unicode License Agreement - Data Files and Software (2016)"                   nil))
     (Unicode-TOU                           . ("Unicode Terms of Use"                                                         nil))
     (Unlicense                             . ("The Unlicense"                                              t))
     (UPL-1.0                               . ("Universal Permissive License v1.0"                          t))
     (Vim                                   . ("Vim License"                                                t))
     (VOSTROM                               . ("VOSTROM Public License for Open Source"                                       nil))
     (VSL-1.0                               . ("Vovida Software License v1.0"                                                 nil))
     (W3C-19980720                          . ("W3C Software Notice and License (1998-07-20)"                                 nil))
     (W3C-20150513                          . ("W3C Software Notice and Document License (2015-05-13)"                        nil))
     (W3C                                   . ("W3C Software Notice and License (2002-12-31)"               t))
     (Watcom-1.0                            . ("Sybase Open Watcom Public License 1.0"                                        nil))
     (Wsuipa                                . ("Wsuipa License"                                                               nil))
     (WTFPL                                 . ("Do What The F*ck You Want To Public License"                t))
     (X11                                   . ("X11 License"                                                                    t))
     (Xerox                                 . ("Xerox License"                                                                nil))
     (XFree86-1.1                           . ("XFree86 License 1.1"                                        t))
     (xinetd                                . ("xinetd License"                                             t))
     (Xnet                                  . ("X.Net License"                                                                nil))
     (xpp                                   . ("XPP License"                                                                  nil))
     (XSkat                                 . ("XSkat License"                                                                nil))
     (YPL-1.0                               . ("Yahoo! Public License v1.0"                                                   nil))
     (YPL-1.1                               . ("Yahoo! Public License v1.1"                                 t))
     (Zed                                   . ("Zed License"                                                                  nil))
     (Zend-2.0                              . ("Zend License v2.0"                                          t))
     (Zimbra-1.3                            . ("Zimbra Public License v1.3"                                 t))
     (Zimbra-1.4                            . ("Zimbra Public License v1.4"                                                   nil))
     (zlib-acknowledgement                  . ("zlib/libpng License with Acknowledgement"                                     nil))
     (Zlib                                  . ("zlib License"                                               t))
     (ZPL-1.1                               . ("Zope Public License 1.1"                                                      nil))
     (ZPL-2.0                               . ("Zope Public License 2.0"                                    t))
     (ZPL-2.1                               . ("Zope Public License 2.1"                                    t))

   )

  "SPDX License Identifiers with their descriptions.

Links:

• URL `https://spdx.org/licenses/'

Version (of SPDX): 3.4 2018-12-20"

  :type '(alist :key-type   (string :tag "SPDX License Identifier")
                :value-type (list (string :tag "License Description")
                                  (boolean :tag "FSF Free/Libre?")))

  :safe t
  :group 'sboo)

;;----------------------------------------------;;

(defun sboo-spdx-license-libre-p (license)

  "Is LICENSE free-and-libre?

Inputs:

• LICENSE — a symbol.

Output:

• a boolean.

Example:

• M-: (sboo-spdx-is-license-free-and-libre 'GPL-3.0-or-later)
    ⇒ t

Links:

• URL `https://www.gnu.org/licenses/license-list.en.html' (Free Software Foundation)"

  (let* ()

    ))

;;----------------------------------------------;;

(cl-defun sboo-read-feature (&key prompt require-match initial-input only-builtins)

  "Read a `featurep' symbol.

Inputs:

• ONLY-BUILTINS — a `booleanp'.
  whether to restrict the output (and completion)
  to features from Builtin Package (i.e. features
  which are distributed with the official Emacs distribution,
  as of `emacs-version' ≥25).

Inputs (pass-thru):

• PROMPT        — c.f. `completing-read'.
• REQUIRE-MATCH — c.f. `completing-read'.
• INITIAL-INPUT — c.f. `completing-read'.

Output:

• a `symbolp'
  a `featurep' (which can be `require'd).
  if ONLY-BUILTINS is non-nil, returns a
  Builtin Package.

Related:

• `features'"

  (interactive)

  (let ((PROMPT (format "%s: "
                        (or prompt "Feature")))

        (REQUIRE-MATCH (or require-match t))
        (INITIAL-INPUT initial-input)
        (CANDIDATES    (if only-builtins

                           (progn
                             (require 'package)
                             (mapcar #'car package--builtins))

                         features))
        )

    (let* ((STRING (completing-read PROMPT CANDIDATES nil REQUIRE-MATCH INITIAL-INPUT))
           (SYMBOL (intern-soft STRING))
           )

      SYMBOL)))

;; ^ Notes
;;
;; M-: (type-of features)
;; 'cons
;;
;;
;;

;;----------------------------------------------;;

(cl-defun sboo-read-command (&key prompt require-match initial-input)

  "Read a `commandp' symbol.

Related:

• `obarray'"

  (interactive)

  (let ((PROMPT (format "%s: "
                        (or prompt "Command")))

        (REQUIRE-MATCH (or require-match t))
        (INITIAL-INPUT (or initial-input "sboo-"))
        )

    (let* ((STRING (completing-read PROMPT obarray #'commandp REQUIRE-MATCH INITIAL-INPUT))
           (SYMBOL (intern-soft STRING))
           )

      SYMBOL)))

;; ^ Notes
;;
;; M-: (type-of obarray)
;; 'vector
;;

;;----------------------------------------------;;

(cl-defun sboo-read-spdx-license (&optional libre-only)
;;(cl-defun sboo-read-spdx-license (&key libre-only)

  "Read an SPDX License Identifier.

Example:

• M-x sboo-read-spdx-license <RET>
    ⇒ ()

Links:

• URL `https://spdx.org/licenses/'

Related:

• `sboo-spdx-license-alist'"


  (interactive (list
                (if current-prefix-arg t nil)
                ))

  (let*  ((PROMPT        (format "%s: " "License")) ;(format "%s: " (or prompt "License")))
          (REQUIRE-MATCH t) ;(or require-match t))
          (INITIAL-INPUT "GPL-") ;(or initial-input "GPL-"))
          (PREDICATE     (if libre-only
                             sboo-spdx-license-libre-p
                           nil))
          (CANDIDATES    (mapcar #'car sboo-spdx-license-alist))
          ;; ^ « mapcar #'car » is a « alist-keys ».
          )

         (let* ((STRING (completing-read PROMPT CANDIDATES PREDICATE REQUIRE-MATCH INITIAL-INPUT))
                (SYMBOL (intern-soft STRING))
                )

           SYMBOL)))

;;----------------------------------------------;;

(cl-defun sboo-insert-spdx-license (license &optional libre-only)

  "Insert an SPDX License Identifier.

Completion:

• via `sboo-read-spdx-license'

Links:

• URL `https://spdx.org/licenses/'

Related:

• `sboo-spdx-license-alist'"

  (interactive (let ((LIBRE-ONLY (if current-prefix-arg t nil)))
                 (list
                  (sboo-read-spdx-license LIBRE-ONLY)
                  LIBRE-ONLY
                  )))

  (let* ((STRING (symbol-name license))
         )

    (insert STRING)))

;;TODO sboo-read-environment-variable read-envvar-name

;;TODO emacs copy a lisp expression to the clipboard

;; (kill-new
;; https://stackoverflow.com/questions/2178850/how-to-copy-to-clipboard-in-emacs-lisp

;TODO few core buffers like home.nix and emacs.md
;TODO known dirs like emacs, config, haskell, notes

;;----------------------------------------------;;
;; Clipboard -----------------------------------;;
;;----------------------------------------------;;

(defcustom sboo-register-char

  ?a

  "Text-Register for appending Clipboard-Contents.

a `characterp'.

Users:

• `sboo-register-append'
• `sboo-register-yank'"

  :type '(character :tag "Register")

  :safe #'characterp
  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-register-separator

  "\n"

  "Separator for `sboo-register-append'.

a `stringp'
(c.f. `register-separator', which is a `characterp')."

  :type '(string :tag "Separator")

  :safe #'stringp
  :group 'sboo)

;;==============================================;;

(defun sboo-register-append-dwim (&optional no-message)

  "DWIM: Append region or line to `sboo-register-char'.

Each invocation of `sboo-register-append'
is separated by `sboo-register-separator'.

Inputs (passthru):

• NO-MESSAGE — a `booleanp' (see `sboo-register-append-region').

See:

• `sboo-register-yank'

Wraps:

• `append-to-register'.

Links:

• Info Node `(emacs) Text Registers'
• URL `https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Registers.html'
• URL `http://ergoemacs.org/emacs/emacs_using_register.html'
• URL `http://ergoemacs.org/emacs/emacs_copy_append.html'"

  (interactive)

  (let* ((BEG (if (region-active-p)
                  (region-beginning)
                (line-beginning-position)))

         (END (if (region-active-p)
                  (region-end)
                (line-end-position)))
         )

    (sboo-register-append-region BEG END)))

;;----------------------------------------------;;

(defun sboo-register-append-region (beg end &optional no-message)

  "Append region to contents of Text-Register `sboo-register-char'.

Each invocation of `sboo-register-append'
is separated by `sboo-register-separator'.

Inputs:

• BEG — an `integerp' or `markerp'.
  the BEGinning of a region.
• END — an `integerp' or `markerp'.
  the ENDing of a region.
• NO-MESSAGE — a `booleanp' (optional).
  whether to not `message' what text was appended.

See:

• `sboo-register-yank'

Wraps:

• `append-to-register'."

  (interactive "r")

  (let* ((register-separator ?\n)
         ;; ^ NOTE `register-separator' is a Register (not a String), with the newline beingg a mnemonic.
         ;;  The actual separator is `sboo-register-separator'.
         )

    (set-register register-separator sboo-register-separator)

    (append-to-register sboo-register-char beg end)

    (when (and (called-interactively-p 'any) (not no-message))
      (message "Appended: 「%s」" (buffer-substring beg end)))

    ()))

;;----------------------------------------------;;

(defun sboo-register-prepend (beg end)

  "Prepend region to contents of Text-Register `sboo-register-char'.

Each invocation of `sboo-register-prepend'
is separated by `sboo-register-separator'.

See:

• `sboo-register-yank'

Wraps:

• `prepend-to-register'."

  (interactive "r")

  (let* ((register-separator ?\n)
         )

    (set-register register-separator sboo-register-separator)

    (prepend-to-register sboo-register-char beg end)))

;;----------------------------------------------;;

(defun sboo-register-yank (&optional keep-register)

  "Paste contents of Text-Register `sboo-register-char'.

Inputs:

• KEEP-REGISTER — a `booleanp'.
  Text-Register `sboo-register-char' is cleared,
  *unless* KEEP-REGISTER is non-nil.

See:

• `sboo-register-append'

Wraps:

• `insert-register'."

  (interactive "P")

  (insert-register sboo-register-char)

  (unless keep-register
    (set-register sboo-register-char "")))

;;==============================================;;

(defun sboo-copy-buffer-filepath-to-clipboard ()

  "Put the current file name on the clipboard.

From `https://stackoverflow.com/a/2417617/1337806'."

  (interactive)

  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;;----------------------------------------------;;
;; Appearence ----------------------------------;;
;;----------------------------------------------;;

(defun sboo-set-background-color (color)

  "Set the background color to COLOR."

  (interactive (list
                (read-color "Color: ")))

  (set-background-color color))

;;----------------------------------------------;;

(defun sboo-invert-colors ()

  "Invert all colors in the current theme."

  (interactive)

  (let* ()

    (modify-frame-parameters (selected-frame)
			     `( (reverse . t)
                                ))

    ()))

;;----------------------------------------------;;
;; Colors: Thing -------------------------------;;
;;----------------------------------------------;;

(put 'color 'forward-op #'sboo-forward-color)

;;;###autoload
(defun sboo-forward-color (&optional count)

  "`thing-at-point' support for colors.

Inputs:

• COUNT — an `integerp'.

Effect:

• move forwards (or backwards) COUNT (or -COUNT) colors.

Definitions:

• a « 'color » thing can be:

    • a Hex string, like « \"#8C5353\" ».
    • a color name, like « \"red\" ».
    • an RGB triplet, like « (1.0 0.0 0.0) ».

Links:

• URL `'

Related:

• `'"

  (let* ()

    ))                                  ;TODO

;;----------------------------------------------;;
;; Colors: Invert ------------------------------;;
;;----------------------------------------------;;

(defun sboo-invert-color-string (string)

  "Invert a hex string or color name.

Inputs:

• STRING — a `stringp'. Either:

    • a hex string, like « \"#8C5353\" ».
    • a color name, like « \"red\" ».

Output:

• a `stringp'

Example:

• M-: (sboo-invert-color-string \"#8C5353\")
    ⇒ \"#73ACAC\"
• M-: (sboo-invert-color-string \"8c5353\")
    ⇒ \"73acac\"
• M-: (sboo-invert-color-string \"red\")
    ⇒ \"cyan\"

Laws:

• Involution (i.e. it's its own inverse i.e. applying it twice is identity) given Hex-Strings.
  Not an involution when given Color-Names.

Wraps `color-complement'."

  (interactive (list
                (thing-at-point 'word)
                ))

  (let* (                               ;TODO if start with "#" strip then add back. preserve A-F casing.
         )

    (color-complement string)))

;;----------------------------------------------;;

(defun sboo-invert-color-at-point ()

  "Invert the hex string or color name.

Inputs:

• STRING — a `stringp'. Either:

    • a hex string, like « \"#8C5353\" ».
    • a color name, like « \"red\" ».

Output:

• a `stringp'

Example:

• M-: (sboo-invert-color-string \"#8C5353\")
    ⇒ \"#73ACAC\"
• M-: (sboo-invert-color-string \"8c5353\")
    ⇒ \"73acac\"
• M-: (sboo-invert-color-string \"red\")
    ⇒ \"cyan\"

Laws:

• Involution (i.e. it's its own inverse i.e. applying it twice is identity) given Hex-Strings.
  Not an involution when given Color-Names.

Wraps `sboo-invert-color-string'."

  (interactive)                         ;TODO region or word, -dwim

  (let* ((WORD (thing-at-point 'word))

         (ORIGINAL-COLOR (sboo-color-p WORD))
         (INVERSE-COLOR  (sboo-invert-color-string ORIGINAL-COLOR))

         (INVERSE-WORD   (format "%s"
                                 (color-rgb-to-hex (nth 0 INVERSE-COLOR)
                                                   (nth 1 INVERSE-COLOR)
                                                   (nth 2 INVERSE-COLOR))))
         )

    (save-excursion

      (sboo-kill-thing-at-point 'word)

      (insert INVERSE-WORD))))

;; e.g. (sboo-invert-color-at-point "#0000ffffffff")

;; ^ NOTE
;;
;;   M-: (color-rgb-to-hex 0.0 1.0 1.0)
;;       "#0000ffffffff"
;;
;;   M-: (color-rgb-to-hex @'(0.0 1.0 1.0))
;;       TODO
;;

;;----------------------------------------------;;

(defun sboo-color-p (color)

  "Whether COLOR is a valid color string.

Inputs:

• COLOR — a `stringp' color, one of:

    • a Hex Value  —
    • a Color Name —

Output:

• a `stringp' or nil.
  normalized COLOR.

Example:

• M-: (sboo-color-p \"red\")
    ⇒ \"red\"
• M-: (sboo-color-p \"RED\")
    ⇒ \"red\"
• M-: (sboo-color-p \"der\")
    ⇒ nil

Links:

• URL `'

Related:

• `'"

  (let* ((COLOR (downcase color))
         )

    (identity COLOR)))

;;----------------------------------------------;;

(defun sboo-color-name (color)

  "TODO Return the named color closest to COLOR.

Inputs:

• COLOR — a color:

    • a Hex string   —
    • an RGB triplet —
    • an HSL triplet —
    • a Color Name string —

Output:

• a `stringp'.
  a Color Name with a minimal `color-distance' from COLOR.

Example:

• M-: (sboo-color-name \"red\")
    ⇒ \"red\"

Links:

• URL `'

Related:

• `'"

  (let* ()

    ))

;;==============================================;;

(defcustom sboo-things-builtin

  '( symbol
     list
     sexp
     defun
     filename
     url
     email
     word
     sentence
     whitespace
     line
     page
    )

  "Builtin things for `forward-thing' (see `thingatpt').")

;;----------------------------------------------;;

(defcustom sboo-things-custom

  '(
   )

  "Custom things for `forward-thing' (besides `sboo-things-builtin')."

  :type '(repeat (symbol :tag "Thing"))

  :safe t
  :group 'sboo)

;;----------------------------------------------;;

(cl-defun sboo-things (&key (fast nil))

  "List all known things for `forward-thing'.

Inputs:

• FAST — a `booleanp'.

Output:

• a `listp' of `symbolp's.

Example:

• M-: (sboo-things :fast nil)
    ⇒ '(symbol list sexp defun filename url email word sentence whitespace line page str page op line list word point button symbol sentence paragraph defun char comment whitespace thing sexp)

Related:

• `sboo-things-builtin'
• `sboo-things-custom'
• `sboo-forward'"

  (let* ((KNOWN-THINGS (append sboo-things-builtin sboo-things-custom))
         )

    (if fast
        KNOWN-THINGS

      (let* ((UNKNOWN-THINGS '())
             (ADD-THING       (lambda (symbol)
                                (when (sboo-thing-p symbol)
                                    (push symbol UNKNOWN-THINGS))))
             )

        (progn
          (mapatoms ADD-THING obarray)

          (let* ((THINGS (append KNOWN-THINGS UNKNOWN-THINGS))
                 )
            (progn
              (remove-duplicates THINGS :test #'eq :from-end t)
              THINGS)))))))

;; ^ NOTE
;;
;;   M-: (remove-duplicates '(a b b a) :test #'eq :from-end t)

;;----------------------------------------------;;

(cl-defun sboo-read-thing (&key (fast t) prompt (must-match t))

  "Read a thing (c.f. `thingatpt').

Related:

• `sboo-things'"

  (interactive (list :fast       (if current-prefix-arg t nil)
                     :prompt     nil
                     :must-match t
                ))

  (let*  ((PROMPT        (format "%s: " (or prompt "Thing")))
          (REQUIRE-MATCH must-match)
          (CANDIDATES    (sboo-things :fast fast))
          )

         (let* ((STRING (completing-read PROMPT CANDIDATES nil REQUIRE-MATCH nil))
                (SYMBOL (intern-soft STRING))
                )

           SYMBOL)))

;;----------------------------------------------;;

(cl-defun sboo-forward-ops (&key)

  "List all known things for `forward-thing'.

Inputs:

• FAST — a `booleanp'.

Output:

• a `listp' of `functionp's.

Example:

• M-: (sboo-forward-ops)
    ⇒ (list #'forward-page #'forward-line #'forward-list #'forward-word #'forward-point #'forward-button #'forward-symbol #'forward-sentence #'forward-paragraph #'end-of-defun #'forward-char #'forward-comment #'forward-whitespace #'forward-thing #'forward-sexp))

Related:

• `'"

  (let* ((FORWARD-OPS    '())
         (ADD-FORWARD-OP (lambda (symbol)
                           (let* ((FORWARD-OP (sboo-forward-op symbol)))
                             (when (and FORWARD-OP (functionp FORWARD-OP))
                               (push FORWARD-OP FORWARD-OPS)))))
         )

    (progn
      (mapatoms ADD-FORWARD-OP obarray)
      FORWARD-OPS)))

;;----------------------------------------------;;

(defun sboo-thing-p (symbol)

  "Whether SYMBOL is a thing for `forward-p'.

Inputs:

• SYMBOL — a `symbolp'.

Output:

• a `booleanp'.

Example:

• M-: (sboo-thing-p 'word)
    ⇒ t

• M-: thing (sboo-thing-p 'gabagool)
    ⇒ nil"

  (if (sboo-forward-op symbol) t nil))

;;----------------------------------------------;;

(defun sboo-forward-op (symbol)

  "Return the movement function of the SYMBOL thing.

Inputs:

• SYMBOL — a `symbolp'.

Output:

• a `functionp' or nil.

Example:

• M-: (sboo-forward-op 'sexp)
    ⇒ #'`forward-sexp'
• M-: (sboo-forward-op 'acab)
    ⇒ nil

Notes:

a thing is defined as:

• « 'forward-op » symbols — i.e. any `symbolp' with the « 'forward-op » property.
• « forward-* » functions — any `functionp's which starts with “forward-”.

most things have « forward-* » functions:

• M-: (sboo-forward-op 'sexp)
    ⇒ #'`forward-sexp'
• M-: (intern-soft (format \"forward-sexp\"))
    ⇒ #'`forward-sexp'
• M-: (get 'sexp 'forward-op)
    ⇒ nil

some things have a (differently-named) « 'forward-op » property:

• M-: (sboo-forward-op 'defun)
    ⇒ #'`end-of-defun'
• M-: (intern-soft (format \"forward-defun\"))
    ⇒ nil
• M-: (get 'defun 'forward-op)
    ⇒ #'`forward-sexp'"

  (or (get symbol 'forward-op)
      (intern-soft (format "forward-%s" symbol))))

;;----------------------------------------------;;

;;;###autoload
(cl-defun sboo-forward (thing &optional (count 1))

  "Move COUNT THING(s) forward (i.e. -COUNT THING(s) backward).

Inputs:

• THING — a `symbolp'.
• COUNT — an `integerp'.

Completion (via function `sboo-things'):

• « 'forward-op » symbols — i.e. any `symbolp' with the « 'forward-op » property (see `get').
• « forward-* » functions — any `functionp's which starts with “forward-”.

Example:

• M-x sboo-forward <RET> word <RET> -2 <RET>
   ;; moves two words backward.
   ;; i.e. « (forward-thing 'word -2) »,
   ;; i.e. « (forward-word -2) ».

Related:

• `forward-thing'
• `sboo-forward-op'"

  (interactive (list
                (intern-soft (completing-read "Thing: " (sboo-things) nil :require-match))
                (read-number     "Count: " 1)
                ))

  (let* ((FORWARD-OP (sboo-forward-op thing))
         )

    (if (functionp FORWARD-OP)
        (funcall FORWARD-OP count)

      (error "(sboo-forward '%s) — %s is not a thing (for `forward-thing')" thing thing))))

;;----------------------------------------------;;
;; Buffers -------------------------------------;;
;;----------------------------------------------;;

(defun sboo-buffer-delete-all-blank-lines ()

  "Delete all blank (i.e. whitespace-only) lines (in the ‘current-buffer’).

Links:

• URL `https://emacs.stackexchange.com/questions/48526/how-do-i-delete-all-blank-lines-in-a-buffer'

Related:

• `flush-lines'"

  (interactive)

  (let* ((REGEX (rx bol (* blank) eol))
         )

    (flush-lines REGEX (point-min) (point-max))))

;;==============================================;;

(defcustom sboo-font-size-default 12

  "The default font size, for `sboo-set-font-size'.

an `integerp'."

  :type '(integer :tag "Size")

  :safe #'integerp
  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-font-name-default ""    ;TODO

  "The default font name, for `sboo-set-font-size'.

a `stringp'."

  :type '(string :tag "Name")

  :safe #'stringp
  :group 'sboo)

;;----------------------------------------------;;

(defun sboo-set-font-size (&optional font-size font-name)

  "Sets font size to FONT-SIZE (for all frames).

Inputs:

• FONT-SIZE — an `integerp'. Defaults to `font-size-default'.
• FONT-NAME — an `stringp'. Defaults to `font-name-default'.

Examples:
• M-x sboo-set-font-size <RET> <RET>"

  (interactive (list
                (read-number "number: " font-size)
                nil                     ;TODO
                ))

  (let ((FONT-SIZE (or font-size font-size-default))
        (FONT-NAME (or           font-name-default))
        )

    (set-frame-font (format "%s %d" FONT-NAME FONT-SIZE) nil t)))

;;==============================================;;
;; SQL...

(defun sboo-sql-postgres-mtg ()

  "Connect to the « M:TG-SQL » (PostgreSQL) database (via a `psql' shell).

Links:

• URL `'

Related:

• `sql-postgres'"

  (interactive)

  (let* ((sql-user     "sboo")
         (sql-database "mtgsql")        ; "postgresql:///mtgsql"
         (sql-server   "")

         (sql-postgres-options '())
        )

    (call-interactively #'sql-postgres)))

;; ^ `sql-postgres':
;;
;; Interpreter used comes from variable ‘sql-postgres-program’.  Login uses
;; the variables ‘sql-database’ and ‘sql-server’ as default, if set.
;; Additional command line parameters can be stored in the list
;; ‘sql-postgres-options’.
;;

;;----------------------------------------------;;
;; HTML ----------------------------------------;;
;;----------------------------------------------;;

(defun sboo-html-escape (start end)

  "Escape HTML characters in the region between START and END.

URL `http://shallowsky.com/blog/linux/editors/emacs-escape-html.html'."

  (interactive "r")

  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&amp;lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;")
      ())))

;;----------------------------------------------;;
;; `font-lock' ---------------------------------;;
;;----------------------------------------------;;

(defun sboo-insert-font-lock-string-face (text)

  (interactive "sString (Unquoted): ")

  (let* ((TEXT-ESCAPED     (format "%S" text))
         (TEXT-PROPERTIZED (propertize TEXT-ESCAPED
                                       'font-lock-face 'font-lock-string-face))
        )

    (insert TEXT-PROPERTIZED)))

;; M-: (sboo-insert-font-lock-string-face "example \"string\"")

;;----------------------------------------------;;
;; `ssh' ---------------------------------------;;
;;----------------------------------------------;;

(defun sboo-ssh-to-host (host)

  "Run program `ssh' into HOST.

Inputs:

• HOST — a `stringp'.
  the Hostname.

Example:

• M-: (sboo-ssh-to-host \"\")
    ⇒

Links:

• URL `https://michael.englehorn.com/config.html'"

  (interactive "sHost: ")

  (let* ((buffer-name (format "*SSH %s*" host))
         (buffer (get-buffer buffer-name))
         )

    (if buffer
        (switch-to-buffer buffer)

      (progn
        (multi-term)
        (term-send-string
         (get-buffer-process (rename-buffer buffer-name))
         (format "ssh %s\r" host))))))

;;----------------------------------------------;;
;; Processes -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-shell-command-buffer (command)

  "Capture `shell-command' output in `help-mode' buffer.

Input:

• COMMAND — a `stringp'.
  a Command Invocation
  (i.e. both the program name and the program arguments as a single line).

Examples:

• M-x sboo-shell-command-buffer
  RET \"echo 'example'\"
  RET

Links:

• URL `https://jakemccrary.com/blog/2013/08/10/emacs-capture-shell-command-output-in-temporary-buffer/'"

  (interactive (list
                (sboo-read-program :prompt "$ "
                                   :must-match nil)
                ))

  (let* ((PROGRAM     ( command))
         (BUFFER-NAME (format "*Help: %s*" PROGRAM))
         )

  (with-output-to-temp-buffer BUFFER-NAME
    (shell-command command
                   BUFFER-NAME
                   "*Messages*")
    (pop-to-buffer BUFFER-NAME))

  BUFFER-NAME))

;; ^ `help-mode':
;;
;; close the buffer by just pressing « q ».

;; ^ `shell-command':
;;
;; (shell-command COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER)

;;----------------------------------------------;;

(cl-defun sboo-read-program (&key prompt must-match)

  "`completing-read' a program name (on your « $PATH »).

Links:

• URL `'

Uses:

• `shell-command-history'"

  (interactive)

  (let* ((PROMPT     (or prompt "$ "))
         (MUST-MATCH (or must-match t))
         (HISTORY    'shell-command-history)
         (PROGRAMS   ())
        )

    (completing-read PROMPT PROGRAMS nil MUST-MATCH nil HISTORY)))

;;----------------------------------------------;;
;; Spell-Checking ------------------------------;;
;;----------------------------------------------;;

(defun sboo-flyspell-add-word ()

  "Register the current word with your personal dictionary.

Related:

• `flyspell-do-correct'

Links:

• URL `https://stackoverflow.com/questions/22107182/in-emacs-flyspell-mode-how-to-add-new-word-to-dictionary'"

  (interactive)

  (let ((CURRENT-LOCATION (point))
        (WORD             (flyspell-get-word))
        )

    (when (consp WORD)
      (flyspell-do-correct 'save nil (car word) CURRENT-LOCATION (cadr word) (caddr word) CURRENT-LOCATION))))

;;----------------------------------------------;;
;; Web -----------------------------------------;;
;;----------------------------------------------;;

(defun sboo-browse-uri-chrome (&optional uri)

   "Open URI in a Google-Chrome.

Inputs:

• URI — a `stringp'."

   (interactive)


   (let* ((URI     (or uri (read-string "URI: ")))
          (BROWSER (executable-find "google-chrome"))

          (browse-url-generic-program BROWSER)
          )

     (browse-url-generic URI)))

;;----------------------------------------------;;

;;TODO:

;; (defun sboo-browse-uri-dwim (&optional uri)

;;    "Open URI in a browser.

;; Inputs:

;; • URI — a `stringp'."

;;    (interactive)

;;    (let* ((URI (or uri
;;                    ()
;;                    (read-string "URI: ")))
;;           )

;;      URI))

;;----------------------------------------------;;
;; Shell ---------------------------------------;;
;;----------------------------------------------;;

(defun sanityinc/shell-command-in-view-mode (start end command &optional output-buffer replace &rest other-args)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless (or output-buffer replace)
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))

(advice-add 'shell-command-on-region :after 'sanityinc/shell-command-in-view-mode)

;;----------------------------------------------;;
;; `edit-indirect' -----------------------------;;
;;----------------------------------------------;;

(defun sboo-edit-indirect-dwim ()

  "DWIM: `edit-indirect' either the region or a block.

When `region-active-p', edit via `edit-indirect-region'.
Otherwise, edits the current “block”, one of:

• String     — in most `prog-mode's.
• Codeblock  — in `html-mode's and `markdown-mode's.
• Quasiquote — in `haskell-mode'.

Customize:

• `markdown-code-lang-modes' — how `markdown-edit-code-block' dispatches the `major-mode'.

IMPLEMENTATION

Related:

• `edit-indirect-region' — from feature `edit-indirect'.
• `string-edit-at-point' — from feature `string-edit'.
• `markdown-edit-code-block' — from feature `markdown-mode'.

KEYBINDINGS

If `sboo-edit-indirect-dwim' succeeds, the following keybindings are active...

Commands:

• `edit-indirect-commit' — « \\[edit-indirect-commit] »
• `edit-indirect-abort'  — « \\[edit-indirect-abort] »

Keymap:

\\{edit-indirect-mode-map}"

  (interactive)

  (let* (
         )

    (if (use-region-p)

        (call-interactively #'edit-indirect-region)

      (cond

       ((derived-mode-p 'markdown-mode)
        (markdown-edit-code-block))

       ;;TODO:
       ;; ((derived-mode-p 'emacs-lisp-mode)
       ;;  ())

       ;;TODO:
       ;; ((derived-mode-p 'haskell-mode)
       ;;  (haddock-edit-docstring))

       ((and (derived-mode-p 'prog-mode)
             (featurep 'string-edit))
        (call-interactively #'string-edit-at-point))

       (t
        (user-error "[‘sboo-edit-indirect-dwim’] Not within a code-block or docstring or string"))))))

;;TODO:
;; sboo-mark-string
;; sboo-mark-doc
;; sboo-html-mark-codeblock
;; sboo-markdown-mark-codeblock
;; sboo-haskell-mark-quasiquote

;;----------------------------------------------;;
;; Editing -------------------------------------;;
;;----------------------------------------------;;

;; (defun sboo-sort-region (beg end &optional arg)

;;   "Sort all lines between BEG and END.

;; If ARG (the \\[universal-argument]) is non-nil, 
;; sort in reverse order."

;;   (interactive "*r\nP")

;;   ())

;;----------------------------------------------;;
;; Munging -------------------------------------;;
;;----------------------------------------------;;

(defun sboo-blockquote-toggle-region (beg end)

  "Prefix each line between BEG and END with “>” (i.e. GREATER_THAN_SIGN).

Inputs:

• BEG — an `integerp'.
  Region beginning.
  e.g. `mark', `region-beginning', `point-min'.
  Defaults to `region-beginning'.
• END — an `integerp'.
  Region ending.
  e.g. `point', `region-end', `point-max'.
  Defaults to `region-end'.

Output:

• a `stringp'.

Effects:

• Modifies `current-buffer'.

Related:

• Calls `string-insert-rectangle'.
• Unlike `comment-region', `string-insert-rectangle':

    ❶ doesn't indent the comment starter.
    ❷ does fill interstitial blank lines.

Links:

• Info Node `(efaq) Inserting text at the beginning of each line'"

  (interactive "r")

  (let* ((POINT-BEG (or beg (region-beginning)))
         (POINT-END (or end (region-end)))

         (DELIMETER "> ")
         )

    (string-insert-rectangle POINT-BEG POINT-END DELIMETER)))

;;----------------------------------------------;;

(defun sboo-acronym-of (text)

  "Return an acronym of TEXT.

Examples:

• M-: (sboo-acronym-of \"Text-based user interface\")
    ⇒ \"TUI\""

  (let* ((SEPARATORS  "[/ ]")  ; e.g. or "[-/ ]"
         (OMIT-NULLS? t)

         (ACRONYM (upcase (apply #'string
                                 (mapcar #'string-to-char
                                         (split-string text SEPARATORS OMIT-NULLS?)))))
         )

    ACRONYM))

;; ^ M-: (sboo-acronym-of "Text-based user interface")
;;     ⇒ "TUI"

;;----------------------------------------------;;
;; Selection -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-extend-selection-leftward ()

  "Extend the `region-beginning' (leftwards/upwards).

Related:

• `sboo-extend-selection-rightward'"

  ())

;;----------------------------------------------;;

(defun sboo-extend-selection-rightward ()

  "Extend the `region-end' (rightwards/downwards).

Notes:

• Unlike `forward-char' under `shift-select-mode',
  `sboo-extend-selection-rightward' is invariant
  w.r.t. where the `point' and `mark' are.

Related:

• `sboo-extend-selection-leftward'"

  ())

;;----------------------------------------------;;
;; Completion ----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Miscellanea ---------------------------------;;
;;----------------------------------------------;;



;;----------------------------------------------;;
;; Re-Exports ----------------------------------;;
;;----------------------------------------------;;

(require 'sboo-english)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; `color' feature:
;;
;; M-: (color-name-to-rgb "cyan")
;;     '(0.0 1.0 1.0)
;;
;; M-: (color-rgb-to-hex 0.0 1.0 1.0)
;;     "#0000ffffffff"
;;
;; M-: (color-complement "cyan")
;;     '(1.0 0.0 0.0)
;;
;; M-: (color-values "cyan")
;;     '(0 65535 65535)
;;
;; M-: (color-lighten-name "red" 0.10)
;;     "#fffe00830083"
;;
;; M-: (color-darken-name "red" 0.10)
;;     "#ff7b00000000"
;;
;; M-: (color-saturate-name "red" 0.10)
;;     "#ffff00000000"
;;
;; M-: (color-desaturate-name "red" 0.10)
;;     "#ffde00200020"
;;

;; Interactive Commands
;; ====================
;;
;;
;;

;; `completing-read'
;;  ===============
;;
;;     (completing-read `PROMPT' `COLLECTION'
;;            &optional `PREDICATE' `REQUIRE-MATCH' `INITIAL-INPUT' `HIST' DEF INHERIT-INPUT-METHOD)
;;
;; Read a string in the minibuffer, with completion.
;;
;; `PROMPT' is a string to prompt with; normally it ends in a colon and a space.
;; e.g.
;;     "Symbol: "
;;
;; `COLLECTION' can be:
;; - a list of strings,
;; - an alist,
;; - an obarray,
;; - a hash table,
;; - a function that performs (HOW?) the completion itself.
;;
;; `PREDICATE'
;;
;; `REQUIRE-MATCH'
;;
;; `INITIAL-INPUT'
;;
;; `HIST'
;;
;;
;;
;;
;;
;;
;;
;;

;; List Major Modes:
;;
;; You can view a list of major mode names by Alt+x `describe-variable' on `auto-mode-alist':
;;
;;     (describe-variable 'auto-mode-alist)
;;

;; `kill-ring-save':
;;
;;   (kill-ring-save BEG END &optional REGION)
;;
;; Save the region as if killed, but don’t kill it.
;; In Transient Mark mode, deactivate the mark.
;; If ‘interprogram-cut-function’ is non-nil, also save the text for a window
;; system cut and paste.
;;

;; list-matching-lines
;;
;; ^ M-x `list-matching-lines' shows all lines in a buffer that match some regexp.

;; See:
;;    - http://www.wilkesley.org/~ian/xah/emacs/emacs_open_file_path_fast.html
;;    -

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;;
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-commands)

;;; sboo-commands.el ends here