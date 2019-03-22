;;----------------------------------------------;;
;; Custom `interactive' commands.
;;
;; (This `feature' is "pure" (it has declarations only, no settings)).
;;
;; Most commands are namespaced under `sboo-` or `xah-`
;; (as are their non-`interactive' utilities).
;; 
;; See my `define-graceful-command` macro, for conveniently defining commands with fallbacks
;; (when external packages haven't been installed and/or can't be loaded).
;;
;;----------------------------------------------;;
;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;----------------------------------------------;;

;; builtins:

(require 'cl-lib)
(require 'shell)

;; personal:

(require 'sboo-conditions)
(require 'sboo-utilities)

;;----------------------------------------------;;
;; Utilities: Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(cl-defmacro define-graceful-boolean-command 

  ( NAME
    &key internal external doc
  )

  `(defun ,NAME (PrefixArgument)
     
     ,doc
     
     (interactive "P")
     
     (let ((*command* (function ,external)))

        (if (commandp *command*)

          (call-interactively *command*)

          (call-interactively (function ,internal) PrefixArgument)))))


;; ^ `defalias' for commands with graceful degradation.
;;
;; Wraps `defun' and `call-interactively'.
;;
;; See:
;; 
;; - https://stackoverflow.com/questions/37531124/emacs-how-to-use-call-interactively-with-parameter
;; - 
;; 

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

  "
"

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filesystem Navigation.
;;----------------------------------------------;;

(defun sboo-find-file ()

  "Wraps `ffap' ("find file at point").

  TODO handle "dir/dir/file.ext:line:column"
  "
  
  (interactive)

  (if (commandp #'ffap)

      (call-interactively #'ffap)

    (call-interactively #'find-file)))

;;----------------------------------------------;;
;; Text Navigation.
;;----------------------------------------------;;


(defun xah-search-current-word ()

  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)

  (let ( $p1 $p2 )

    (if (use-region-p)

        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))

      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9") ;TODO dont use region if only whitespace
        (setq $p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq $p2 (point))))

    (setq mark-active nil)

    (when (< $p1 (point))
      (goto-char $p1))

    (isearch-mode t)

    (isearch-yank-string (buffer-substring-no-properties $p1 $p2))))

;;----------------------------------------------;;
;; Projectile.
;;----------------------------------------------;;

(defun sboo-projectile-find-file ()
  (interactive)
  (projectile-find-file))
  ;;OLD (projectile-find-file (make-hash-table))

;;----------------------------------------------;;

;; (defun sboo-projectile-grep ()
;;   (interactive)
;;   (projectile-grep))

;;----------------------------------------------;;
;; Macros
;;----------------------------------------------;;

(defun sboo-kmacro-insert-counter-letter ()

  "Inserts a,b,c(,...) when `kmacro-counter' is 0,1,2,(,...)."
  (interactive)

  (progn
    (insert (make-string 1 (+ ?a kmacro-counter)))
    (kmacro-add-counter 1)))

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


;;==============================================;;

;;TODO https://stackoverflow.com/questions/22434484/emacs-modes-highlighting-uses-of-variable-under-point


;;==============================================;;

(defvar sboo-comment-character-default

  ?#

  "The default comment character.")

;;----------------------------------------------;;

(defvar sboo-comment-infix-character-default

  ?-

  "The default comment character.")

;;----------------------------------------------;;

(defvar sboo-comment-prefix-string-default

  "#"

  "The default comment character.")

;;----------------------------------------------;;

(defvar sboo-comment-length-default

  50

  "How long comments should be.")

;;----------------------------------------------;;

(defvar sboo-comment-h1-default

  "##################################################"

  "The default comment dividor.")

;;----------------------------------------------;;

(defvar sboo-comment-h2-default

  "#------------------------------------------------#"

  "The default comment dividor.")

;;----------------------------------------------;;

(defvar sboo-comment-character-alist

  '(

    (haskell-mode    . ?\-)
    (emacs-lisp-mode . ?\;)
    (nix-mode        . ?\#)
    (bash-mode       . ?\#)
    (python-mode     . ?\#)

    )

  "Comment characters per `major-mode'.")

;;----------------------------------------------;;

(defvar sboo-comment-length-alist

  '(
    )

  "Comment length per `major-mode'.")

;;----------------------------------------------;;

(defvar sboo-comment-infix-character-alist

  '(

    (haskell-mode    . ?\-)
    (emacs-lisp-mode . ?\-)
    (nix-mode        . ?\-)
    (bash-mode       . ?\-)
    (python-mode     . ?\-)

    )

  "Comment characters per `major-mode'.")

;;----------------------------------------------;;

(defvar sboo-comment-prefix-string-alist

  '(

    (haskell-mode    . "-- ")
    (emacs-lisp-mode . ";;")
    (nix-mode        . "#")
    (bash-mode       . "#")
    (python-mode     . "#")

    )

  "Comment characters per `major-mode'.")

;;----------------------------------------------;;

(defvar sboo-comment-suffix-string-alist

  '(

    (haskell-mode    . " --")
    (emacs-lisp-mode . ";;")
    (nix-mode        . "#")
    (bash-mode       . "#")
    (python-mode     . "#")

    )

  "Comment characters per `major-mode'.")

;;----------------------------------------------;;

(defvar sboo-comment-h1-alist

  '(

    (haskell-mode    . "-- ============================================ --")
    (emacs-lisp-mode . ";;==============================================;;")
    (nix-mode        . "##################################################")
    (bash-mode       . "##################################################")
    (python-mode     . "##################################################")

    )

  "Comment dividors per `major-mode'.")

;;----------------------------------------------;;

(defvar sboo-comment-h2-alist

  '(

    (haskell-mode    . "--------------------------------------------------")
    (emacs-lisp-mode . ";;----------------------------------------------;;")
    (nix-mode        . "#------------------------------------------------#")
    (bash-mode       . "#------------------------------------------------#")
    (python-mode     . "#------------------------------------------------#")

    )

  "Comment dividors per `major-mode'.")

;;----------------------------------------------;;

(defun sboo-comment-length ()

  "Lookup `sboo-comment-length-alist', defaulting to `sboo-comment-length-default'."

  (interactive)

  (let* ((MODAL-LENGTH (alist-get major-mode sboo-comment-length-alist))
         (LENGTH       (if MODAL-LENGTH
                           MODAL-LENGTH
                         sboo-comment-length-default))
         )
    LENGTH))

;;----------------------------------------------;;

(defun sboo-comment-infix-character ()

  "Lookup `sboo-comment-infix-character-alist', defaulting to `sboo-comment-infix-character-default'."

  (interactive)

  (let* ((MODAL-CHAR (alist-get major-mode sboo-comment-infix-character-alist))
         (CHAR       (if MODAL-CHAR
                         MODAL-CHAR
                       sboo-comment-infix-character-default))
         )
    CHAR))

;;----------------------------------------------;;

(defun sboo-comment-prefix-string ()

  "Lookup `sboo-comment-prefix-string-alist', defaulting to `sboo-comment-prefix-string-default'."

  (interactive)

  (let* ((MODAL-STRING (alist-get major-mode sboo-comment-prefix-string-alist))
         (STRING       (if MODAL-STRING
                         MODAL-STRING
                       sboo-comment-prefix-string-default))
         )
    STRING))

;;----------------------------------------------;;

(defun sboo-comment-suffix-string ()

  "Lookup `sboo-comment-suffix-string-alist', defaulting to `sboo-comment-suffix-string-default'."

  (interactive)

  (let* ((MODAL-STRING (alist-get major-mode sboo-comment-suffix-string-alist))
         (STRING       (if MODAL-STRING
                         MODAL-STRING
                       sboo-comment-suffix-string-default))
         )
    STRING))

;;----------------------------------------------;;

(defun sboo-insert-comment-header (text)

  "Insert a comment section header labeled TEXT.

TEXT gets padded to `sboo-comment-length-default'.

Related:

• `sboo-insert-comment-h2'"

  (interactive (list
                (read-string "Text: ")
                ))

  (let* ((MODAL-COMMENT (alist-get major-mode sboo-comment-h2-alist))
         (COMMENT       (if MODAL-COMMENT
                            MODAL-COMMENT
                          sboo-comment-h2-default))

         (LENGTH         (sboo-comment-length))
         (PREFIX         (sboo-comment-prefix-string))
         (SUFFIX         (sboo-comment-suffix-string))
         (PADDING-CHAR   (sboo-comment-infix-character))

         (TEXT          (if (fboundp #'s-capitalize) ;TODO; customizeable sboo-capitalize-string
                            (s-capitalize text)
                          text))
         (TEXT-PREFIX   (concat PREFIX " " TEXT " "))
         (LINE          (concat (truncate-string-to-width TEXT-PREFIX
                                                          (- LENGTH (length SUFFIX))
                                                          nil
                                                          PADDING-CHAR
                                                          (char-to-string PADDING-CHAR))
                                SUFFIX))
         )

    (progn
      (insert COMMENT "\n")
      (insert LINE    "\n")
      (insert COMMENT "\n")
      ())))

;; ^ NOTES:
;;
;; (truncate-string-to-width STR END-COLUMN &optional START-COLUMN PADDING ELLIPSIS)
;;
;; e.g.:
;;
;; M-: (truncate-string-to-width "-- Example " (- 24 1) nil ?- "---")
;;   ⇒ "-- Example ------------"
;;
;; M-: (string-width "\n")
;;   ⇒ 0
;;
;; M-: (string-bytes "\x100")
;;   ⇒ 2
;;
;; M-: (length "\n")
;;   ⇒ 1
;;
;; 
;;

;;----------------------------------------------;;

(defun sboo-insert-comment-h1 ()

  "Insert a (« <h1> »-style) comment dividor.

Related:

• `sboo-comment-h1-alist'
• `sboo-comment-h1-default'"

  (interactive)

  (let* ((COMMENT (alist-get major-mode sboo-comment-h1-alist))
        )

    (if COMMENT
        (insert COMMENT)
      (insert sboo-comment-h1-default))

    (insert "\n")))

;;----------------------------------------------;;

(defun sboo-insert-comment-h2 ()

  "Insert a (« <h2> »-style) comment dividor.

Related:

• `sboo-comment-h2-alist'
• `sboo-comment-h2-default'"

  (interactive)

  (let* ((COMMENT (alist-get major-mode sboo-comment-h2-alist))
        )

    (if COMMENT
        (insert COMMENT)
      (insert sboo-comment-h2-default))

    (insert "\n")))

;;==============================================;;

;;----------------------------------------------;;

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

(cl-defun sboo-read-feature (&key prompt require-match initial-input)

  "Read a `featurep' symbol.

Related:

• `features'"

  (interactive)

  (let ((PROMPT (format "%s: "
                        (or prompt "Feature")))

        (REQUIRE-MATCH (or require-match t))
        (INITIAL-INPUT (or initial-input "sboo-"))
        (CANDIDATES features)
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

;;TODO sboo-read-environment-variable read-envvar-name

;;TODO emacs copy a lisp expression to the clipboard

;; (kill-new 
;; https://stackoverflow.com/questions/2178850/how-to-copy-to-clipboard-in-emacs-lisp

;TODO few core buffers like home.nix and emacs.md
;TODO known dirs like emacs, config, haskell, notes

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
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

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

;; list-matching-lines
;;
;; ^ M-x `list-matching-lines' shows all lines in a buffer that match some regexp.

;; See:
;;    - http://www.wilkesley.org/~ian/xah/emacs/emacs_open_file_path_fast.html
;;    - 

;;==============================================;;
(provide 'sboo-commands)