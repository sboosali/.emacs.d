;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; Inserting comments.
;; 
;; • 
;; • 
;;
;; Commands include:
;;
;; • `sboo-comment-insert-h1'
;; • `sboo-comment-insert-h2'
;; • `sboo-comment-insert-header'
;; 
;; Customizeable variables include:
;;
;; • `sboo-comment-character-default'
;; • `sboo-comment-length-default'
;; • `sboo-comment-prefix-string-alist'
;; • `sboo-comment-infix-character-alist'
;; • `sboo-comment-suffix-string-alist'
;; • `sboo-comment-'
;;
;; Related variables include:
;;
;; • `comment-start'
;;

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(require 'cl-lib)
(require 'pcase)

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-comment

  nil

  "Insertion of (padded, aligned) comment headers."

  :prefix "comment-"
  :group 'fill)

;;==============================================;;

(defcustom sboo-comment-default-prefix-char

  ?#

  "The default comment `characterp'."

  :type '(character :tag "Character")

  :safe #'characterp
  :group 'sboo-comment)

;;----------------------------------------------;;

(defcustom sboo-comment-default-h1-infix-char

  ?=

  "The default `characterp' in “h1” comments."

  :type '(character :tag "Character")

  :safe #'characterp
  :group 'sboo-comment)

;;----------------------------------------------;;

(defcustom sboo-comment-default-h2-infix-char

  ?-

  "The default `characterp' in “h2” comments."

  :type '(character :tag "Character")

  :safe #'characterp
  :group 'sboo-comment)

;;----------------------------------------------;;

(defvar sboo-comment-prefix-string-default

  "#"

  "The default comment character(s) at the start of a comment.")

;;----------------------------------------------;;

(defvar sboo-comment-suffix-string-default

  "#"

  "The default comment character(s) at the end of a comment.")

;;----------------------------------------------;;

(defvar sboo-comment-length-default

  50

  "How long comments should be.")

;;----------------------------------------------;;

(defvar sboo-comment-h1-default

  "##################################################"

  "The default comment divider.")

;;----------------------------------------------;;

(defvar sboo-comment-h2-default

  "#------------------------------------------------#"

  "The default comment divider.")

;;----------------------------------------------;;

(defvar sboo-comment-character-alist

  `(

    (lisp-mode             . ?\;)
    (emacs-lisp-mode       . lisp-mode)
    (scheme-mode           . lisp-mode)
    (lisp-interaction-mode . lisp-mode)

    (haskell-mode       . ?\-)
    (haskell-cabal-mode . haskell-mode)
    (cabal-mode         . haskell-mode)
    (cabal-project-mode . haskell-mode)

    (html-mode          . ?-)
    (mhtml-mode         . html-mode)
    (markdown-mode      . html-mode)
    (gfm-mode           . html-mode)

    (nix-mode           . ?\#)
    (bash-mode          . ?\#)
    (python-mode        . ?\#)
    (css-mode           . ?*)
    )

  "Comment characters per `major-mode'.

Defaults to `sboo-comment-default-prefix-char'.")

;;----------------------------------------------;;

(defvar sboo-comment-length-alist

  '(
    )

  "Comment length per `major-mode'.

Defaults to `sboo-comment-length-default'.")

;;----------------------------------------------;;

(defvar sboo-comment-infix-character-alist

  '(

    (lisp-mode             . ?\-)
    (emacs-lisp-mode       . lisp-mode)
    (scheme-mode           . lisp-mode)
    (lisp-interaction-mode . lisp-mode)

    (haskell-mode       . ?\-)
    (haskell-cabal-mode . haskell-mode)
    (cabal-mode         . haskell-mode)
    (cabal-project-mode . haskell-mode)

    (html-mode          . ?-)
    (mhtml-mode         . html-mode)
    (markdown-mode      . html-mode)
    (gfm-mode           . html-mode)

    (nix-mode           . ?\-)
    (bash-mode          . ?\-)
    (python-mode        . ?\-)
    (css-mode           . ?\-)
    (sql-mode           . ?\-)

    )

  "Comment characters per `major-mode'.

Defaults to `sboo-comment-default-h1-infix-char'.")

;;----------------------------------------------;;

(defvar sboo-comment-prefix-string-alist

  '(

    (lisp-mode             . ";;")
    (emacs-lisp-mode       . lisp-mode)
    (scheme-mode           . lisp-mode)
    (lisp-interaction-mode . lisp-mode)

    (haskell-mode       . "--")
    (haskell-cabal-mode . haskell-mode)
    (cabal-mode         . haskell-mode)
    (cabal-project-mode . haskell-mode)

    (html-mode          . "<!--")
    (mhtml-mode         . html-mode)
    (markdown-mode      . html-mode)
    (gfm-mode           . html-mode)

    (nix-mode           . "#")
    (bash-mode          . "#")
    (python-mode        . "#")
    (css-mode           . "/*")
    (xmodmap-mode       . "!")
    (sql-mode           . "--")

    )

  "Comment characters per `major-mode'.

Defaults to `sboo-comment-prefix-string-default'.")

;;----------------------------------------------;;

(defvar sboo-comment-suffix-string-alist

  '(

    (lisp-mode             . ";;")
    (emacs-lisp-mode       . lisp-mode)
    (scheme-mode           . lisp-mode)
    (lisp-interaction-mode . lisp-mode)

    (haskell-mode       . "--")
    (haskell-cabal-mode . haskell-mode)
    (cabal-mode         . haskell-mode)
    (cabal-project-mode . haskell-mode)

    (html-mode          . "-->")
    (mhtml-mode         . html-mode)
    (markdown-mode      . html-mode)
    (gfm-mode           . html-mode)

    (nix-mode           . "#")
    (bash-mode          . "#")
    (python-mode        . "#")
    (css-mode           . "*/")
    (xmodmap-mode       . "!")
    (sql-mode           . "--")

    )

  "Comment characters per `major-mode'.

Defaults to `sboo-comment-suffix-string-default'.")

;;----------------------------------------------;;

(defvar sboo-comment-h1-alist

  '(

    (lisp-mode             . ";;==============================================;;")
    (emacs-lisp-mode       . lisp-mode)
    (scheme-mode           . lisp-mode)
    (lisp-interaction-mode . lisp-mode)

    (haskell-mode          . "-- ============================================ --")
    (haskell-cabal-mode    . haskell-mode)
    (cabal-mode            . haskell-mode)
    (cabal-project-mode    . haskell-mode)

    (html-mode             . "<!-- ========================================= -->")
    (mhtml-mode            . html-mode)
    (markdown-mode         . html-mode)
    (gfm-mode              . html-mode)

    (nix-mode              . "##################################################")
    (bash-mode             . "##################################################")
    (python-mode           . "##################################################")
    (css-mode              . "/************************************************/")
    (xmodmap-mode          . "! ============================================== !")

    )

  "Comment dividors per `major-mode'.

Defaults to `sboo-comment-h1-default'.")

;; TODO either Implement aliases for modes, or use inheritance for derived modes. e.g.:
;;
;; '(lisp-mode . (emacs-lisp-mode scheme-mode lisp-interaction-mode))
;; '(html-mode . (mhtml-mode))

;;----------------------------------------------;;

(defvar sboo-comment-h2-alist

  '(

    (lisp-mode             . ";;----------------------------------------------;;")
    (emacs-lisp-mode       . lisp-mode)
    (scheme-mode           . lisp-mode)
    (lisp-interaction-mode . lisp-mode)

    (haskell-mode          . "--------------------------------------------------")
    (haskell-cabal-mode    . haskell-mode)
    (cabal-mode            . haskell-mode)
    (cabal-project-mode    . haskell-mode)

    (html-mode             . "<!----------------------------------------------->")
    (mhtml-mode            . html-mode)
    (markdown-mode         . html-mode)
    (gfm-mode              . html-mode)

    (nix-mode              . "#------------------------------------------------#")
    (bash-mode             . "#------------------------------------------------#")
    (python-mode           . "#------------------------------------------------#")
    (css-mode              . "/* -------------------------------------------- */")
    (xmodmap-mode          . "! ---------------------------------------------- !")
    (sql-mode              . "--------------------------------------------------")

    )

  "Comment dividors per `major-mode'.

Defaults to `sboo-comment-h2-default'.")

;;----------------------------------------------;;

(defvar sboo-comment-history-list

  nil

  "History for the `sboo-comment-read-string' command.")

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-comment-length ()

  "Lookup `sboo-comment-length-alist', defaulting to `sboo-comment-length-default'."

  (interactive)

  (let* ((LENGTH (sboo-comment-alist-get sboo-comment-length-alist :mode major-mode :default sboo-comment-length-default))
         )
    LENGTH))

;;----------------------------------------------;;

(defun sboo-comment-h1-infix-char ()

  "Lookup `sboo-comment-infix-character-alist', defaulting to `sboo-comment-default-h1-infix-char'."

  (interactive)

  (let* ((CHAR (sboo-comment-alist-get sboo-comment-infix-character-alist :mode major-mode :default sboo-comment-default-h1-infix-char))
         )
    CHAR))

;;----------------------------------------------;;

(defun sboo-comment-h2-infix-char ()

  "Lookup `sboo-comment-infix-character-alist', defaulting to `sboo-comment-default-h2-infix-char'."

  (interactive)

  (let* ((CHAR (sboo-comment-alist-get sboo-comment-infix-character-alist :mode major-mode :default sboo-comment-default-h2-infix-char))
         )
    CHAR))

;;----------------------------------------------;;

(defun sboo-comment-prefix-string ()

  "Lookup `sboo-comment-prefix-string-alist', defaulting to `sboo-comment-prefix-string-default'."

  (interactive)

  (let* ((STRING (sboo-comment-alist-get sboo-comment-prefix-string-alist :mode major-mode :default sboo-comment-prefix-string-default))
         )
    STRING))

;;----------------------------------------------;;

(defun sboo-comment-suffix-string ()

  "Lookup `sboo-comment-suffix-string-alist', defaulting to `sboo-comment-suffix-string-default'."

  (interactive)

  (let* ((STRING (sboo-comment-alist-get sboo-comment-suffix-string-alist :mode major-mode :default sboo-comment-suffix-string-default))
         )
    STRING))

;;----------------------------------------------;;

(cl-defun sboo-comment-alist-get (alist &key mode default)

  "Resolve a “value” in ALIST given the “key” MODE.

Inputs:

• ALIST — either an alist (i.e. `listp' of `consp's), or a (a `symbolp' whose `symbol-value' is an alist).
  each cons cell has a `car' of a `symbolp', and a `cdr' of either a `stringp's or a `symbolp'. 
• MODE — a `symbolp'. 
  should be a `major-mode'.

Ouput:

• a `stringp', `characterp', or `integerp'.
  (When given “alist”s besides this feature's variables (i.e. those named `sboo-comment-*-alist'), 
  this function may return `nil', or any non-`symbolp' type with a “value” in ALIST).

Example:

• M-: (sboo-comment-alist-get 'sboo-comment-character-alist)
    ⇒ 59

    ; (59 is the semicolon character)
    ; (assuming `major-mode' is `emacs-lisp-mode')

• M-: (sboo-comment-alist-get 'sboo-comment-prefix-string-alist :mode 'markdown-mode :default sboo-comment-prefix-string-default)
    ⇒ \"<!--\"

    ; (the “key” `markdown-mode' has “value” `html-mode', which itself has a `stringp' “value”.)"

  (let* ((MODE    (or mode major-mode))
         (DEFAULT (or default nil))
         (ALIST   (pcase (type-of alist)

                    ('symbol (symbol-value alist))
                    ('cons   alist)

                    (_ nil)))
         )

    (if (and (symbolp MODE)
             (consp ALIST))             ; `listp' matches `nil', `consp' doesn't.

        (let* ((VALUE-1 (alist-get MODE ALIST DEFAULT))
               (VALUE-2 (if (symbolp VALUE-1)
                            (alist-get VALUE-1 ALIST DEFAULT)
                          VALUE-1))
               )

          VALUE-2)

    DEFAULT)))

;; e.g.
;;
;; M-: (alist-get 'emacs-lisp-mode sboo-comment-character-alist :key-not-found)
;;

;;----------------------------------------------;;

(cl-defun sboo-comment-read-string (&key (prompt "Text") (initial nil) (history 'sboo-comment-history-list))

  "Feature-specific `read-string'.

Inputs:

• PROMPT — a `stringp'.
  Without a trailing space-plus-colon (i.e. “ :”).
• INITIAL — a `stringp'.
• HISTORY — a `symbolp'.
  Defaults to `sboo-comment-history-list'.
  Its `symbol-value' is: a `listp' of `stringp's.

Output:

• a `stringp'."

  (let* ((PROMPT (format "%s: " prompt))
         )

    (read-string PROMPT initial history)))

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-comment-insert-h1 ()

  "Insert a (« <h1> »-style) comment dividor.

Related:

• `sboo-comment-h1-alist'
• `sboo-comment-h1-default'"

  (interactive)

  (let* ((COMMENT (sboo-comment-alist-get sboo-comment-h1-alist :mode major-mode))
        )

    (if COMMENT
        (insert COMMENT)
      (insert sboo-comment-h1-default))

    (insert "\n")))

;;----------------------------------------------;;

(defun sboo-comment-insert-h2 ()

  "Insert a (« <h2> »-style) comment dividor.

Related:

• `sboo-comment-h2-alist'
• `sboo-comment-h2-default'"

  (interactive)

  (let* ((COMMENT (sboo-comment-alist-get sboo-comment-h2-alist :mode major-mode))
        )

    (if COMMENT
        (insert COMMENT)
      (insert sboo-comment-h2-default))

    (insert "\n")))

;;----------------------------------------------;;

(defun sboo-comment-insert-header (text)

  "Insert a comment section header labeled TEXT.

TEXT gets padded to `sboo-comment-length-default'.

Related:

• `sboo-comment-insert-h2'"

  (interactive (list
                (sboo-comment-read-string :prompt "Text")
                ))

  (let* ((COMMENT (sboo-comment-alist-get sboo-comment-h2-alist :mode major-mode :default sboo-comment-h2-default))

         (LENGTH         (sboo-comment-length))
         (PREFIX         (sboo-comment-prefix-string))
         (SUFFIX         (sboo-comment-suffix-string))
         (PADDING-CHAR   (sboo-comment-h1-infix-char))

         (TEXT          (sboo-comment--capitalize text))
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
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-comment--capitalize (text)

  "Capitalize each word in TEXT.

Inputs:

• TEXT — a `stringp'.

Output:

• a `stringp'.

Examples:

• M-: (sboo-comment--capitalize \"capitalize each word\")
    ⇒ \"Capitalize Each Word\"
• M-: (sboo-invert-color-string \"EOF\")
    ⇒ \"EOF\"
    ; not \"Eof\"

Behavior:

• Preserve Capitalization — if TEXT has any uppercase character, `sboo-comment--capitalize' is identity."

  ;; buffer-based capitalization:


  (if (let ((case-fold-search nil)
            )
        (if (string-match-p "[[:upper:]]" text) t nil))
      ;; "Are any of the letters in TEXT in upper case?"

      text

    (with-temp-buffer
      ;;TODO `subword-mode'?

      (insert text)

      (goto-char (point-min))
      (while (< (point) (point-max))
        (capitalize-word +1))

      (buffer-substring-no-properties (point-min) (point-max)))))

;; e.g.
;;
;; M-: (let ((case-fold-search nil)) (if (string-match-p "[[:upper:]]" "eof") t nil))
;; M-: (let ((case-fold-search nil)) (if (string-match-p "[[:upper:]]" "eOf") t nil))
;; M-: (let ((case-fold-search nil)) (if (string-match-p "[[:upper:]]" "EOF") t nil))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; `read-string':
;;
;; (read-string PROMPT &optional INITIAL-INPUT HISTORY DEFAULT-VALUE INHERIT-INPUT-METHOD)
;;

;;==============================================;;
(provide 'sboo-comment)