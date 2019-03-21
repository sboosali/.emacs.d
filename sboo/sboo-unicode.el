;; -*- lexical-binding: t; -*-

(require 'cl)     ;; "CommonLisp"
(require 'subr-x) ;; "SUBRoutine-eXtRAS"
(require 'mule)   ;; "MUltiLingual Environment"

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-ucs-names

  nil

  "List of Unicode Character names.

Examples:

• M-: (nth 891 sboo-ucs-names)
    ⇒ \"LATIN SMALL LETTER ALPHA\"

Accessed by Function `sboo-ucs-names'.")

;;----------------------------------------------;;

(defvar sboo-ucs-names-with-namesake-character

  nil

  "List of Unicode Character names.

Examples:

• M-: (nth 891 sboo-ucs-names-with-namesake-character)
    ⇒ \"ɑ LATIN SMALL LETTER ALPHA\"

Displayed by Function `sboo-read-character-name'.

Like `sboo-ucs-names', but each name is prefixed by the namesake character (plus a space).")

;;----------------------------------------------;;

(defcustom sboo-unicode-display-namesake-default

  t

  "DISPLAY-NAMESAKE argument of `sboo-read-character-by-name'.

Whether `sboo-read-character-by-name' displays, for example:

• \"LATIN SMALL LETTER ALPHA\"   (if nil)
• \"ɑ LATIN SMALL LETTER ALPHA\" (if t)

a `booleanp'."

  :type '(boolean)

  :safe  t
  :group 'sboo)

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-ucs-names (&key display-namesake)

  "Return all Unicode Character names.

Output:

• a `listp' of `stringp's.

Examples:

• M-: (sboo-ucs-names)
    ⇒ (\"NULL\" ... \"BULLET\" ...)

• M-: (sboo-ucs-names :display-namesake t)
    ⇒ (\"  NULL\" ... \"• BULLET\" ...)

Related:

• `ucs-names'

Notes:

• `ucs-names' is a `hash-table-p' on Emacs≥26 and an alist on Emacs≤25."

  (progn

    ;; Initialize:

    (unless (and sboo-ucs-names
                 sboo-ucs-names-with-namesake-character)

      (let* ((OBJECT (ucs-names))
             (TYPE   (type-of OBJECT))
             (NAMES  (pcase TYPE
                       ('hash-table (hash-table-keys OBJECT))
                       ('list       (mapcar #'car OBJECT))
                       (_           OBJECT)))
             )

        (progn

          (unless sboo-ucs-names
            (setq sboo-ucs-names NAMES))

          (unless sboo-ucs-names-with-namesake-character
            (setq sboo-ucs-names-with-namesake-character
                  (mapcar #'sboo-prefix-namesake-character NAMES))))))

    ;; Access:

    (if (bound-and-true-p display-namesake)
        sboo-ucs-names-with-namesake-character
      sboo-ucs-names)))

;;----------------------------------------------;;

(defun sboo-ucs-names-get (name)

  "Get a character by name.

Inputs:

• NAME — a `stringp'.

Output:

• an `integerp'.

Example:

• M-: (format \"%c\" (sboo-ucs-names-get \"BULLET\"))
    ⇒ \"•\"
"

  (let* ((NAME   (upcase name))
         (OBJECT (ucs-names))
         (TYPE   (type-of OBJECT))
         (CHAR   (pcase TYPE
                   ('hash-table (gethash NAME OBJECT))
                   ('list       (alist-get NAME OBJECT nil nil #'equal))
                   (_           nil)))
         )

    CHAR))

;; ^ (format "%c" (sboo-ucs-names-get "BULLET"))

;;----------------------------------------------;;

(defun sboo-prefix-namesake-character (name)

  "Prefix NAME with the char it names.

Inputs:

• NAME — a `stringp'. The name of the unicode character.

Output:

• a `stringp'. 
  the output `string-width' will always be exactly two more than the input's.

Examples:

    M-: (sboo-prefix-namesake-character \"BULLET\")
      ⇒ \"• BULLET\"

    M-: (sboo-prefix-namesake-character \"NULL\")
      ⇒ \"  NULL\"

    M-: (equal (substring (sboo-prefix-namesake-character \"NULL\") 2) \"NULL\")
      ⇒ t

    M-: (- (string-width (sboo-prefix-namesake-character \"NULL\")) (string-width \"NULL\"))
      ⇒ 2"

  (let* ((CHAR   (sboo-get-character-by-name name))
         (STRING (if CHAR
                     (char-to-string CHAR)
                   ""))
         (WIDTH  (string-width STRING))
         (PREFIX (if (and STRING (= 1 WIDTH))
                     (concat STRING " ")
                   "  "))
         )

    (concat PREFIX name)))

;; (sboo-prefix-namesake-character "BULLET")

;;----------------------------------------------;;

(defun sboo-strip-namesake-character (string)

  "Invert `sboo-prefix-namesake-character'.

Inputs:

• STRING — a `stringp'.

Output:

• a `stringp'. 

Examples:

    M-: (sboo-strip-namesake-character (sboo-prefix-namesake-character \"BULLET\"))
      ⇒ \"BULLET\""

    (substring string 2))

;;----------------------------------------------;;

(defun sboo-get-char-name (char)

  "Get the Unicode Character Database « 'name » of CHAR.

Inputs:

• CHAR — a character (an `integerp').

Examples:

    M-: (call-interactively #'sboo-get-char-name)
    Character: c
    ⇒ \"LATIN SMALL LETTER C\"

Related:

• `get-char-code-property'."

  (interactive (list (read-char-exclusive "Character (press a key): ")))

  (get-char-code-property char 'name))

;;----------------------------------------------;;

(defun sboo-get-unicode-names-list ()

  "Construct `sboo-unicode-names-list'.

Via `ucs-names'."

  (unless ucs-names
    (ucs-names))
  ;; ^ Initialize if uninitialized.

  (hash-table-keys ucs-names))

  ;; (let ((NAMES '()))

  ;;   (progn
  ;;     (cl-flet ((ADD-CHAR-NAME (CHAR PROPERTIES)
  ;;                              (add-to-list 'NAMES (aget PROPERTIES 'name)))) ;TODO

  ;;       (map-char-table #'ADD-CHAR-NAME
  ;;                       char-code-property-table))

  ;;     NAMES)))

;;----------------------------------------------;;

(defun sboo-get-unicode-names-hash-table ()

  "Construct `sboo-unicode-names-hash-table'.

Via `ucs-names'."

  (unless ucs-names
    (ucs-names))
  ;; ^ Initialize if uninitialized.

  ucs-names)

;;----------------------------------------------;;

(defun sboo-get-unicode-completion-description-hash-table ()

  "Construct `sboo-unicode-completion-descriptions-hash-table'.

Via `ucs-names'."

  (unless ucs-names
    (ucs-names))
  ;; ^ Initialize if uninitialized.

  ucs-names

  )

;;----------------------------------------------;;
;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;----------------------------------------------;;

;;;###autoload
(defvar sboo-unicode-names-list (sboo-get-unicode-names-list)

  "The list of each Unicode character's (unique) `name'.

Haskell Type « :: [String] ».

[TODO ordered list?]

[TODO « Map Char String »]

e.g...

    M-: (consp (member \"BULLET\" sboo-unicode-names-list))
    t

")

;;----------------------------------------------;;

;;;###autoload
(defvar sboo-unicode-names-hash-table (sboo-get-unicode-names-hash-table)

  "The mapping from each Unicode character `name' to its character.

Equals `ucs-names'.

Haskell Type « :: Map Char String ».

Examples:

    M-: (gethash \"BULLET\" sboo-unicode-names-hash-table)
    ?•
")

;; M-: (gethash "BULLET" ucs-names)

;;----------------------------------------------;;

;;;###autoload
(defvar sboo-unicode-completion-descriptions-hash-table (sboo-get-unicode-completion-description-hash-table)

  "The mapping from each Unicode character `name' to a description thereof.

This description combines the name of the character with said character being named.
In particular, each table-key is a Unicode Character Codepoint (the table-value) 
prepended to its Unicode Character Name (with a space).
For example, one entry is:

    (:key   \"• BULLET\"
     :value ?•
    )

Provides caching for completion.

NOTE Why does this exist when `sboo-unicode-names-hash-table' does too? 
Because currently (circa 2018), `helm' doesn't support the `:annotation-function' property of `completion-extra-properties'.

Derived from `ucs-names'.

Haskell Type « :: Map String Char ».

Examples:

    M-: (gethash \"• BULLET\" sboo-unicode-completion-descriptions-hash-table)
    ?•

    M-: (gethash \"BULLET\" sboo-unicode-completion-descriptions-hash-table)
    nil

    M-: (gethash \"•\" sboo-unicode-completion-descriptions-hash-table)
    nil")

;; ^
;;     M-: (gethash "• BULLET" sboo-unicode-completion-descriptions-hash-table)
;;     ?•

;;----------------------------------------------;;

(defun sboo-read-character-name (&optional display-namesake)

  "Read a Unicode Character name.

Inputs:

• DISPLAY-NAMESAKE — a `booleanp'. 
                     Whether to display the unicode character itself (beside the name).

Output:

• a `stringp'.
  a key of `ucs-names'.

Type (Haskell):

• « :: IO String ».

Related:

• `ucs-names'."

  (interactive (list
                (if current-prefix-arg t nil)
                ))

  (let*  ((PROMPT        (format "%s: " "Unicode Character name"))
          (REQUIRE-MATCH t)
          (PREDICATE     nil)
          (CANDIDATES    (if display-namesake
                             sboo-ucs-names-with-namesake-character
                           sboo-ucs-names))
          )

    (let* ((STRING (completing-read PROMPT CANDIDATES PREDICATE REQUIRE-MATCH nil))
           (NAME   (if display-namesake
                       (sboo-strip-namesake-character STRING)
                     STRING))
           )

      NAME)))

;;----------------------------------------------;;

(defun sboo-read-character-by-name (&optional display-namesake)

  "Read a Unicode Character name, returning the corresponding Unicode Character.

Inputs:

• DISPLAY-NAMESAKE — a `booleanp'. 
                     Whether to display the unicode character itself (beside the name).

Output:

• an `integerp' (a character).
  a value of `ucs-names'.

Type (Haskell):

• « :: IO Char ».

Related:

• `ucs-names'.
• `read-character-by-name' — doesn't support fuzzy-matching."

  (interactive (list
                (if current-prefix-arg t nil)
                ))

  (let* ((STRING (sboo-read-character-name display-namesake))
         (CHAR   (sboo-ucs-names-get STRING))
         )

    CHAR))

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-read-character-name-via-collection ()

  "Read a Unicode character name, returning the string `NAME'.

Haskell Type « :: IO String ».

See `ucs-names'."

  (completing-read "Unicode character name: " sboo-unicode-names-list nil t))

;;----------------------------------------------;;

(defun sboo-annotate-character-name-with-character-literal (name)

  "Annotate `NAME' (e.g. \"BULLET\") with the char itself (e.g. ?•).

Haskell Type « :: String -> String ».
"
  ;TODO(throw 'sboo NAME)

  (let ((CHAR
         (gethash name sboo-unicode-names-hash-table)))

    (if CHAR
        (let ((PRINTABLE
               t)) ;;TODO check 'general-category for whether character is printable (or is control)

          (if PRINTABLE
              (let ((ANNOTATED
                     (format-message "%s  %s" (char-to-string CHAR) name)))
              ;; (let ((ANNOTATION
              ;;        (format-message " %s" (char-to-string CHAR))))

                ANNOTATED)
            name)))))

;;----------------------------------------------;;

(defun sboo-read-character-name-with-annotations ()

  "Read a Unicode character name, returning the string `NAME'.

Annotates each completion candidate with the unicode character being named.

Haskell Type « :: IO String ».

Calls `completing-read' with Info node ‘(elisp)Programmed Completion’"

  (let ((completion-extra-properties
         '(:annotation-function sboo-annotate-character-name-with-character-literal)))

    (completing-read "Unicode character name: " sboo-unicode-names-hash-table nil t)))

;; ^
;;     M-: (message (sboo-read-character-name-with-annotations))

;;----------------------------------------------;;

(defun sboo-read-character-with-literals-displayed ()

  "Read a Unicode character name, returning the string `NAME'.

Prepends each completion candidate with the unicode character being named
(which must then be stripped back out).

Haskell Type « :: IO String ».

Calls `completing-read' with ‘(elisp)Programmed Completion’"

  (let* ((COMPLETION
          (completing-read "Unicode character (char & name): " sboo-unicode-completion-descriptions-hash-table nil t))
         (CHAR
          (gethash COMPLETION sboo-unicode-completion-descriptions-hash-table)))

    CHAR))

;; ^
;;     M-: (message (sboo-read-character-with-literals-displayed))

;;----------------------------------------------;;

(cl-defun sboo-read-character (&key method display)

  "Read a Unicode character name, returning the string `NAME'.

Inputs:

• NAME    — a `stringp'. The name of the unicode character.
• DISPLAY — a `booleanp'. Whether to display the unicode character itself (beside the name).

Wraps `sboo-read-character-with-literals-displayed', `sboo-read-character-name-with-annotations', `sboo-read-character-name-via-collection'."

  (let ((CHAR-READER
         (if METHOD METHOD #'sboo-read-character-with-literals-displayed))) ;TODO shorter aliases (than full names) in keywords).

    (call-interactively CHAR-READER)))

;;TODO function (not command) alias?
;; (defalias sboo-read-character-name #'sboo-read-character-name-with-annotations)

;;----------------------------------------------;;

(cl-defun sboo-get-character-by-name (name)

  "Return the Unicode character whose name is `NAME'.

Inputs:

• NAME — a `stringp'. The name of a Unicode Character.

Type (Haskell):

• « :: String -> Maybe Char »

Examples:

    M-: (sboo-get-character-by-name \"BULLET\")
    ?•

    M-: (sboo-get-character-by-name \"NOT A UNINCODE CHARACTER NAME\")
    nil

See `sboo-unicode-names-hash-table'."

  (interactive (list
                (sboo-read-character-name-with-annotations)))

  (let ((CHAR (gethash name ucs-names)))

    CHAR))

;;----------------------------------------------;;

(defun sboo-insert-character-by-name (name)

  "Insert the Unicode Character named `NAME'.

Inputs:

• NAME — a `stringp'. The name of a Unicode Character.

Type (Haskell):

• « :: String -> IO () ».

Notes:

• `sboo-insert-character-by-name' is like `insert-char', but:

    • its completion is more flexible (for example, `helm' can be configured to efficiently fuzzily-match)
    • it displays the literal unicode character itself alongside each name (when `sboo-unicode-display-namesake-default' is non-nil).

Related:

• `sboo-unicode-display-namesake-default'.
• `sboo-read-character-name'.
• `sboo-ucs-names-get'."

  (interactive (list (sboo-read-character-name sboo-unicode-display-namesake-default)
                     ))

  (let ((CHAR (sboo-ucs-names-get name)))

    (insert-char CHAR)))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;;; DOCS ‘ucs-names’: [TODO]
;;
;; « ucs-names » :: « HashMap String Char »
;;
;;     M-: (type-of ucs-names)
;;     'hash-table
;;
;; Examples:
;;
;;     M-: (gethash "BULLET" ucs-names)
;;     8226
;;     
;;     M-: '(?•)
;;     '(8226)
;;
;;     M-: (type-of (gethash "BULLET" ucs-names))
;;     'integer
;; 
;;     M-: (member "BULLET" (hash-table-keys ucs-names))
;;     t
;;

;;; DOCS ‘get-char-code-property’:
;;
;; « (get-char-code-property CHAR PROPNAME) »
;;
;; > M-: (get-char-code-property ?• 'name)
;; > "BULLET"
;; > 
;; > M-: (type-of (get-char-code-property ?• 'name))
;; > 'string
;; > 
;; > M-: (type-of char-code-property-table)
;; > 'char-table
;;
;; NOTE a char-table is sparse and compact, so `aref' doesn't quite work: [TODO why not?]
;;
;;     M-: (aref char-code-property-table ?•)
;;     nil
;;
;;     M-: (char-table-range char-code-property-table ?•)
;;     nil
;; 
;;     M-: 
;;     
;;
;;

;;; DOCS `map-char-table':
;;
;; « (map-char-table FUNCTION CHAR-TABLE) »
;;
;; > This function calls its argument function for each element of char-table that has a non-nil value. The call to function is with two arguments, a key and a value. The key is a possible range argument for char-table-range—either a valid character or a cons cell (from . to), specifying a range of characters that share the same value. The value is what (char-table-range char-table key) returns.
;;
;; NOTE:
;; > Standard mapping functions like `mapcar` do not allow char-tables because a char-table is a sparse array whose nominal range of indices is very large.
;;

;;; DOCS `mapcar':
;;
;; « (mapcar FUNCTION SEQUENCE) »
;;
;; > Apply FUNCTION to each element of SEQUENCE, and make a list of the results.
;; > The result is a list just as long as SEQUENCE.
;; > SEQUENCE may be a list, a vector, a bool-vector, or a string.
;;

;;; DOCS `insert-char':
;;
;; > DEFUN ("insert-char", Finsert_char, Sinsert_char, 1, 3,
;; >        "(list (read-char-by-name \"Insert character (Unicode name or hex): \")\
;; >               (prefix-numeric-value current-prefix-arg)\
;; >               t))",
;; >        doc: /* Insert COUNT copies of CHARACTER.
;; > Interactively, prompt for CHARACTER.  You can specify CHARACTER in one
;; > of these ways:
;; >
;; >  - As its Unicode character name, e.g. \"LATIN SMALL LETTER A\".
;; >    Completion is available; if you type a substring of the name
;; >    preceded by an asterisk `*', Emacs shows all names which include
;; >    that substring, not necessarily at the beginning of the name.
;; >
;; >  - As a hexadecimal code point, e.g. 263A.  Note that code points in
;; >    Emacs are equivalent to Unicode up to 10FFFF (which is the limit of
;; >    the Unicode code space).
;; > 
;; >  - As a code point with a radix specified with #, e.g. #o21430
;; >    (octal), #x2318 (hex), or #10r8984 (decimal).
;; >
;; > If called interactively, COUNT is given by the prefix argument.  If
;; > omitted or nil, it defaults to 1.
;; > 

;; NOTE `completion-extra-properties':
;;
;; * :annotation-function
;; * :exit-function
;;
;; 

;;; DOCS `cl-flet':
;;
;; e.g.
;;
;;    (require 'cl)
;;    (cl-flet ((f (x) (* x x)))
;;      (f 7))
;;

;;; DOCS `char-to-string':
;;
;; e.g.
;;
;;    M-: (char-to-string ?•)
;;    "•"

;;; See:
;;
;; ./share/emacs/26.1/lisp/international/uni-name.el 
;; 

;;----------------------------------------------;;
(provide 'sboo-unicode)