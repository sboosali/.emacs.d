;; -*- lexical-binding: t; -*-

(require 'cl)
(require 'subr-x)
(require 'mule)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-get-char-name (CHAR)

  "Get the Unicode Character Database `name' of the given character `CHAR'.

Via `get-char-code-property'."

  (interactive (list
                (read-char-exclusive)))

  (get-char-code-property CHAR 'name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-get-unicode-names-hash-table ()

  "Construct `sboo-unicode-names-hash-table'.

Via `ucs-names'."

  (unless ucs-names
    (ucs-names))
  ;; ^ Initialize if uninitialized.

  ucs-names)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defvar sboo-unicode-names-hash-table (sboo-get-unicode-names-hash-table)

  "The mapping from each Unicode character `name' to its character.

Equals `ucs-names'.

Haskell Type « :: Map Char String ».

Examples:

    M-: (gethash \"BULLET\" sboo-unicode-names-hash-table)
    ?•
")

;; (gethash "BULLET" ucs-names)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-read-character-name ()

  "Read a Unicode character name, returning the string `NAME'.

Haskell Type « :: IO String ».

See `ucs-names'."

  (completing-read "Unicode character name: " sboo-unicode-names-list nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-read-character-by-name ()

  "Read a Unicode character name, returning the char `CHAR'.

Haskell Type « :: IO Char ».

See `ucs-names'.

(NOTE `CHAR' is an `integerp'.)

Also see `read-character-by-name', which doesn't support fuzzy-matching."

  (completing-read "Unicode character name: " sboo-unicode-names-hash-table nil t)) ;TODO convert hash-table to alist? completion ignores values.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-get-character-by-name (NAME)

  "Return the Unicode character whose name is `NAME'.

Haskell Type « :: Strinig -> Maybe Char ».

Examples:

    M-: (sboo-get-character-by-name \"BULLET\")
    ?•

    M-: (sboo-get-character-by-name \"NOT A UNINCODE CHARACTER NAME\")
    nil

See `sboo-unicode-names-hash-table'."

  (interactive (list
                (sboo-read-character-name)))

  (let ((CHAR (gethash NAME ucs-names)))
    CHAR))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-insert-character-by-name (NAME)

  "Insert the Unicode character `NAME', by name.

Wraps `sboo-read-character-by-name'.

Like `insert-char', but:

* its completion is more flexible (`helm' will fuzzily-match); and
* it displays TODO the literal unicode character itself alongside each name.
"

  (interactive (list
                (sboo-read-character-name)))

  (let ((CHAR (sboo-get-character-by-name NAME)))

    (insert-char CHAR)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; DOCS `cl-flet':
;;
;; e.g.
;;
;;    (require 'cl)
;;    (cl-flet ((f (x) (* x x)))
;;      (f 7))
;;

;;; See:
;;
;; ./share/emacs/26.1/lisp/international/uni-name.el 
;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-unicode)