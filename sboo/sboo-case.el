;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; Casing functions (e.g. "camelCase", "snake_case", etc).
;; 
;; • `sboo-case-vapor-dwim'
;; • `sboo-case-camel-dwim'
;; • `sboo-case-snake-dwim'
;; • `sboo-case-TODO-dwim'
;; • 
;;
;; 

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(require 'cl-lib)
(require 'subr-x)

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-case-vapor-string (s)

  "Convert S to Vaporwave Case.

Inputs:

• S — a `stringp'.

Output:

• a `stringp'.

Example:

• M-: (sboo-case-vapor-string \"vapor case\")
    ⇒ \"ＶＡＰＯＲ ＣＡＳＥ\"

Related:

• `sboo-case-vapor-char'"

  (let* ((STRING       (string-join (split-string s split-string-default-separators t split-string-default-separators) " "))
         (CHARS        (string-to-list STRING))
         (VAPOR-CHARS  (mapcar #'sboo-case-vapor-char CHARS))
         (VAPOR-STRING (apply #'string VAPOR-CHARS))
         )

    VAPOR-STRING))

;; M-: (sboo-case-vapor-string "vapor case")
;;  ⇒ "ＶＡＰＯＲ ＣＡＳＥ"

;; (WORDS (split-string STRING split-string-default-separators t split-string-default-separators))

;;----------------------------------------------;;

(defun sboo-case-vapor-char (c)

  "Convert C to an Uppercase Fullwidth Character.

Inputs:

• C — a `charp'. an ASCII Alphanumeric.
  For example, « ?c » a.k.a. « LATIN SMALL LETTER C ».

Output:

• a `charp'. both Uppercase and Fullwidth.
  For example, « ?Ｃ » a.k.a. « FULLWIDTH LATIN CAPITAL LETTER C ».

Example:

• M-: (format \"%c\" (sboo-case-vapor-char ?c))
    ⇒ \"Ｃ\"

• M-: (sboo-case-vapor-char ?c)
    ⇒ (sboo-case-vapor-char ?C)
    ⇒ ?Ｃ
    ⇒ 65315

Notes:

• “Range U+FF01..FF5E reproduces the characters of ASCII 21..7E as fullwidth forms.”
"

  (let* ((CHAR-UPPER (upcase c))
         (CHAR-WIDE  (when (or (and (>= CHAR-UPPER ?A)
                                    (<= CHAR-UPPER ?Z))
                               (and (>= CHAR-UPPER ?0)
                                    (>= CHAR-UPPER ?9)))
                       (+ 65248 CHAR-UPPER)))
         )

    (if CHAR-WIDE
        CHAR-WIDE
      CHAR-UPPER)))

;; M-: (- 65315 67)
;;     65248

;; M-: ?A
;;     65
;;
;; M-: ?Z
;;     90
;;
;; M-: ?0
;;     48
;;
;; M-: ?9
;;     57
;;

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-case-vapor-dwim (&optional s)

  "Convert S to ＶＡＰＯＲ ＣＡＳＥ.

Related:

• `sboo-case-vapor-string'"

  (interactive (list
                (sboo-case--dwim-string :prompt    "String (to ＶＡＰＯＲ ＣＡＳＥ): "
                                        :predicate #'sboo-case--text-string-p)
                ))

  (let* ((STRING       s)
         (VAPOR-STRING (sboo-case-vapor-string STRING))
         )

    (insert VAPOR-STRING)))

;;TODO overwrite highlighted region, or paste onto clipboard, or overwrite thing (word/sentence).

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-case--dwim-string (&key (prompt "String: ") (predicate #'sboo-case--text-string-p))

  "Do-What-I-Mean — get a valid string from the user.

Inputs:

• PROMPT    — a `stringp'.
• PREDICATE — a `functionp'. 
  takes one `stringp' and gives a `booleanp'.
  PREDICATE defines a “valid” string.

Output:

• a `stringp' or nil. try (in order) each of:

    • the region             (if a valid string is highlighted).
    • the current sentence   (if valid).
    • the current word       (if valid).
    • the clipboard contents (if valid).
    • user input             (valid or invalid).

Links:

• URL `https://stackoverflow.com/questions/605846/how-do-i-access-the-contents-of-the-current-region-in-emacs-lisp'
• URL `https://stackoverflow.com/questions/2219537/how-to-get-selected-text-in-emacs-lisp'

Related:

• `delete-selection-mode'"

  (let* (
         )

    (or (condition-case _

            (when (use-region-p)
              (filter-buffer-substring (region-beginning) (region-end)))

          ;; ^ Try the current selection.

          (error nil))

         (condition-case _

             (let* ((SENTENCE (thing-at-point 'sentence))
                    (TEXT-P   (and SENTENCE
                                   (funcall predicate SENTENCE)))
                    )
               (if TEXT-P
                   SENTENCE

                 (let* ((LINE   (thing-at-point 'line))
                        (TEXT-P (and LINE
                                     (funcall predicate LINE)))
                        )
                   (if TEXT-P
                       LINE

                     nil))))

          ;; ^ Try the current sentence.

          (error nil))

        (condition-case _

            (let* ((CLIPBOARD-CONTENTS (car kill-ring))
                   (TEXT-P             (and CLIPBOARD-CONTENTS
                                            (funcall predicate CLIPBOARD-CONTENTS)))
                   )
              (if TEXT-P
                  (substring CLIPBOARD-CONTENTS 0 1)))

          ;; ^ Try the clipboard.

          (error nil))

        (condition-case _

            (read-string prompt)

          ;; ^ Try user input.

          (error nil))

        )))

;; e.g. things: symbol, word, sentence, line.

;;----------------------------------------------;;

(defun sboo-case--text-string-p (s)

  "Whether S looks like simple text (a guess).

Inputs:

• S — a `stringp'.

Output:

• a `booleanp'.
  Matches only Alphanumeric Characters and Whitespace Characters (see `rx')."

  (and (stringp s)
       (if (string-match-p (rx bos (1+ (or alphanumeric whitespace)) eos) s) t nil)
  ))

;;----------------------------------------------;;

(defun sboo-case--printable-string-p (s)

  "Whether S is printable characters only (not binary).

Inputs:

• S — a `stringp'.

Output:

• a `booleanp'."

  (and (stringp s)
       (if (string-match-p (rx bos (1+ printing) eos) s) t nil)
  ))

;;----------------------------------------------;;

(defun sboo-case--non-empty-string-p (s)

  "Whether S isn't “empty”.

Inputs:

• S — a `stringp'.

Output:

• a `booleanp'."

  (and (stringp s)
       (not (string-empty-p s))
       (not (if (string-match-p (rx bos (1+ printing) eos) s) t nil))
  ))


;;----------------------------------------------;;

(cl-defun sboo-case-capitalize (text &key (steganograph nil))

  "Capitalize all words in TEXT.

Inputs:

• TEXT         — a `stringp'.
• STEGANOGRAPH — a `booleanp'. 
  (invisibly) `propertize' the output with the TEXT;
  this upgrades `sboo-case-tokenize' into a lossless transformation) helps other casing functions
  (e.g. `sboo-case-invert') preserve the original tokens (e.g. acronyms),
  including lossy transformations

Example:

• M-: (sboo-case-capitalize \"\")
    ⇒ \"\"

Related:

• `sboo-case-capitalize-string'
• `sboo-case-preserve-acronyms' (customizeable)"

  (let* ((WORDS              (sboo-case-tokenize text))
         (WORDS-CAPITALIZED  (mapcar #'sboo-case-capitalize-string WORDS))
         (STRING-CAPITALIZED (string-join WORDS-CAPITALIZED " "))
         )

    STRING-CAPITALIZED))

;; M-: (sboo-case-capitalize " words and/or subwords ") ;TODO
;;  ⇒ "Words And/or Subwords"

;;----------------------------------------------;;

(cl-defun sboo-case-capitalize-string (s)

  "Capitalize the first character in S."

  (concat (upcase (substring s 0 1)) (downcase (substring s 1))))

;;----------------------------------------------;;

(cl-defun sboo-case-tokenize (text &key (steganograph nil))

  "Split TEXT into tokens.

Inputs:

• TEXT         — a `stringp'.
• STEGANOGRAPH — a `booleanp'. 
  (invisibly) `propertize' the output with the TEXT;
  this upgrades `sboo-case-tokenize' into a lossless transformation) helps other casing functions
  (e.g. `sboo-case-invert') preserve the original tokens (e.g. acronyms),
  including lossy transformations

Output:

• a `listp' of `stringp's.

Example:

• M-: (sboo-case-tokenize \"\")
    ⇒ '()

• M-: (sboo-case-tokenize \" two  words \" :steganograph nil)
    ⇒ '(\"two\" \"words\")

• M-: (sboo-case-tokenize \"two-words and/or sub_words\")
   ⇒ '(\"two\" \"-\" \"words\" \"and\" \"/\" \"or\" \"sub\" \"_\" \"words\")

Notes:

• By default, the tokens are words.

Related:

• `sboo-case-preserve-acronyms' (customizeable)"

  (declare (side-effect-free t))

  (let ((*separator-regex* "[_-/ \f\t\n\r\v]+")
        (*omit-nulls*      t)
        (*trim-regex*      "[ \f\t\n\r\v]+")
        )

    (let* ((WORDS (split-string text *separator-regex* *omit-nulls* *trim-regex*))
           )

      WORDS)))

;; M-: (sboo-case-tokenize " two  words " :steganograph nil)
;;  ⇒ '("two" "words")

;; M-: (sboo-case-tokenize "two-words and/or sub_words")
;;  ⇒ '("two" "-" "words" "and" "/" "or" "sub" "_" "words")

;; M-: (split-string "two-words and/or sub_words")
;;  ⇒ '("two-words" "and/or" "sub_words")

;;----------------------------------------------;;

(cl-defun sboo-case-tokenize-word (word &key (steganograph nil))

  "Split WORD into subwords."


  (declare (side-effect-free t))

  (let ((*separator-regex* "[-/_]+")
        (*omit-nulls*      t)
        (*trim-regex*      "[ \f\t\n\r\v]+")
        )

    (let* ((SUBWORDS (split-string word *separator-regex* *omit-nulls* *trim-regex*))
           )

      SUBWORDS)))

;; M-: (sboo-case-tokenize-word "two-words")
;;  ⇒ '("two" "-" "words")

;; M-: (sboo-case-tokenize-word "and/or")
;;  ⇒ '("and" "/" "or")

;; M-: (sboo-case-tokenize-word "sub_words")
;;  ⇒ '("sub" "_" "words")

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; M-: 
;;   ⇒ 

;; M-: (split-string " camel  case " split-string-default-separators t split-string-default-separators)
;;   ⇒ '("camel" "case")
;;
;; M-: (string-join '("camel" "case") " ")
;;   ⇒ "camel case"
  
;; M-: (string-to-list "case")
;;   ⇒ '(?c ?a ?s ?e)
;;   ⇒ '(99 97 115 101)

;; M-: (string-match-p (rx bos (0+ space) eos) " \n\t ")
;;   ⇒ 'TODO

;; M-: (if (string-match-p (rx bos (1+ printing) eos) "matches whitespace and graphic characters") t nil)
;;   ⇒ 't

;; M-: (if (string-match-p (rx bos (1+ (or alphanumeric whitespace)) eos) "whitespace and alphanumum3r1c5") t nil)
;;   ⇒ t
;;
;; M-: (if (string-match-p (rx bos (1+ (or alphanumeric whitespace)) eos) "(no punctuation or symbols)") t nil)
;;   ⇒ nil
  
;;==============================================;;
(provide 'sboo-case)