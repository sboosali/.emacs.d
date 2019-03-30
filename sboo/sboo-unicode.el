;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; Completion for Unicode Character Names.
;;
;; • `sboo-ucs-names-table' — the primary Unicode data structure.
;; • `sboo-read-character-name' — the primary completion function.
;;
;; 

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(require 'cl-lib)     ;; "CommonLisp Library"
(require 'subr-x) ;; "SUBRoutine-eXtRAS"
(require 'mule)   ;; "MUltiLingual Environment"

;;----------------------------------------------;;
;; Variables: Customizeable --------------------;;
;;----------------------------------------------;;

(defcustom sboo-top-unicode-characters-list

  '(
    ?•
    ?→
    ?«
    ?»
    ?“
    ?”
    ?⇒
    ?①
    ?②
    ?③
    ?✔
    ?✘
    ?∀
    ?∃
    ?∑
    ?∏
    ?é
    ?ñ
    ?à
    ?⌥
    ?^
    ?⇪
    ?❖
    ?⌘
    ?∈
    ?∋
    ?⊆
    ?⊇
    ?Σ
    ?¬
    ?∝
    ?∅
    ?∞

    ;;

    ?α
    ?β
    ?γ
    ?🖮
    ?🖰
    ?⇧
    ?☰
    ?⭾
    ?⏎
    ?⌫
    ?⌦
    ?⎋
    ?⤒
    ?⤓
    ?⇤
    ?⇥
    ?▲
    ?▼
    ?↑
    ?↓
    ?→
    ?←
    ?❓
    ?✂
    ?⎌
    ?🗘
    ?🔎
    ?🔇
    ?🔉
    ?🔊
    ?◼
    ?⏯
    ?⏮
    ?⏭
    ?⏫
    ?⏬
    ?¡
    ?¿
    ?―
    ?¶
    ?˽
    ?🗷
    ?☑
    ?⓪
    ?①
    ?②
    ?③
    ?④
    ?⑤
    ?⑥
    ?⑦
    ?⑧
    ?⑨
    ?⑩
    ?⑪
    ?⑫
    ?⑬
    ?⑭
    ?⑮
    ?⑯
    ?⑰
    ?⑱
    ?⑲
    ?⑳
    ?👁
    ?👂
    ?😃
    ?👍
    ?✊
    ?👉
    ?🌣
    ?⛆
    ?❄
    ?⛰
    ?🗺
    ?☕
    ?🍄
    ?🐕
    ?🐒
    ?🐘
    ?🐖
    ?🐙
    ?🐬
    ?🦁
    ?🚲
    ?𝄞
    ?𝄢
    ?🎹
    ?🎸
    ?♩
    ?♪
    ?♫
    ?♬
    ?♯
    ?♭
    ?𝆏
    ?𝆐
    ?𝆑
    ?𝆒
    ?𝆓
    ?🅐
    ?☭
    ?©
    ?🄯 
    ?♻
    ?㎰
    ?㎱
    ?㎲
    ?㎳
    ?㎍
    ?㎎
    ?㎏
    ?㎚
    ?㎛
    ?㎜
    ?㎝
    ?㎞
    ?㏌
    ?㎅
    ?㎆
    ?㎇
    ?㏈
    ?㎐
    ?㎑
    ?㎧
    ?㎨
    ?㎉
    ?𝛁
    ?×
    ?ℤ
    ?ℚ
    ?ℝ
    ?ℯ
    ?√
    ?㏒
    ?⊕
    ?⊖
    ?⊘
    ?⊚
    ?⊜
    ?±
    ?∉
    ?∌
    ?⊈
    ?⊉
    ?⨝
    ?≤
    ?≥
    ?≰
    ?≱
    ?≡
    ?≢
    ?⊨
    ?⊭
    ?∄
    ?∵
    ?⊦
    ?⊬
    ?∧
    ?∨
    ?∫
    ?∂
    ?⊶
    ?⊷
    ?⊸
    ?（
    ?）
    ?［
    ?］
    ?｛
    ? ｝
    ?︷
    ?︸
    ?⎧
    ?⎨
    ?⎩
    ?⟹
    ?⬳
    ?⟿
    ?⭍
    ?↔
    ?®
    ?™
    ?¼
    ?½
    ?¾
    ?⅓
    ?⅔
    ?⅛
    ?⅜
    ?⅝
    ?⅞
    ?Ⅰ
    ?Ⅱ
    ?Ⅲ
    ?Ⅳ
    ?Ⅴ
    ?Ⅵ
    ?Ⅶ
    ?Ⅷ
    ?Ⅸ
    ?Ⅹ
    ?Ⅺ
    ?Ⅻ
    )

  "Set of Unicode Characters to prioritize during display/selection.

Related:

• `sboo-insert-char'

Links:

• URL `http://xahlee.info/comp/unicode_computing_symbols.html'"

  :type '(repeated (char))

  :safe t
  :group 'sboo)

;; NOTE 👼 👻 🧙 🧚 🧛 🧜 🧝 🧞 🧟 👿 ⌘ ✲ ⎈ ^ ⌃ ❖ ⌘ ⎇ ⌥ ◆ ◇ ✦ ✧ ⇧ ⇪ 🄰 🅰 ⇪  ⬅ ➡ ⬆ ⬇ ⎉ ⎊ ⎙ ⍰ ❓ ❔ ℹ 🛈 ☾ ⏏ ✉ 🏠 🏡 ⌂ ✂ ✄ ⎌ ↶ ↷ ⟲ ⟳ ↺ ↻ 🔍 🔎 🔅 🔆 🔇 🔈 🔉 🔊 🕨 🕩 🕪 ◼ ⏯ ⏮ ⏭ ⏪ ⏩ ⏫ ⏬ 🥬 🥦 🍄 🍅 🍆 🌶 🥑 🥕 🥒 🥔 🥜 🐭 🐹 🐰 🐶 🐺 🦊 🐵 🐸 🙈 🙉 🙊 🐯 🦁 🦓 🦒 🐴 🐮 🐷 🐻 🐼 🐲 🦄 🐅 🐆 🐘 🦏 🐂 🐃 🐄 🐎 🦌 🐐 🐏 🐑 🐖 🐗 🦛 🐪 🐫 🦍 🦙 🦘 🐉 🦖 🦕 🎹 🎻 🎷 🎺 🎸 🥁 🎵 🎶 🎼 🎜 🎝 ♩ ♪ ♫ ♬ 𝅜 𝅝 𝅗𝅥 𝅘𝅥 𝅘𝅥𝅮 𝅘𝅥𝅯 𝅘𝅥𝅰 𝅘𝅥𝅱 𝅘𝅥𝅲 𝄺 𝄩 𝄻 𝄼 𝄽 𝄾 𝄿 𝅀 𝅁 𝅂 𝄒 𝄓 𝄐 𝄑 𝆏 𝆐 𝆑 𝆒 𝆓 𝄀 𝄁 𝄂 𝄃 𝄄 𝄅 𝄔 𝄕 𝄆 𝄇 𝄈 𝄉 𝄊 𝄋 𝄌 𝄍 𝄎 𝄏 𝄖 𝄗 𝄘 𝄙 𝄚 𝄛 𝄞 𝄡 𝄢 𝄚𝄟 𝄚𝄠 𝄚𝄣 𝄚𝄤 𝄚𝄥 𝄚 𝄦 ♯ 𝄪 𝄰 𝄱 𝄲 𝄳 ♭ 𝄫 𝄬 𝄭 ♮ 𝄮 𝄯

;;----------------------------------------------;;
;; Variables: Internal -------------------------;;
;;----------------------------------------------;;

(defvar sboo-ucs-names-table

  nil

  "Hash-Table from Unicode Character names to Unicode Character codepoints.

« ucs » abbreviates « Unicode CharacterS(?) ».

Examples:

• M-: (message \"%c\" (gethash \"LATIN SMALL LETTER ALPHA\" sboo-ucs-names-table))
    ⇒ \"ɑ\"

Accessed by Function `sboo-ucs-names-table'.")

;;----------------------------------------------;;

(defvar sboo-ucs-names-list

  nil

  "List of Unicode Character names.

Examples:

• M-: (nth 891 sboo-ucs-names-list)
    ⇒ \"LATIN SMALL LETTER ALPHA\"

Related:

• `sboo-unicode--'

Accessed by Function `sboo-ucs-names-list'.")

;;----------------------------------------------;;

(defvar sboo-ucs-names-interesting-list

  nil

  "List of Unicode Character names, filtering away “boring” characters (like ASCII).

Related:

• `sboo-unicode--interesting-character-p'")

;;----------------------------------------------;;

(defvar sboo-ucs-names-annotated-list

  nil

  "List of Unicode Character names, annotated with their namesake-characters.

Examples:

• M-: (nth 891 sboo-ucs-names-annotated-list)
    ⇒ \"ɑ LATIN SMALL LETTER ALPHA\"

Related:

• `sboo-unicode--annotate-character-p'

Displayed by Function `sboo-read-character-name'.

Like `sboo-ucs-names-list', but each name is prefixed by the namesake character (plus a space).")

;;----------------------------------------------;;

(defcustom sboo-unicode-completion-annotate

  t

  "Whether to display the unicode character itself (beside the name), during completion.

Type:

• a `booleanp'.

Related:

• `sboo-read-character-by-name'.

For example, `sboo-unicode-completion-annotate' toggles whether `sboo-read-character-by-name' displays:

• \"LATIN SMALL LETTER ALPHA\"   (if nil)
• \"ɑ LATIN SMALL LETTER ALPHA\" (if t)"

  :type '(boolean)

  :safe  t
  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-unicode-completion-namesake-divider

  "  "

  "Divider between a character and its name, in `sboo-read-character-name' (a `stringp')."

  :type '(choice (string  :tag "Literal text")
                 (integer :tag "Number of spaces")
                 (const nil :tag "No divider")) ;TODO or "Fallback to default"?

  ;;:set ;TODO set dirty flag (property?)

  :safe  t
  :group 'sboo)

;;----------------------------------------------;;
;; Functions: Accessors ------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-ucs-names-table (&key (refresh nil) (only-interesting t))

  "Return a mapping of Unicode Character names to Unicode Character codepoints.

Initializes Variable `sboo-ucs-names-table'.

Inputs:

• REFRESH — a `booleanp'.
  If non-nil, rebuild Variable `sboo-ucs-names-table' from `ucs-names'
  (even if they've already been initialized to a non-nil `hash-table-p').
• ONLY-INTERESTING — a `booleanp'.
  If non-nil, filter by `sboo-unicode--interesting-character-p'.

Output:

• a `hash-table-p' from `stringp's to `integerp's.

Examples:

• M-: (sboo-ucs-names-table :refresh t)
    ⇒ #s(hash-table size 42943 test equal rehash-size 1.5 rehash-threshold 0.8125 data (\"NULL\" 0 ... ))

Related:

• `ucs-names'

Notes:

• `ucs-names' is a `hash-table-p' on Emacs≥26 and an alist on Emacs≤25."

  (progn

    ;; Initialize:

    (when (or refresh
              (not sboo-ucs-names-table))

      (let* ((OBJECT (ucs-names))
             (TYPE   (type-of OBJECT))
             (TABLE  (pcase TYPE
                       ('hash-table OBJECT)
                       ('list       (sboo/alist->table OBJECT :test #'equal :size 43000))
                       (_           nil)))

             (TABLE2  (if only-interesting
                          (let ((NEW-TABLE (copy-hash-table TABLE)))
                            (maphash (lambda (key value)
                                       (unless (sboo-unicode--interesting-character-p value)
                                           (remhash key NEW-TABLE)))
                                     NEW-TABLE)
                            NEW-TABLE)
                        (copy-hash-table TABLE)))
             )

        (setq sboo-ucs-names-table TABLE2)))

    ;; Access:

    sboo-ucs-names-table))

;; M-: (sboo-ucs-names-table :refresh t :only-interesting t)

;;----------------------------------------------;;

(cl-defun sboo-ucs-names-list (&key (refresh nil) (annotate t) (only-interesting t))

  "Return all Unicode Character names.

Inputs:

• REFRESH — a `booleanp'.
  If non-nil, rebuild the « sboo-ucs-names-* » variables
  (i.e. Variable `sboo-ucs-names-list', Variable `sboo-ucs-names-annotated-list')
  from `ucs-names', even if they've already been initialized (to a non-nil `listp').
• ANNOTATE — a `booleanp'.
  Whether to return Variable `sboo-ucs-names-list' or Variable `sboo-ucs-names-annotated-list'.
• ONLY-INTERESTING — a `booleanp'.
  If non-nil, keep only “interesting” characters
  (i.e. filter by `sboo-unicode--interesting-character-p').

Output:

• a `listp' of `stringp's.

Examples:

• M-: (sboo-ucs-names-list)
    ⇒ (\"NULL\" ... \"BULLET\" ...)

• M-: (sboo-ucs-names-list :annotate t)
    ⇒ (\"  NULL\" ... \"• BULLET\" ...)

• M-: (sboo-ucs-names-list :only-interesting t)
    ⇒ ( ... \"• BULLET\" ...)

Related:

• `ucs-names'
• `sboo-top-unicode-characters-list'

Notes:

• `ucs-names' is a `hash-table-p' on Emacs≥26 and an `alistp' on Emacs≤25."

  (progn

    ;; Initialize:

    (when (or refresh
              (not (and sboo-ucs-names-list
                        sboo-ucs-names-annotated-list)))

      (let* ((TABLE        (sboo-ucs-names-table :refresh refresh :only-interesting only-interesting))
             (NAMES-KEYS   (hash-table-keys TABLE))
             (NAMES-TOP    (mapcar #'sboo-unicode-get-char-name sboo-top-unicode-characters-list))
             (NAMES-SORTED (sboo-unicode--move-sublist-to-front NAMES-TOP NAMES-KEYS :test #'equal))
             (NAMES        NAMES-SORTED)
             )

        (progn

          (setq sboo-ucs-names-list NAMES)

          (setq sboo-ucs-names-annotated-list
                (mapcar #'sboo-unicode--prefix-namesake-character NAMES)))))

    ;; Access:

    (if annotate
        sboo-ucs-names-annotated-list
      sboo-ucs-names-list)))

;; M-: (sboo-ucs-names-list :refresh t :annotate t :only-interesting t)

;;----------------------------------------------;;

(defun sboo-unicode-completion-namesake-divider-string ()

  "Accessor for variable `sboo-unicode-completion-namesake-divider'.

Output:

• a `stringp'."

  (let* ((OBJECT sboo-unicode-completion-namesake-divider)
         )

  (pcase OBJECT

    ((pred stringp)  OBJECT)
    ((pred integerp) (make-string OBJECT ?\ ))
    ('nil            nil)

    (_ " "))))

;; ^ NOTE «  ?\  » is a space literal character.

;;----------------------------------------------;;

(defun sboo-unicode-completion-namesake-divider-length ()

  "Accessor for Variable `sboo-unicode-completion-namesake-divider'.

Output:

• an `integerp'."

  (let* ((OBJECT sboo-unicode-completion-namesake-divider)
         )

  (pcase OBJECT

    ((pred stringp)  (length OBJECT))
    ((pred integerp) OBJECT)
    ('nil            nil)

    (_ 1))))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-ucs-names-get (name)

  "Get the Unicode Character whose name is NAME.

Inputs:

• NAME — a `stringp'.
  The name of a Unicode Character.

Output:

• an `integerp'.
  a Unicode Character codepoint.

Type (Haskell):

• « :: String -> Maybe Char »

Example:

• M-: (format \"%c\" (sboo-ucs-names-get \"BULLET\"))
    ⇒ \"•\"
• M-: (sboo-ucs-names-get \"NOT A UNINCODE CHARACTER NAME\")
    ⇒ nil"

  (let* ((NAME  (upcase name))
         (TABLE (sboo-ucs-names-table))
         (CHAR  (gethash NAME TABLE))
         )

    CHAR))

;; ^ (format "%c" (sboo-ucs-names-get "BULLET"))

;;----------------------------------------------;;

(defun sboo-unicode-get-char-name (char)

  "Get the Unicode Character Database « 'name » of CHAR.

Inputs:

• CHAR — a `characterp' (preferred);
  or a `stringp' or `symbolp' (accepted).

Examples:

    M-: (sboo-unicode-get-char-name ?γ)
    ⇒ \"GREEK SMALL LETTER GAMMA\"

    M-: (sboo-unicode-get-char-name 'γ)
    ⇒ \"GREEK SMALL LETTER GAMMA\"

    M-: (sboo-unicode-get-char-name \"γαμμα\")
    ⇒ \"GREEK SMALL LETTER GAMMA\"

Related:

• `get-char-code-property'."

  (let* ((CHAR (pcase char
                 ((pred characterp) char)
                 ((pred stringp)    (aref char 0))
                 ((pred symbolp)    (aref (symbol-name char) 0))
                 (_ (throw 'sboo-unicode-get-char-name
                           (format-message "CHAR is « %S », of type « %S », not a `characterp' or `stringp'." char (type-of char))))))

         (NAME (get-char-code-property CHAR 'name))
         )

    (progn
      (when (called-interactively-p 'any)
        (message NAME))

      NAME)))

;; TODO e.g. with `M-x set-input-method RET TeX RET`, typing `\xi` inputs `ξ`.

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(defun sboo-unicode-print-char (char)

  "Get the Unicode Character Database « 'name » of CHAR.

Inputs:

• CHAR — a character (an `integerp').

Examples:

    M-: (call-interactively #'sboo-unicode-print-char)
    Character: c
    ⇒ \"LATIN SMALL LETTER C\"

    M-: (sboo-unicode-print-char ?γ)
    ⇒ \"GREEK SMALL LETTER GAMMA\"

Related:

• `sboo-unicode-get-char-name'."

  (interactive (list
                (or (condition-case _
                        (sboo-unicode--interesting-character-p (thing-at-point 'char))
                      (error nil))
                    (condition-case _
                        (let ((CLIPBOARD-CONTENTS (car kill-ring)))
                          (if (and CLIPBOARD-CONTENTS (= 1 (length CLIPBOARD-CONTENTS)))
                              (substring CLIPBOARD-CONTENTS 0 1)))
                      (error nil))
                    (condition-case _
                        (read-char-exclusive "Character (press a key): ")
                                        ;TODO read 1-length string.
                      (error nil))
                    )))

  (let* ((NAME (sboo-unicode-get-char-name CHAR))
         )

    (progn
      (when (called-interactively-p 'any)
        (message NAME))

      NAME)))

;;----------------------------------------------;;

(defun sboo-read-character-name (&optional refresh annotate only-interesting)

  "Read a Unicode Character name.

Inputs: Keyword Arguments are passed-through to `sboo-ucs-names-list'.

Output:

• a `stringp'.
  a key of `ucs-names'.

Type (Haskell):

• « :: IO String ».

Related:

• `ucs-names'."

  (interactive (list nil
                     (if current-prefix-arg t nil)
                     ))

  (let*  ((ANNOTATE      (or annotate sboo-unicode-completion-annotate))

          (PROMPT        (format "%s: " "Unicode Character name"))
          (REQUIRE-MATCH t)
          (PREDICATE     nil)
          (HISTORY       nil)
          (CANDIDATES    (sboo-ucs-names-list :annotate ANNOTATE :only-interesting only-interesting :refresh refresh))
          )

    (let* ((STRING (let ((completion-ignore-case t))
                     (completing-read PROMPT CANDIDATES PREDICATE REQUIRE-MATCH nil HISTORY nil nil)))

           (NAME   (if ANNOTATE
                       (sboo-unicode--strip-namesake-character STRING)
                     STRING))
           )

      (string-trim-left NAME))))

;; ^ NOTE `string-trim-left' will trim a `sboo-unicode-completion-namesake-divider' any length.

;;----------------------------------------------;;

(defun sboo-read-character-by-name ()

  "Read a Unicode Character name, returning the corresponding Unicode Character.

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

  (let* ((STRING (sboo-read-character-name nil t t))
         (CHAR   (sboo-ucs-names-get STRING))
         )

    CHAR))

;;----------------------------------------------;;

;;TODO;(cl-defun sboo-insert-unicode-name (name &key display-properties)

(defun sboo-insert-unicode-name ()

  "Read and insert a Unicode Character name.

Output:

• a `stringp'.
  The Unicode Character name that was read and inserted.
  nil if that character didn't satisfy `sboo-unicode--insertable-character-p'.

Type (Haskell):

• « :: IO String ».

Related:

• `sboo-read-character-name'."

  (interactive)

  (let ((STRING (sboo-read-character-name nil t t))
        (NAME   (upcase STRING))
        )

    (insert STRING)))

;; M-x (call-interactively #'sboo-insert-unicode-name)

;;----------------------------------------------;;

;;TODO;(cl-defun sboo-insert-char (name &key display-properties)


(defun sboo-insert-char (name)

  "Insert the Unicode Character named `NAME'.

Inputs:

• NAME — a `stringp'. 
         The name of a Unicode Character.

Type (Haskell):

• « :: String -> IO () ».

Notes:

• `sboo-insert-char' is like `insert-char', but:

    • its completion is more flexible (for example, `helm' can be configured to efficiently fuzzily-match)
    • it displays the literal unicode character itself alongside each name (when `sboo-unicode-completion-annotate' is non-nil).

Related:

• `sboo-unicode-completion-annotate'.
• `sboo-read-character-name'.
• `sboo-ucs-names-get'."

  (interactive (list (sboo-read-character-name nil t t)
                     ))

  (let* ((NAME name)
         (CHAR (sboo-ucs-names-get NAME))
         )

    (pcase CHAR

      ('nil              nil)
      ((pred stringp)    (insert      CHAR))
      ((pred characterp) (insert-char CHAR))

      (_ (error (format "(sboo-insert-char ?%c)" CHAR))))))

;; M-x (call-interactively #'sboo-insert-char) 

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-unicode--prefix-namesake-character (name)

  "Prefix NAME with the character it names.

Inputs:

• NAME — a `stringp'. The name of the unicode character.

Output:

• a `stringp'. 
  the output `length' will always be exactly two more than the input's.

Some characters aren't displayed, including:

• non-printable characters (e.g. NULL)
• ascii characters (e.g. LATIN SMALL LETTER A)

Examples:

    M-: (sboo-unicode--prefix-namesake-character \"BULLET\")
      ⇒ \"•  BULLET\"

    M-: (sboo-unicode--prefix-namesake-character \"SKULL\")
      ⇒ \"💀  SKULL\"

    M-: (sboo-unicode--prefix-namesake-character \"NULL\")
      ⇒ \"   NULL\"

    M-: (equal (substring (sboo-unicode--prefix-namesake-character \"NULL\") (sboo-unicode-completion-namesake-divider-length)) \"NULL\")
      ⇒ t

    M-: (= (sboo-unicode-completion-namesake-divider-length) (- (length (sboo-unicode--prefix-namesake-character \"NULL\")) (length \"NULL\"))
      ⇒ t"

  (let* ((CHAR    (sboo-ucs-names-get name))
         (DIV     (sboo-unicode-completion-namesake-divider-string))
         )

    (if DIV

        (let* ((STRING (if (and CHAR (sboo-unicode--annotate-character-p CHAR))
                           (char-to-string CHAR)
                         " "))
               )
          (concat STRING DIV name))

      (char-to-string CHAR))))

;; « C-x C-e » Tests:
;;
;; (sboo-unicode--prefix-namesake-character "SKULL")
;; (sboo-unicode--prefix-namesake-character "LATIN SMALL LETTER ALPHA")
;; (sboo-unicode--prefix-namesake-character "")
;;
;; (sboo-unicode--prefix-namesake-character "NULL")
;; (sboo-unicode--prefix-namesake-character "DIGIT ZERO")
;; (sboo-unicode--prefix-namesake-character "LATIN SMALL LETTER A")
;;

;;----------------------------------------------;;

(cl-defun sboo-unicode--annotate-character-p (char &key )

  "Whether `sboo-read-character' should annotate CHAR.

Inputs:

• CHAR — an `integerp'.
  A Unicode Character codepoint.

Output:

• a `booleanp'. 

Some characters can't be displayed (or shouldn't be annotated), including:

• Non-printable characters (e.g. NULL)
• Alphanumeric ASCII characters (e.g. LATIN SMALL LETTER A)

Examples:

    M-: (sboo-unicode--annotate-character-p ?💀)
      ⇒ t

    M-: (sboo-unicode--annotate-character-p ?-)
      ⇒ t

    M-: (sboo-unicode--annotate-character-p 0)
      ⇒ nil

    M-: (sboo-unicode--annotate-character-p ?0)
      ⇒ nil

    M-: (sboo-unicode--annotate-character-p ?a)
      ⇒ nil

    M-: (sboo-unicode--annotate-character-p ?Z)
      ⇒ nil

Links:

• URL `https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Display.html'"

  (let* ((UNPRINTABLE?        (< char 32))  ;TODO what about unprintables in higher-ranges? or surrogates?
         (ASCII-ALPHANUMERIC? (or (and (>= char ?0) (<= char ?9))
                                  (and (>= char ?A) (<= char ?Z))
                                  (and (>= char ?a) (<= char ?z))
                                  ))
         )

    (and (not UNPRINTABLE?)
         (not ASCII-ALPHANUMERIC?)
         )))

;;----------------------------------------------;;

(cl-defun sboo-unicode--insertable-character-p (char &key )

  "Whether `sboo-insert-char' can `insert' CHAR.

Inputs:

• CHAR — an `integerp'.
  A Unicode Character codepoint.

Output:

• a `booleanp'.

Some characters can't be inserted, including:

• Surrogates (e.g. TODO)

Links:

• URL `'"

  t)

;;----------------------------------------------;;

(defun sboo-unicode--strip-namesake-character (string)

  "Invert `sboo-unicode--prefix-namesake-character'.

Inputs:

• STRING — a `stringp'.

Output:

• a `stringp'. 

Examples:

    M-: (sboo-unicode--strip-namesake-character (sboo-unicode--prefix-namesake-character \"BULLET\"))
      ⇒ \"BULLET\"

Related:

• `sboo-unicode-completion-namesake-divider'"

  (let* ((LENGTH (sboo-unicode-completion-namesake-divider-length))
         )

    (if LENGTH
        (substring string (+ 1 LENGTH))
      string)))

;; (sboo-unicode--strip-namesake-character (sboo-unicode--prefix-namesake-character "BULLET"))

;;----------------------------------------------;;

(defun sboo-unicode--interesting-character-p (char)

  "Whether CHAR is an “interesting” Unicode Character.

Inputs:

• CHAR — an `integerp'.

Output:

• a `booleanp'."

  (let* ((UNPRINTABLE?        (< char 32))  ;TODO what about unprintables in higher-ranges? or surrogates?
         (ASCII-ALPHANUMERIC? (or (and (>= char ?0) (<= char ?9))
                                  (and (>= char ?A) (<= char ?Z))
                                  (and (>= char ?a) (<= char ?z))
                                  ))
         )

    (and (not UNPRINTABLE?)
         (not ASCII-ALPHANUMERIC?)
         )))

;;----------------------------------------------;;

(cl-defun sboo/alist->table (alist &key test size)

  "Create a Hash Table from ALIST.

Inputs:

• ALIST — an association `listp'. 
• TEST  — a `symbolp' (naming a `functionp'). See `make-hash-table'.
• SIZE  — an `integerp'. See `make-hash-table'.

Output:

• a `hash-table-p' with the same entries as ALIST (modulo duplicate keys).

Example:

• M-: (sboo/alist->table '((x . 1) (y . 2)) :test #'eq :size 3)
    ⇒ #s(hash-table size 3 test eq data (x 1 y 2))"

  (let* ((TEST  (or test
                    #'equal))

         (SIZE  (or size
                    (length alist)))

         (TABLE (make-hash-table :test TEST :size SIZE))

         )

    (progn

      (dolist (ENTRY alist)
        (let ((k (car ENTRY))
              (v (cdr ENTRY))
              )
          (puthash k v TABLE)))

      TABLE)))

;;----------------------------------------------;;

(cl-defun sboo-unicode--move-sublist-to-front (sublist superlist &key (test #'equal))

  "Pull SUBLIST to the head of SUPERLIST.

Inputs:

• SUBLIST   — a `listp'. items not in SUPERLIST are ignored.
• SUPERLIST — a `listp'. the relative order of items not in SUBLIST is preserved.
• TEST      — a `functionp'. how to check items for equality.

Output:

• a `listp'. a new list (copied from SUPERLIST, then modified).
  has the same items as SUPERLIST, possibly in a different order.

Example:

• M-: (sboo-unicode--move-sublist-to-front '(x y z) '(a b y c x))
    ⇒ '(x y a b c)

• M-: (sboo-unicode--move-sublist-to-front nil '(a b c))
   ⇒ '(a b c)

• M-: (sboo-unicode--move-sublist-to-front '(x y z) nil)
    ⇒ nil

Related:

• `'"

  (let* ((SUPERLIST-COPY (copy-sequence superlist))
         (SUBLIST-COPY   (remove-duplicates sublist :test test))

         (COMPARE        (lambda (x y)

                            (let ((X-IN-SUBLIST (if (cl-member x SUBLIST-COPY :test test) 1 0))
                                  (Y-IN-SUBLIST (if (cl-member y SUBLIST-COPY :test test) 1 0))
                                  )
                              (cond

                                ((and (> X-IN-SUBLIST 0) (> Y-IN-SUBLIST 0))
                                 (let ((X-INDEX (cl-position x SUBLIST-COPY :test test))
                                       (Y-INDEX (cl-position y SUBLIST-COPY :test test))
                                       )
                                   (<= X-INDEX Y-INDEX)))

                                (t
                                 (> X-IN-SUBLIST Y-IN-SUBLIST))))))
         )

    (cl-stable-sort SUPERLIST-COPY COMPARE)))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;;; DOCS ‘ucs-names’: [TODO]
;;
;; « ucs-names » :: « HashMap String– Char »
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
;;    (require 'cl-lib)
;;    (cl-flet ((f (x) (* x x)))
;;      (f 7))
;;

;;; DOCS `char-to-string':
;;
;; e.g.
;;
;;    M-: (char-to-string ?•)
;;    "•"

;; `cl-stable-sort'
;;
;;     (cl-sort SEQ PREDICATE :key)
;;
;; This function sorts seq into increasing order as determined by using predicate to compare pairs of elements. predicate should return true (non-nil) if and only if its first argument is less than (not equal to) its second argument. For example, < and string-lessp are suitable predicate functions for sorting numbers and strings, respectively; > would sort numbers into decreasing rather than increasing order.
;; 

;; `cl-position':
;; 
;; M-: (cl-position "y" '("x" "y" "z" "y") :test #'equal)
;;     1
;; 

;;; See:
;;
;; ./share/emacs/26.1/lisp/international/uni-name.el 
;; 

;;==============================================;;
(provide 'sboo-unicode)