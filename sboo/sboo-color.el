;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Colors

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(require 'cl)
(require 'pcase)
(require 'json)

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

(cl-defstruct (sboo-color-name
               (:constructor sboo-color-name-create)
               (:copier      nil))

  name hex)

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;; sboo-color--*

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;; sboo-color-*

(cl-defun sboo-color-parse-json (&key file string)

  "Parse a json STRING or FILE, whose schema is a list of `sboo-color-name'.

Inputs:

• FILE — a « .json » file.
• STRING — a json string.

Output:

• a list of `sboo-color-name's.

Example:

• M-: (sboo-color-parse-json :string \"[{\\\"name\\\":\\\"1975 Earth Red\\\",\\\"hex\\\":\\\"#7a463a\\\"}]\")
    ⇒ [ ... ]

• M-: (sboo-color-parse-json :file \"colornames.json\")
    ⇒ [ ... ]

Links:

• URL `https://unpkg.com/color-name-list@3.64.0/dist/colornames.json"

  (let* ((JSON (if string
                   string
                 (if file
                     (read file)
                   (throw 'sboo-color-parse-json))))
         )

    (let* ((json-object-type 'hash-table)
           (json-array-type  'list)
           (json-key-type    'string)
           (TABLE            (json-read-file file)))

      TABLE)))

;; e.g. (sboo-color-parse-json :string "[{\"name\":\"1975 Earth Red\",\"hex\":\"#7a463a\"}]")

;; e.g.
;;
;; M-: (with-temp-buffer (insert "[{\"name\":\"1975 Earth Red\",\"hex\":\"#7a463a\"}]") (json-read))
;;   ⇒ ((name . "1975 Earth Red")(hex . "#7a463a"))
;;
;; 
;;
  
;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defcustom sboo-color-alist

  '( 
   )

  "Represents a set of `sboo-color-name's.

Associates color names with hex values."

  :type '(alist :key-type   (string :tag "Color name")
                :value-type (choice (const nil)
                                    (string :tag "Hex value")))

  :safe  t
  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-color-json-file

  "colornames.json"

  "JSON file of color names.

Schema:

• is « [ {\"name\":\"string\", \"hex\":\"string\"} ] »

Data source for `sboo-color-alist'."

  :type '(string :tag "FILE.json")

  :safe  t
  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-color-elisp-file

  "colornames.alist.el"

  "Elisp file of color names.

Data source for `sboo-color-alist'."

  :type '(string :tag "FILE.el")

  :safe  t
  :group 'sboo)

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-color-read (&key prompt require-match initial-input)

  "Read a color by name, returing a hex string.

Reads from `sboo-color-alist'.

Example:

• M-: (sboo-color-read )
    ⇒ 

Example:

• M-x sboo-read-color <RET>
    ⇒ ()

Links:

• URL `https://github.com/meodai/color-names#readme'

Related:

• `sboo-color-alist'"

  (interactive)

  (let ((PROMPT (format "%s: "
                        (or prompt "Color")))

        (REQUIRE-MATCH (or require-match t))
        (INITIAL-INPUT (or initial-input ""))
        )

    (let* ((has_sboo-color-alist_been_set? (not (eq nil sboo-color-alist))) ;TODO; or check/set a global flag, defaulting to X11's « colors.txt »

           (CANDIDATES (progn
                         (unless has_sboo-color-alist_been_set?
                           ())
                         sboo-color-alist))

           (STRING (completing-read PROMPT CANDIDATES nil REQUIRE-MATCH INITIAL-INPUT))
           (COLOR  )
           )

      COLOR)))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;;
;; 
;;
;;----------------------------------------------;;
(provide 'sboo-color)