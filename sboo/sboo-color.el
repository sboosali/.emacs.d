;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Colors

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(require 'cl-lib)
(require 'pcase)
(require 'seq)
(require 'json)

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

(cl-defstruct (sboo-color
               (:constructor sboo-color-create)
               (:copier      nil))

  name hex)

;TODO create a utility defmacro for cl-defstruct

;; (cl-defstruct (NAME
;;                (:constructor NAME-create)
;;                (:copier      nil))
;;   @FIELDS)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defcustom sboo-colors-alist

  nil

  "Represents a set of `sboo-color-name's.

Associates color names with hex values."

  :type '(alist :key-type   (string :tag "Color name")
                :value-type (choice (const nil)
                                    (string :tag "Hex value")))

  :safe  t
  :group 'sboo)

;;----------------------------------------------;;

(defvar sboo-colors-table

  nil

  "Represents a set of `sboo-color-name's.

Associates color names (`stringp') with hex values (`stringp').")

;;----------------------------------------------;;

(defcustom sboo-color-json-file

  "colornames.json"

  "JSON file of color names.

Schema:

• is « [ {\"name\":\"string\", \"hex\":\"string\"} ] »

Data source for `sboo-color-alist'."

  :type '(string :tag "FILE.json")

  :safe #'stringp
  :group 'sboo)

;;----------------------------------------------;;

(defcustom sboo-color-elisp-file

  "colornames.alist.el"

  "Elisp file of color names.

Data source for `sboo-color-alist'."

  :type '(string :tag "FILE.el")

  :safe #'stringp
  :group 'sboo)

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;; sboo-color-*

(cl-defun sboo-color-parse-json (&key json-file json-string)

  "Parse a json STRING or FILE, whose schema is a list of `sboo-color-name'.

Inputs:

• FILE — a « .json » file.
• STRING — a json string.

Output:

• a list of `sboo-color-name's.

Example:

• M-: (sboo-color-parse-json :json-string \"[{\\\"name\\\":\\\"1975 Earth Red\\\",\\\"hex\\\":\\\"#7a463a\\\"}]\")
    ⇒ [ ... ]

• M-: (sboo-color-parse-json :json-file \"colornames.json\")
    ⇒ [ ... ]

Related:

• `sboo-color/read-colornames.json'"

  (let* ()
    (cond

      ((stringp json-string)

       (with-temp-buffer (insert json-string)
                         (goto-char (point-min))
                         (json-read)))

      ((stringp json-file)

       (let* ((ALIST (sboo-color/read-colornames.json json-file))
              (TABLE (make-hash-table))
              )

         (dolist (PAIR ALIST)
           (let* ((COLOR (car PAIR))
                  (HEX   (cdr PAIR))
                  )
             (puthash COLOR HEX)))

         TABLE))

      (t (throw 'sboo-color-parse-json)))))

;; ^ e.g.:
;;
;; (sboo-color-parse-json :json-string "[{\"name\":\"1975 Earth Red\",\"hex\":\"#7a463a\"}]")
;; (sboo-color-parse-json :json-file   "colornames.json")
;;

;; e.g.
;;
;; M-: (with-temp-buffer (insert "[{\"name\":\"1975 Earth Red\",\"hex\":\"#7a463a\"}]") (json-read))
;;   ⇒ ((name . "1975 Earth Red")(hex . "#7a463a"))
;;
;; 
;;
  
;;----------------------------------------------;;

(defun sboo-color-names ()

  "Color names.

Output:

• a `sequencep' of `stringp's."

  (let* ((TABLE (sboo-colors))
         (KEYS  (hash-table-keys TABLE))
         )

    KEYS))

;; (sboo-color-names)

;;----------------------------------------------;;

(cl-defun sboo-colors (&key force)

  "Accessor for `sboo-colors-table' (and `sboo-colors-alist').

Output:

• a `hash-table-p' from `stringp's to `stringp's."

  (let* ()

    (progn

      (when (or force (not sboo-colors-table))

         ;TODO; or check/set a global flag, defaulting to X11's « colors.txt » (which isn't JSON).
        (let* ((FILE  sboo-color-json-file)
               (JSON  (sboo-color-parse-json :json-file FILE))  
               (TABLE (identity JSON))
               )
          (setq sboo-colors-table TABLE)))

      sboo-colors-table)))

;; (sboo-colors :force t)

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-read-color (&key force prompt require-match initial-input)

  "Read a color by name, returing a hex string.

Reads from `sboo-color-alist'.

Example:

• M-x sboo-read-color <RET>
    ⇒ ()

Links:

• URL `https://github.com/meodai/color-names#readme'

Related:

• `sboo-colors'"

  (interactive)

  (let ((PROMPT        (format "%s: " (or prompt "Color")))
        (REQUIRE-MATCH (or require-match t))
        (INITIAL-INPUT (or initial-input ""))

        (CANDIDATES    (sboo-colors :force force))
        ;; TODO colorize strings with hex in minibuffer??
        )

    (let* ((STRING (completing-read PROMPT CANDIDATES nil REQUIRE-MATCH INITIAL-INPUT))
           (COLOR  STRING)
           )

      COLOR)))

;; (sboo-read-color)

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;; sboo-color/*

(defun sboo-color/read-colornames.json (filename &key)

  "Read (and parse) FILENAME a « colornames.json » file.

Notes:

• the JSON Schema of \"colornames.json\" is like « [ { NAME: HEX } ] ».

Links:

• URL `https://unpkg.com/color-name-list@3.64.0/dist/colornames.json'"

  (let* ((json-key-type    'string)
         (json-array-type  'vector)
         (json-object-type 'alist)
         )

    (json-read-file filename)))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;;
;; 
;;
;;==============================================;;
(provide 'sboo-color)