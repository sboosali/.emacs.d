;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; Configuration for SQL.
;; 
;; • 
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
(require 'pcase)
(require 'seq)

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Customization -------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-sql

  nil

  "Personal SQL customization."

  :prefix "sboo-sql-"

  :group 'sboo
  :group 'sql)

;;==============================================;;

(defcustom sboo-sql-compile-command-default

  #'sboo-postgresql-compile-command-default

  "Default `compile-command' for `sql-mode' buffers.

a `stringp' or `functionp'."

  :type '(choice (string :tag "Command-Line")
                 (function))

  :safe #'stringp
  :group 'sboo-sql)

;;----------------------------------------------;;

(defcustom sboo-sql-prettify-symbols-alist

  '(
    ("=="     . ?≡)
    ("/="     . ?≠)
    ("<="     . ?≤)
    (">="     . ?≥)

    ("->"     . ?→)
    ("->>"    . ?⤁)
    )

  "`prettify-symbols-alist' for Sql.

Prettify some Applicative methods 
(á la Applicative Programming with Effects).

Extends `sql-font-lock-symbols-alist'.

Links:

• URL `http://xahlee.info/comp/unicode_arrows.html'"

  :type '(alist :key-type   (string    :tag "String to match")
                :value-type (character :tag "Char to show")
                )

  :safe t
  :group 'sboo-sql)

;;----------------------------------------------;;

(defcustom sboo-sql-font-lock-alist

  '(
    ("TODO" . font-lock-warning-face)
    )

  "Extra keywords (to fontify) for SQL files.

Associates regexps with faces.

(This only affects display, not navigation.)"

  :type '(alist :key-type   (regexp)
                :value-type (string :tag "Face"))

  :safe #'listp
  :group 'sboo-sql)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;; ^ `font-lock-builtin-face'

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-postgresql-compile-command-default (&optional file)

  "Default `compile-command' for `sql-mode'.

Links:

• URL `https://www.postgresql.org/docs/9.5/app-psql.html'

Related:

• `'"

  (interactive)

  (let* ((FILE (or file buffer-file-name (read-file-name "SQL File: ")))
         )

    (format-message "psql --file=%s"
                    FILE)))

;;----------------------------------------------;;

(defun sboo-sql-font-lock-add-keywords ()

  "Call `font-lock-add-keywords'."

  (let* ((KEYWORD-ALIST sboo-sql-font-lock-alist)
         (KEYWORD-LIST  KEYWORD-ALIST)
         )

    (font-lock-add-keywords mode KEYWORD-LIST)))

;;----------------------------------------------;;

(defun sboo-sql-prettify-symbols ()

  "Extend `prettify-symbols-alist' with `sboo-sql-prettify-symbols-alist'."

  (interactive)

  (if prettify-symbols-mode

      (prettify-symbols-mode 0)

    (progn
      (setq-local prettify-symbols-alist sboo-sql-prettify-symbols-alist)

      (prettify-symbols-mode +1))))

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 
;;==============================================;;
(provide 'sboo-sql)