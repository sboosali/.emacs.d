;;; sboo-sql.el --- -*- lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 07 May 2019
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Personal configuration for the SQL expression language (family).
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile
  (require 'pcase)
  (require 'cl-lib))

;;----------------------------------------------;;

(progn
  (require 'seq))

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

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-sql)

;;; sboo-sql.el ends here