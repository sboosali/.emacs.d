;;; sboo-syntax.el --- Definitions for Syntax Tables and Syntax Classes
;;; -*- lexical-binding: t; -*-

;;==============================================;;
;; Copyright (C) GPL-3.0-or-later 2019 Spiros Boosalis

;; Author: Spiros Boosalis <samboosalis@gmail.com>
;; Maintainer: Spiros Boosalis <samboosalis@gmail.com>
;; Version: 0.0.0
;; URL: https://github.com/sboosali/sboo-syntax
;; Keywords:
;; Package-Requires: ((cl-lib "0.5") (pkg-info "0.4") (emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;;==============================================;;
;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;==============================================;;
;;; Commentary:

;; Definitions for Syntax Tables and Syntax Classes.
;;
;; Commands provided:
;;
;; • `sboo-syntax-'
;;
;; Variables provided:
;;
;; • `sboo-syntax-'
;;
;; Bugs: https://github.com/sboosali/sboo-syntax/issues
;;
;; History: https://github.com/sboosali/sboo-syntax/blob/master/CHANGELOG.md
;;

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;;; Requirements -------------------------------;;
;;----------------------------------------------;;

(progn

  (require 'seq)
  (require 'subr-x)
  (require 'pcase))

;;----------------------------------------------;;

(eval-when-compile

  (require 'cl-lib))

;;----------------------------------------------;;
;; Constants -----------------------------------;;
;;----------------------------------------------;;

(defconst sboo-syntax-class-whitespace ?\-

  "Syntax Class for whitespace characters.")

;;----------------------------------------------;;

(defconst sboo-syntax-class-word ?\w

  "Syntax Class for (natural language) letters. 

Letters include an alphabet's phonograms, a (non-phonetic) writing system's logograms. For example: the Latin alphabet (i.e. letters A to Z in both uppercase and lowercase); CJKV (i.e. the Chinese / Japanese / Korean / Vietnamese character sets).")

;;----------------------------------------------;;

(defconst sboo-syntax-class-symbol ?\_ 

  "Syntax Class for (programming language) identifier characters. 

The union of the `sboo-syntax-class-symbol' and `sboo-syntax-class-whitespace' classes determine a programing language “identifier”.")

;;----------------------------------------------;;

(defconst sboo-syntax-class-punctuation ?\.

  "Syntax Class for Punctuation characters.")

;;----------------------------------------------;;

(defconst sboo-syntax-class-string-delimiter ?\"

  "Syntax Class for String Delimiter characters.")

;;----------------------------------------------;;

(defconst sboo-syntax-class-left-bracket ?\(

  "Syntax Class for Opening/Left Bracket characters.")

;;----------------------------------------------;;

(defconst sboo-syntax-class-right-bracket ?\)

  "Syntax Class for Closing/Right Bracket characters.")

;;----------------------------------------------;;

(defconst sboo-syntax-class-comment-start ?\<

  "Syntax Class for Comment Start characters.")

;;----------------------------------------------;;

(defconst sboo-syntax-class-comment-end ?\>

  "Syntax Class for Comment End characters.")

;;----------------------------------------------;;

(defconst sboo-syntax-class-escape ?\\

  "Syntax Class for Escaping characters.")

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-syntax/list-of-char-entries-p (entries)

  "Return t if ENTRIES is a `listp' of: `characterp's, and/or `consp's of `characterp's."

  (and (listp entries)
       (seq-every-p #'sboo-syntax/char-entry-p entries)))

;;----------------------------------------------;;

(defun sboo-syntax/char-entry-p (entry)

  "Return t if ENTRY is: a `characterp's; or a `consp' of `characterp's.

Examples:

M-: (sboo-syntax/char-entry-p ?<)
  ⇒ t

M-: (sboo-syntax/char-entry-p '(?< . ?>))
  ⇒ t

M-: (sboo-syntax/char-entry-p '(?« ?»))
  ⇒ t

M-: (sboo-syntax/char-entry-p \"<\")
  ⇒ nil

M-: (sboo-syntax/char-entry-p '(?1 ?2 ?3))
  ⇒ nil"

  (pcase entry

    ((and (pred characterp) x)
     t)

    (`(,(and (pred characterp) x) . ,(and (pred characterp) y))
     t)

    (`(,(and (pred characterp) x) ,(and (pred characterp) y))
     t)

    (_ nil)))

;; e.g.:
;;
;; (pcase '(?< . ?>) (`(,(and (pred characterp) x) . ,(and (pred characterp) y)) t) ((and (pred characterp) x) t) (_ nil)) ; t
;;
;; (sboo-syntax/char-entry-p "<")
;;

;;----------------------------------------------;;

(defun sboo-syntax/optional-char-table-p (table)

  "Return t if TABLE is: a `char-table-p' or nil."

  (or (eq nil table)
      (char-table-p table)))

;;----------------------------------------------;;
;;; Customization ------------------------------;;
;;----------------------------------------------;;

;;;###autoload
(defgroup sboo-syntax nil

  "Definitions for Syntax Tables and Syntax Classes."

  :link '(url-link :tag "GitHub" "https://github.com/sboosali/sboo-syntax#readme")

;;  :group '
  :prefix "sboo-syntax-")

;;==============================================;;

(defcustom sboo-syntax-brackets-list

  '(
    (?< . ?>)

    (?« . ?»)

    ?|
   )

  "Associates bracket characters with their matching character."

  :type '(repeat (choice (character                :tag "Self-Bracket")
                           (cons character character :tag "    Brackets")
                           (list character character :tag "    Brackets")
                           ))

  :safe #'sboo-syntax/list-of-char-entries-p
  :group 'sboo-syntax)

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-syntax-add-bracket-entries (brackets &key syntax-table)

  "Register (with SYNTAX-TABLE) the bracket-character(s) in BRACKETS.

Input:

• BRACKETS — a `listp'.
  each entry is either: a `consp' (c.f. an Association List) of `characterp's; or a `characterp' (which is interpreted as a pair of that same character duplicated).
• SYNTAX-TABLE — an optional `char-table-p'.
  a syntax table.

Effect:

• Calls `sboo-syntax-add-bracket-entry' on each entry in BRACKETS.

Examples:

• M-: (sboo-syntax-add-bracket-entries '((?< . ?>) (?« . ?»)) :syntax-table standard-syntax-table)"

  (cl-assert (sboo-syntax/list-of-char-entries-p brackets) t)
  (cl-assert (sboo-syntax/optional-char-table-p syntax-table) t)

  (let* ((SYNTAX-DESCRIPTION-OPENING (string ?\( closing))
         (SYNTAX-DESCRIPTION-CLOSING (string ?\) opening))

         (ENTRIES (sboo-syntax/normalize-entries brackets))
         )

    (dolist (ENTRY ENTRIES)

      (pcase ENTRY

        (`(,(and (pred characterp) OPENING) . ,(and (pred characterp) CLOSING))

         (sboo-syntax-add-bracket-entry OPENING CLOSING :syntax-table syntax-table))

        (_ nil)))

      (or syntax-table (syntax-table))))

;;----------------------------------------------;;

(cl-defun sboo-syntax-add-bracket-entry (opening closing &key syntax-table)

  "Associate characters OPENING and CLOSING as brackets (in SYNTAX-TABLE).

Input:

• OPENING — a `characterp'.
  the opening (or left) bracket character.
• CLOSING — a `characterp'.
  the closing (or right) bracket character.
• SYNTAX-TABLE — an optional `char-table-p'.
  a syntax table.

Examples:

• M-: (sboo-syntax-add-bracket-entry ?< ?>)
• M-: (sboo-syntax-add-bracket-entry ?« ?» :syntax-table standard-syntax-table)

Related:

• `modify-syntax-entry'"

  (cl-assert (characterp opening) t)
  (cl-assert (characterp closing) t)

  (let* ((SYNTAX-DESCRIPTION-OPENING (string ?\( closing))
         (SYNTAX-DESCRIPTION-CLOSING (string ?\) opening))
         )

    (modify-syntax-entry opening SYNTAX-DESCRIPTION-OPENING syntax-table)
    (modify-syntax-entry closing SYNTAX-DESCRIPTION-CLOSING syntax-table)

    (or syntax-table (syntax-table))))

;;----------------------------------------------;;

(defun sboo-syntax/normalize-entries (entries)

  "Return (a copy of) ENTRIES, each entry being a homogeneous `consp'.

Input:

• ENTRIES — a `listp', satisfying `sboo-syntax/list-of-char-entries-p'.

Examples:

M-: (sboo-syntax/normalize-entries '(?| (?< . ?>) (?« ?»)))
  ⇒ '((?| . ?|) (?< . ?>) (?« . ?»))"

  (let* ((ENTRIES-NORMALIZED '())
         )

    (dolist (ENTRY entries)

      (let* ((ENTRY-NORMALIZED (sboo-syntax/normalize-entry ENTRY))
             )

        (when ENTRY-NORMALIZED
          (push ENTRY-NORMALIZED ENTRIES-NORMALIZED))))

    (nreverse ENTRIES-NORMALIZED)))

;;----------------------------------------------;;

(defun sboo-syntax/normalize-entry (entry)

  "Return ENTRY as a homogeneous `consp' (if normalizeable).

Input:

• ENTRIES — a `consp', satisfying `sboo-syntax/char-entry-p'.

Examples:

M-: (sboo-syntax/normalize-entry ?|)
  ⇒ '(?| . ?|)

M-: (sboo-syntax/normalize-entry '(?< . ?>))
  ⇒ '(?< . ?>)

M-: (sboo-syntax/normalize-entry '(?« ?»))
  ⇒ '(?« . ?»)"

  (pcase entry

    ((and (pred characterp) x)
     (cons x x))

    (`(,(and (pred characterp) x) . ,(and (pred characterp) y))
     (cons x y))

    (`(,(and (pred characterp) x) ,(and (pred characterp) y))
     (cons x y))

    (_ nil)))

;;----------------------------------------------;;
;;; Commands -----------------------------------;;
;;----------------------------------------------;;



;;==============================================;;
(provide 'sboo-syntax)

;; Local Variables:
;; End:

;;; sboo-syntax.el ends here