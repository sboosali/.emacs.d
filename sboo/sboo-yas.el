;;; sboo-yas.el --- My ‘yasnippet’ utilities -*- lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 03 May 2019
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

;; Personal ‘yasnippet’ utilities for scripting Snippets.
;;
;; ‘sboo-yas-*’ functions include:
;;
;; • For the ‹# condition:› Snippet Header:
;;
;; • ‘sboo-yas-column-p’
;; • ‘sboo-yas-inside-comment-p’
;; • ‘sboo-yas-inside-string-p’
;; • ‘sboo-yas-inside-doc-p’
;;
;; • For inserting the current file(/module/package/library)-name:
;;
;; • ‘sboo-yas-get-elisp-namespace’
;; • ‘sboo-yas-get-basemame’
;;
;; • For inserting timestamps:
;;
;; • ‘sboo-yas-date-for-created-header’
;; • ‘sboo-yas-human-readable-date’
;; • ‘sboo-yas-machine-readable-date’
;;
;; Links:
;;
;; • URL `https://joaotavora.github.io/yasnippet/snippet-development.html' 
;; • URL `'
;;
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile 
  (require 'cl-lib))

(progn
  (require 'pcase)
  (require 'seq))

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-yas-column-p (&key key (indentation 4))

  "A predicate for a `yasnippet' « condition: » property.

Inputs:

• KEY — the `yasnippet' key (to be expanded).

Output:

• a boolean — Compares `current-column' against the `string-width' of KEY.

Example (yasnippet header):

    # -*- mode: snippet -*-
    # key         : defcustom
    # condition   : (let ((KEY \"defcustom\")) (condition-case nil (sboo-yas-column-p :key KEY :indentation 0) (void-function (= (current-column) (string-width KEY)))))
    # --

Notes (implementation):

• `string-width' — the number of columns the string is displayed across (in the current buffer). TAB chars display across `tab-width' columns.

Links:

• Info node `(elisp) Handling Errors'."

  (let* ((COLUMN      (current-column))
         (LENGTH      (string-width key))
         )

    (and (equal (thing-at-point 'symbol) key)

         (or (= LENGTH COLUMN)

        (when (and indentation (numberp indentation) (> indentation 0))
          (and (>= (- COLUMN LENGTH) 0)
               (<= (- COLUMN LENGTH) indentation)))))))

;;----------------------------------------------;;

(defun sboo-yas-get-basemame (&optional name)

  "Extract the namespace of an elisp file named NAME.

Inputs:

• NAME — a `stringp'.
  defaults to `buffer-file-name' or `buffer-name'.
  a file's basename. 
  leading directories and/or trailing file extensions are stripped.

Output:

• a `stringp'.

Examples:

• M-: (sboo-yas-get-basemame)
     → \"sboo-yas\")"

  (let* ((FILENAME (or name (buffer-file-name) (buffer-name)))
         (BASENAME (file-name-base FILENAME))
         )

    BASENAME))

;; ^ e.g.:
;;
;; • M-: (sboo-yas-get-basemame)
;;     → "sboo-yas"
;;

;;----------------------------------------------;;

(defun sboo-yas-get-elisp-namespace (&optional name)

  "Extract the namespace of an elisp file named NAME.

Inputs:

• NAME — a `stringp'.
  defaults to `buffer-file-name' or `buffer-name'.
  a file's basename. 
  leading directories and/or trailing file extensions are stripped.
  heuristically, “-mode” is stripped.

Output:

• a `stringp'.

Examples:

• M-: (sboo-yas-get-elisp-namespace)
     → \"sboo-yas\")
• M-: (sboo-yas-get-elisp-namespace \"xyz.el\")
     → \"xyz\")
• M-: (sboo-yas-get-elisp-namespace \"~/xyz-mode\")
     → \"xyz\")
"

  (let* ((FILENAME (or name (buffer-file-name) (buffer-name)))
         (BASENAME (file-name-base FILENAME))
         (NAME     (string-trim BASENAME nil "-mode"))
         )

    NAME))

;; ^ e.g.:
;;
;; • M-: (sboo-yas-get-elisp-namespace)
;;     → "sboo-yas"
;; • M-: (sboo-yas-get-elisp-namespace "xyz.el")
;;     → "xyz"
;; • M-: (sboo-yas-get-elisp-namespace "~/xyz-mode")
;;     → "xyz"
;;

;;----------------------------------------------;;

(defun sboo-yas-get-sboo-namespace (&optional name)

  "Extract the namespace of a personal configuration file.

Inputs:

• NAME — a `stringp'.
  defaults to `buffer-file-name' or `buffer-name'.
  a file's basename. 
  heuristically, a leading “sboo-” is stripped.

Output:

• a `stringp'.

Examples:

• M-: (sboo-yas-get-sboo-namespace)
     → \"yasnippets\")"

  (let* ((FILENAME (or name (buffer-file-name) (buffer-name)))
         (BASENAME (file-name-base FILENAME))
         (NAME     (string-trim BASENAME "sboo-" nil))
         )

    NAME))

;;==============================================;;

(cl-defun sboo-yas-machine-readable-date (&key time)

  "Return TIME formatted like « YYYY-MM-DD ».

Inputs:

• TIME — nil or a `listp' of `integerp's (a Timestamp).
  Defaults to `current-time'.

Output:

• a `stringp'.

Example:

• M-: (sboo-yas-machine-readable-date :time '(23825 56775 693364 288000))
    ⇒ \"2019-06-25\""

  (interactive (list :time nil))

  (let* ((TIME (or time (current-time))))

    (format-time-string "%Y-%m-%d" TIME)))

;;----------------------------------------------;;

(cl-defun sboo-yas-human-readable-date (&key time date)

  "Return TIME formatted like « 20 Jan 2016 ».

Inputs:

• TIME — nil or a `listp' of `integerp's (a Timestamp).
  When `called-interactively-p', defaults to `current-time'.

• DATE — nil or a `stringp'.

Output:

• a `stringp'.

Example:

• M-: (sboo-yas-human-readable-date :time '(23825 56775 693364 288000))
    ⇒ \"25 Jun 2019\"

• M-: (sboo-yas-human-readable-date :date \"2019-06-25\")
    ⇒ \"25 Jun 2019\"

Related:

• Parses `sboo-yas-human-readable-date'."

  (interactive (list :time (current-time)
                     :date nil))

  (if time
      (format-time-string "%d %b %Y" time)

    (error "TODO")))

    ;TODO:
    ;; (cl-destructuring-bind (_ _ _ DD MM YYYY _ _ _) (parse-time-string date)
    ;;   (format "%d %b %Y" DD MM YYYY))))

;; M-: (sboo-yas-human-readable-date :date "2019-06-25")
;;   ⇒ "25 Jun 2019"

;; `parse-time-string':
;;
;;   (parse-time-string STRING)
;;
;;   Parse the time-string STRING into (SEC MIN HOUR DAY MON YEAR DOW DST TZ).
;;
;; M-: (parse-time-string "2019-06-25")
;;   ⇒ (nil nil nil 25 6 2019 nil nil nil)
;;
;;

;; `cl-destructuring-bind':
;;
;;   (cl-destructuring-bind ARGS EXPR &rest BODY))
;;
;;   Parse the time-string STRING into (SEC MIN HOUR DAY MON YEAR DOW DST TZ).
;;
;; M-: (cl-destructuring-bind (_ _ _ DD MM YYYY _ _ _) (parse-time-string "2019-06-25") (list DD MM YYYY))
;;   ⇒ (25 6 2019)
;;
;;

;;==============================================;;

(define-inline sboo-yas-inside-comment-p ()

  "Whether `point' is within a Comment (w.r.t. the Syntax Table)."

  (if (nth 4 (syntax-ppss)) t nil))

;;----------------------------------------------;;

(define-inline sboo-yas-inside-string-p ()

  "Whether `point' is within a String (w.r.t. the Syntax Table)."

  (if (nth 6 (syntax-ppss)) t nil))

;;----------------------------------------------;;

(define-inline sboo-yas-inside-doc-p ()

  "Whether `point' is within a Docstring.
out
TODO improve heuristic from String-or-Comment."

  (or (sboo-yas-inside-string-p) (sboo-yas-inside-comment-p)))

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-yas-date-for-created-header (&key time insertp)

  "Return TIME formatted for the « ;; Created:  » library header.

Input:

• nil, or a `listp' of `numberp's.
  defaults to `current-time' (today's date).
• a `booleanp'.
  whether to `insert' the output (as well as return it).
  t when called `interactive'ly.

Output:

• a `stringp'.

Example:

• M-: (sboo-yas-date-for-created-header)
    ⇒ \"01 May 2019\"

Links:

• URL `https://www.emacswiki.org/emacs/InsertingTodaysDate'
• URL `http://ergoemacs.org/emacs/elisp_datetime.html'

Related:

• `format-time-string'"

  (interactive (list :time nil
                     :insertp t))

  (let* ((TIME   (or time (current-time)))
         (STRING (format-time-string "%d %b %Y" TIME))
         )

    (when insertp
      (insert STRING))

    STRING))

;; ^ `format-time-string' Month Formats:
;;
;; ;; full month name:
;; (format-time-string "%B")
;; ⇒ "November"
;;
;; ;; abbreviated month name:
;; M-: (format-time-string "%b")
;; ⇒ "Nov"
;;

;;----------------------------------------------;;

(cl-defun sboo-yas-file-name-for-first-line (&key buffer insertp)

  "Return BUFFER's filename, formatted for a library file's first line.

Input:

• a `bufferp' (or `stringp').
  defaults to the `current-buffer'.
• a `booleanp'.
  whether to `insert' the output (as well as return it).
  t when called `interactive'ly.

Output:

• a `stringp'.

Example:

• M-: (sboo-yas-file-name-for-first-line)
    ; /home/sboo/.emacs.d/sboo/sboo-yas.el
    ⇒ \"sboo-yas\"

Links:

• URL `'

Related:

• `file-name-base'
• `buffer-file-name'"

  (interactive (list :buffer nil
                     :insertp t))

  (let* ((BUFFER (or buffer (current-buffer)))
         (STRING (file-name-base (buffer-file-name BUFFER)))
         )

    (when insertp
      (insert STRING))

    STRING))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;; `yas-insert-snippet':
;;
;; With prefix argument NO-CONDITION, bypass filtering of snippets
;; by condition.
;;

;; `yas-wrap-around-region':
;; 
;; What to insert for snippet’s $0 field.
;; 
;; • If set to a character, insert contents of corresponding register.
;; 
;;   This can be overridden on a per-snippet basis.
;;   A value of ‘cua’ is considered equivalent to ‘?0’ for backwards compatibility.
;; 
;; • If non-nil insert region contents.
;;

;; `yas-next-field-or-maybe-expand':
;; 
;; If a `key' is before `point', try to expand a snippet (`yas-expand').
;; Otherwise, delegate to `yas-next-field'.
;;

;; `yas-reload-all':
;;
;;     (yas-reload-all &optional NO-JIT)
;;
;; When `NO-JIT' is non-`nil', force immediate reload of all known snippets under ‘yas-snippet-dirs’.
;; Otherwise, use just-in-time loading.
;;

;; `yas-snippet-dirs':
;;
;; Each item in this list is a top-level directory,
;; which holds per-mode snippet directories.
;;
;; e.g.
;;
;;     $ find -L ~/.emacs.d/sboo/snippets/ -type f -name '*.yasnippet'
;;     ~/.emacs.d/sboo/snippets/haskell-mode/*.yasnippet
;;     ~/.emacs.d/sboo/snippets/emacs-lisp-mode/*.yasnippet
;;     ~/.emacs.d/sboo/snippets/...
;;

;; `yas-key-syntaxes':
;;
;; (setq yas-key-syntaxes '("w_" "w_." "^ "))
;;
;; default is (yas-try-key-from-whitespace "w_.()" "w_." "w_" "w") (circa 2019-03-12).
;;

;; « # uuid: unique identifier »
;;
;; > This provides to a way to identify a snippet, independent of its name. Loading a second snippet file with the same uuid would replace the previous snippet.
;;
;; « group: snippet menu grouping »
;;
;; > When expanding/visiting snippets from the menu-bar menu, snippets for a given mode can be grouped into sub-menus . This is useful if one has too many snippets for a mode which will make the menu too long.
;; The # group: property only affect menu construction (See the YASnippet menu) and the same effect can be achieved by grouping snippets into sub-directories and using the .yas-make-groups special file (for this see Organizing Snippets
;;

;; See:
;;
;; • URL `http://joaotavora.github.io/yasnippet'
;; • URL `http://ergoemacs.org/emacs/emacs_tip_yasnippet_expand_whole_hyphenated_word.html'
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-yas)

;;; sboo-yas.el ends here