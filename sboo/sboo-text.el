;;; sboo-text.el --- -*- lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 06 May 2019
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

;; Configure `text-mode' (and any `derived-mode-p's).
;; 
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile 
  (require 'cl-lib))

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-text nil

  "Personal `text-mode' configuration."

  :prefix 'sboo
  :group 'sboo)

;;----------------------------------------------;;

(defgroup sboo-conf nil

  "Personal `conf-mode' configuration."

  :prefix 'sboo

  :group 'conf
  :group 'sboo-text)

;;==============================================;;

(defcustom sboo-aspell-conf-keywords

  '(
    "lang"
    "conf"
    "conf-dir"
    "data-dir"
    "dict-alias"
    "dict-dir"
    "encoding"
    "filter"
    "filter-path"
    "mode"
    "extra-dicts"
    "home-dir"
    "ignore"
    "ignore-case"
    "ignore-repl"
    "keyboard"
    "local-data-dir"
    "master"
    "normalize"
    "norm-required"
    "norm-form"
    "norm-strict"
    "per-conf"
    "personal"
    "prefix"
    "repl"
    "run-together"
    "run-together-limit"
    "run-together-min"
    "save-repl"
    "set-prefix"
    "size"
    "sug-mode"
    "sug-edit-dist"
    "sug-typo-analysis"
    "sug-repl-table"
    "sug-split-char"
    "use-other-dicts"
    "variety"
    "warn"
    "affix-compress"
    "clean-affixes"
    "clean-words"
    "invisible-soundslike"
    "partially-expand"
    "skip-invalid-words"
    "validate-affixes"
    "validate-words"
    "backup"
    "byte-offsets"
    "guess"
    "keymapping"
    "reverse"
    "suggest"
    "time"
    "f-tex-check-comments"
    "f-tex-command"
    "f-texinfo-ignore"
    "f-texinfo-ignore-env"
    "f-context-delimiters"
    "f-context-visible-first"
    "f-email-quote"
    "f-email-margin"
    "f-html-check"
    "f-html-skip"
    "f-sgml-check"
    "f-sgml-skip"
    )

  "Keywords for « aspell.conf ».

Zero-or-more `stringp's or `regexpp's.

Used by:

• ‘sboo-aspell-conf’"

  :type '(repeated (choice (string :tag "Keyword")
                           (regexp :tag "Keyword Regexp")
                           ))

  :safe #'listp
  :group 'sboo-conf)

;;----------------------------------------------;;

;; `ispell-mode'

;;----------------------------------------------;;

(defconst sboo-aspell-conf-example-string

  "#------------------------------------------------#\n\n# lang (string)\n#   language code\n# default: <language-tag> = en_US\n\nlang en\n\n#------------------------------------------------#\n\n# conf (string)\n#   main configuration file\n# default: aspell.conf\n\n#------------------------------------------------#\n\n# conf-dir (string)\n#   location of main configuration file\n# default: /etc\n\n#------------------------------------------------#\n\n# data-dir (string)\n#   location of language data files\n# default: <prefix:lib/aspell> = /usr/lib/aspell\n\n#------------------------------------------------#\n\n# dict-alias (list)\n#   create dictionary aliases\n\n#------------------------------------------------#\n\n# dict-dir (string)\n#   location of the main word list\n# default: <prefix:lib/aspell> = /usr/lib/aspell\n\n#------------------------------------------------#\n\n# encoding (string)\n#   encoding to expect data to be in\n# default: !encoding = UTF-8\n\n#------------------------------------------------#\n\n# filter (list)\n#   add or removes a filter\n\nfilter html\n\n#------------------------------------------------#\n\n# filter-path (list)\n#   path(s) aspell looks for filters\n\n#------------------------------------------------#\n\n# mode (string)\n#   filter mode\n# default: url\n\n#------------------------------------------------#\n\n# extra-dicts (list)\n#   extra dictionaries to use\n\n#------------------------------------------------#\n\n# home-dir (string)\n#   location for personal files\n# default: <$HOME|./> = /home/sboo\n\n#------------------------------------------------#\n\n# ignore (integer)\n#   ignore words <= n chars\n# default: 1\n\n#------------------------------------------------#\n\n# ignore-case (boolean)\n#   ignore case when checking words\n# default: false\n\nignore-case true\n\n#------------------------------------------------#\n\n# ignore-repl (boolean)\n#   ignore commands to store replacement pairs\n# default: false\n\n#------------------------------------------------#\n\n# keyboard (string)\n#   keyboard definition to use for typo analysis\n# default: standard\n\n#------------------------------------------------#\n\n# local-data-dir (string)\n#   location of local language data files\n# default: <actual-dict-dir> = /usr/lib/aspell/\n\n#------------------------------------------------#\n\n# master (string)\n#   base name of the main dictionary to use\n# default: <lang> = en\n\n#------------------------------------------------#\n\n# normalize (boolean)\n#   enable Unicode normalization\n# default: true\n\n#------------------------------------------------#\n\n# norm-required (boolean)\n#   Unicode normalization required for current lang\n# default: false\n\n#------------------------------------------------#\n\n# norm-form (string)\n#   Unicode normalization form: none, nfd, nfc, comp\n# default: nfc\n\n#------------------------------------------------#\n\n# norm-strict (boolean)\n#   avoid lossy conversions when normalization\n# default: false\n\n#------------------------------------------------#\n\n# per-conf (string)\n#   personal configuration file\n# default: .aspell.conf\n\n#------------------------------------------------#\n\n# personal (string)\n#   personal dictionary file name\n# default: .aspell.<lang>.pws = .aspell.en.pws\n\n#------------------------------------------------#\n\n# prefix (string)\n#   prefix directory\n# default: /usr\n\n#------------------------------------------------#\n\n# repl (string)\n#   replacements list file name\n# default: .aspell.<lang>.prepl = .aspell.en.prepl\n\n#------------------------------------------------#\n\n# run-together (boolean)\n#   consider run-together words legal\n# default: false\n\n#------------------------------------------------#\n\n# run-together-limit (integer)\n#   maximum number that can be strung together\n# default: 2\n\n#------------------------------------------------#\n\n# run-together-min (integer)\n#   minimal length of interior words\n# default: 3\n\n#------------------------------------------------#\n\n# save-repl (boolean)\n#   save replacement pairs on save all\n# default: true\n\n#------------------------------------------------#\n\n# set-prefix (boolean)\n#   set the prefix based on executable location\n# default: true\n\n#------------------------------------------------#\n\n# size (string)\n#   size of the word list\n# default: +60\n\n#------------------------------------------------#\n\n# sug-mode (string)\n#   suggestion mode\n# default: normal\n\n#------------------------------------------------#\n\n# sug-edit-dist (integer)\n#   edit distance to use, override sug-mode default\n# default: 1\n\n#------------------------------------------------#\n\n# sug-typo-analysis (boolean)\n#   use typo analysis, override sug-mode default\n# default: true\n\n#------------------------------------------------#\n\n# sug-repl-table (boolean)\n#   use replacement tables, override sug-mode default\n# default: true\n\n#------------------------------------------------#\n\n# sug-split-char (list)\n#   characters to insert when a word is split\n\n#------------------------------------------------#\n\n# use-other-dicts (boolean)\n#   use personal, replacement & session dictionaries\n# default: true\n\n#------------------------------------------------#\n\n# variety (list)\n#   extra information for the word list\n\n#------------------------------------------------#\n\n# warn (boolean)\n#   enable warnings\n# default: true\n\n#------------------------------------------------#\n\n# affix-compress (boolean)\n#   use affix compression when creating dictionaries\n# default: false\n\n#------------------------------------------------#\n\n# clean-affixes (boolean)\n#   remove invalid affix flags\n# default: true\n\n#------------------------------------------------#\n\n# clean-words (boolean)\n#   attempts to clean words so that they are valid\n# default: false\n\n#------------------------------------------------#\n\n# invisible-soundslike (boolean)\n#   compute soundslike on demand rather than storing\n# default: false\n\n#------------------------------------------------#\n\n# partially-expand (boolean)\n#   partially expand affixes for better suggestions\n# default: false\n\n#------------------------------------------------#\n\n# skip-invalid-words (boolean)\n#   skip invalid words\n# default: true\n\n#------------------------------------------------#\n\n# validate-affixes (boolean)\n#   check if affix flags are valid\n# default: true\n\n#------------------------------------------------#\n\n# validate-words (boolean)\n#   check if words are valid\n# default: true\n\n#------------------------------------------------#\n\n# backup (boolean)\n#   create a backup file by appending \".bak\"\n# default: true\n\n#------------------------------------------------#\n\n# byte-offsets (boolean)\n#   use byte offsets instead of character offsets\n# default: false\n\n#------------------------------------------------#\n\n# guess (boolean)\n#   create missing root/affix combinations\n# default: false\n\n#------------------------------------------------#\n\n# keymapping (string)\n#   keymapping for check mode: \"aspell\" or \"ispell\"\n# default: aspell\n\n#------------------------------------------------#\n\n# reverse (boolean)\n#   reverse the order of the suggest list\n# default: false\n\n#------------------------------------------------#\n\n# suggest (boolean)\n#   suggest possible replacements\n# default: true\n\n#------------------------------------------------#\n\n# time (boolean)\n#   time load time and suggest time in pipe mode\n# default: false\n\n#------------------------------------------------#\n# Filter: TeX/LaTeX -----------------------------#\n#------------------------------------------------#\n\n#\n# Filter: tex\n#   filter for dealing with TeX/LaTeX documents\n#\n# configured as follows:\n\n#------------------------------------------------#\n\n# f-tex-check-comments (boolean)\n#   check TeX comments\n# default: false\n\n#------------------------------------------------#\n\n# f-tex-command (list)\n#   TeX commands\n\n#------------------------------------------------#\n# Filter: Texinfo -------------------------------#\n#------------------------------------------------#\n\n#\n# Filter: texinfo\n#   filter for dealing with Texinfo documents\n#\n# configured as follows:\n\n#------------------------------------------------#\n\n# f-texinfo-ignore (list)\n#   Texinfo commands to ignore the parameters of\n\n#------------------------------------------------#\n\n# f-texinfo-ignore-env (list)\n#   Texinfo environments to ignore\n\n#------------------------------------------------#\n# Filter: context -------------------------------#\n#------------------------------------------------#\n\n#\n# Filter: context\n#   experimental filter for hiding delimited contexts\n#\n# configured as follows:\n\n#------------------------------------------------#\n\n# f-context-delimiters (list)\n#   context delimiters (separated by spaces)\n\n#------------------------------------------------#\n\n# f-context-visible-first (boolean)\n#   swaps visible and invisible text\n# default: false\n\n#------------------------------------------------#\n# Filter: Email ---------------------------------#\n#------------------------------------------------#\n\n#\n# Filter: email\n#   filter for skipping quoted text in email messages\n#\n# configured as follows:\n\n#------------------------------------------------#\n\n# f-email-quote (list)\n#   email quote characters\n\n#------------------------------------------------#\n\n# f-email-margin (integer)\n#   num chars that can appear before the quote char\n# default: 10\n\n#------------------------------------------------#\n# Filter: HTML ----------------------------------#\n#------------------------------------------------#\n\n#\n# Filter: html\n#   filter for dealing with HTML documents\n#\n# configured as follows:\n\n#------------------------------------------------#\n\n# f-html-check (list)\n#   HTML attributes to always check\n\n#------------------------------------------------#\n\n# f-html-skip (list)\n#   HTML tags to always skip the contents of\n\n#------------------------------------------------#\n# Filter: SGML/XML-------------------------------#\n#------------------------------------------------#\n\n#\n# Filter: sgml\n#   filter for dealing with generic SGML/XML documents\n#\n# configured as follows:\n\n#------------------------------------------------#\n\n# f-sgml-check (list)\n#   SGML attributes to always check\n\n#------------------------------------------------#\n\n# f-sgml-skip (list)\n#   SGML tags to always skip the contents of\n\n#------------------------------------------------#\n# EOF -------------------------------------------#\n#------------------------------------------------#"

  "Example « aspell.conf » file.")

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-text-config ()

  "Configure `text-mode'."

  (remove-hook 'text-mode-hook #'turn-on-auto-fill)

  (add-hook 'text-mode-hook #'turn-on-visual-line-mode)

  'text-mode-hook)

;;----------------------------------------------;;

(defun sboo-aspell-conf ()

  "‘conf-mode’ for « aspell.conf » files.

Uses:

• ‘sboo-aspell-conf-keywords’

Related:

• ‘conf-space-keywords’"

  (let* ((REGEX (sboo-aspell-conf-keywords-regexp sboo-aspell-conf-keywords))
         )

    (conf-space-keywords REGEX)))

;;----------------------------------------------;;

(defun sboo-aspell-conf-keywords-regexp (&optional keywords)

  "Return a `regexp' matching any `stringp' in KEYWORDS.

Inputs:

• KEYWORDS — a `listp' of `stringp's (or of `symbolp's).
  defaults to `sboo-aspell-conf-keywords'.

Output:

• a `regexp'.

Example:

• M-: (sboo-aspell-conf-keywords-regexp '(\"TODO\" \"NOTE\"))
    ⇒ 
"

  ;;(cl-assert (not (equal x y)) :show-args "the arguments must differ")
  ;;(cl-check-type n (integer 1 *) "a positive integer")

  (let* ()

    ))

;; M-: (sboo-aspell-conf-keywords-regexp '("TODO" "NOTE"))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; Schema for « aspell.conf » is:
;;
;; • "lang"                    — `stringp'
;; • "conf"                    — `stringp'
;; • "conf-dir"                — `stringp'
;; • "data-dir"                — `stringp'
;; • "dict-alias"              — `listp'
;; • "dict-dir"                — `stringp'
;; • "encoding"                — `stringp'
;; • "filter"                  — `listp'
;; • "filter-path"             — `listp'
;; • "mode"                    — `stringp'
;; • "extra-dicts"             — `listp'
;; • "home-dir"                — `stringp'
;; • "ignore"                  — `integerp'
;; • "ignore-case"             — `booleanp'
;; • "ignore-repl"             — `booleanp'
;; • "keyboard"                — `stringp'
;; • "local-data-dir"          — `stringp'
;; • "master"                  — `stringp'
;; • "normalize"               — `booleanp'
;; • "norm-required"           — `booleanp'
;; • "norm-form"               — `stringp'
;; • "norm-strict"             — `booleanp'
;; • "per-conf"                — `stringp'
;; • "personal"                — `stringp'
;; • "prefix"                  — `stringp'
;; • "repl"                    — `stringp'
;; • "run-together"            — `booleanp'
;; • "run-together-limit"      — `integerp'
;; • "run-together-min"        — `integerp'
;; • "save-repl"               — `booleanp'
;; • "set-prefix"              — `booleanp'
;; • "size"                    — `stringp'
;; • "sug-mode"                — `stringp'
;; • "sug-edit-dist"           — `integerp'
;; • "sug-typo-analysis"       — `booleanp'
;; • "sug-repl-table"          — `booleanp'
;; • "sug-split-char"          — `listp'
;; • "use-other-dicts"         — `booleanp'
;; • "variety"                 — `listp'
;; • "warn"                    — `booleanp'
;; • "affix-compress"          — `booleanp'
;; • "clean-affixes"           — `booleanp'
;; • "clean-words"             — `booleanp'
;; • "invisible-soundslike"    — `booleanp'
;; • "partially-expand"        — `booleanp'
;; • "skip-invalid-words"      — `booleanp'
;; • "validate-affixes"        — `booleanp'
;; • "validate-words"          — `booleanp'
;; • "backup"                  — `booleanp'
;; • "byte-offsets"            — `booleanp'
;; • "guess"                   — `booleanp'
;; • "keymapping"              — `stringp'
;; • "reverse"                 — `booleanp'
;; • "suggest"                 — `booleanp'
;; • "time"                    — `booleanp'
;; • "f-tex-check-comments"    — `booleanp'
;; • "f-tex-command"           — `listp'
;; • "f-texinfo-ignore"        — `listp'
;; • "f-texinfo-ignore-env"    — `listp'
;; • "f-context-delimiters"    — `listp'
;; • "f-context-visible-first" — `booleanp'
;; • "f-email-quote"           — `listp'
;; • "f-email-margin"          — `integerp'
;; • "f-html-check"            — `listp'
;; • "f-html-skip"             — `listp'
;; • "f-sgml-check"            — `listp'
;; • "f-sgml-skip"             — `listp'
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-text)

;;; sboo-text.el ends here