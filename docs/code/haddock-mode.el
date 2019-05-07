;;; haddock-mode.el --- Major mode for Haddock docstrings -*- lexical-binding: t; -*-

;;----------------------------------------------;;

;; Copyright (C) 2019- Sam Boosalis and haddock-mode
;; Copyright (C) 2007-2017 Jason R. Blevins and markdown-mode

;; Author: Sam Boosalis <samboosalis@gmail.com>
;; Maintainer: Sam Boosalis <samboosalis@gmail.com>
;; Created: 2019
;; Version: 0.0
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Keywords: Haskell, Haddock
;; URL: https://github.com/sboosali/haddock-mode/

;;----------------------------------------------;;

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;----------------------------------------------;;

;;; Commentary:

;; 

;;----------------------------------------------;;


;;; Code:

;;----------------------------------------------;;

(require 'easymenu)
(require 'outline)
(require 'thingatpt)
(require 'cl-lib)
(require 'url-parse)
(require 'button)
(require 'color)
(require 'rx)

;;----------------------------------------------;;

(defvar jit-lock-start)
(defvar jit-lock-end)
(defvar flyspell-generic-check-word-predicate)

;;----------------------------------------------;;

(declare-function eww-open-file "eww")
(declare-function url-path-and-query "url-parse")


;;; Constants =================================================================

(defconst haddock-mode-version "2.4-dev"
  "Haddock mode version number.")

(defconst haddock-output-buffer-name "*haddock-output*"
  "Name of temporary buffer for haddock command output.")


;;; Global Variables ==========================================================

(defvar haddock-reference-label-history nil
  "History of used reference labels.")

(defvar haddock-live-preview-mode nil
  "Sentinel variable for command `haddock-live-preview-mode'.")

(defvar haddock-gfm-language-history nil
  "History list of languages used in the current buffer in GFM code blocks.")


;;; Customizable Variables ====================================================

(defvar haddock-mode-hook nil
  "Hook run when entering Haddock mode.")

(defvar haddock-before-export-hook nil
  "Hook run before running Haddock to export XHTML output.
The hook may modify the buffer, which will be restored to it's
original state after exporting is complete.")

(defvar haddock-after-export-hook nil
  "Hook run after XHTML output has been saved.
Any changes to the output buffer made by this hook will be saved.")

(defgroup haddock nil
  "Major mode for editing text files in Haddock format."
  :prefix "haddock-"
  :group 'text
  :link '(url-link "https://jblevins.org/projects/haddock-mode/"))

(defcustom haddock-command "haddock"
  "Command to run haddock."
  :group 'haddock
  :type '(choice (string :tag "Shell command") function))

(defcustom haddock-command-needs-filename nil
  "Set to non-nil if `haddock-command' does not accept input from stdin.
Instead, it will be passed a filename as the final command line
option.  As a result, you will only be able to run Haddock from
buffers which are visiting a file."
  :group 'haddock
  :type 'boolean)

(defcustom haddock-open-command nil
  "Command used for opening Haddock files directly.
For example, a standalone Haddock previewer.  This command will
be called with a single argument: the filename of the current
buffer.  It can also be a function, which will be called without
arguments."
  :group 'haddock
  :type '(choice file function (const :tag "None" nil)))

(defcustom haddock-hr-strings
  '("-------------------------------------------------------------------------------"
    "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *"
    "---------------------------------------"
    "* * * * * * * * * * * * * * * * * * * *"
    "---------"
    "* * * * *")
  "Strings to use when inserting horizontal rules.
The first string in the list will be the default when inserting a
horizontal rule.  Strings should be listed in decreasing order of
prominence (as in headings from level one to six) for use with
promotion and demotion functions."
  :group 'haddock
  :type '(repeat string))

(defcustom haddock-bold-underscore nil
  "Use two underscores when inserting bold text instead of two asterisks."
  :group 'haddock
  :type 'boolean)

(defcustom haddock-italic-underscore nil
  "Use underscores when inserting italic text instead of asterisks."
  :group 'haddock
  :type 'boolean)

(defcustom haddock-marginalize-headers nil
  "When non-nil, put opening atx header markup in a left margin.

This setting goes well with `haddock-asymmetric-header'.  But
sadly it conflicts with `linum-mode' since they both use the
same margin."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.4"))

(defcustom haddock-marginalize-headers-margin-width 6
  "Character width of margin used for marginalized headers.
The default value is based on there being six heading levels
defined by Haddock and HTML.  Increasing this produces extra
whitespace on the left.  Decreasing it may be preferred when
fewer than six nested heading levels are used."
  :group 'haddock
  :type 'natnump
  :safe 'natnump
  :package-version '(haddock-mode . "2.4"))

(defcustom haddock-asymmetric-header nil
  "Determines if atx header style will be asymmetric.
Set to a non-nil value to use asymmetric header styling, placing
header markup only at the beginning of the line. By default,
balanced markup will be inserted at the beginning and end of the
line around the header title."
  :group 'haddock
  :type 'boolean)

(defcustom haddock-indent-function 'haddock-indent-line
  "Function to use to indent."
  :group 'haddock
  :type 'function)

(defcustom haddock-indent-on-enter t
  "Determines indentation behavior when pressing \\[newline].
Possible settings are nil, t, and 'indent-and-new-item.

When non-nil, pressing \\[newline] will call `newline-and-indent'
to indent the following line according to the context using
`haddock-indent-function'.  In this case, note that
\\[electric-newline-and-maybe-indent] can still be used to insert
a newline without indentation.

When set to 'indent-and-new-item and the point is in a list item
when \\[newline] is pressed, the list will be continued on the next
line, where a new item will be inserted.

When set to nil, simply call `newline' as usual.  In this case,
you can still indent lines using \\[haddock-cycle] and continue
lists with \\[haddock-insert-list-item].

Note that this assumes the variable `electric-indent-mode' is
non-nil (enabled).  When it is *disabled*, the behavior of
\\[newline] and `\\[electric-newline-and-maybe-indent]' are
reversed."
  :group 'haddock
  :type '(choice (const :tag "Don't automatically indent" nil)
                 (const :tag "Automatically indent" t)
                 (const :tag "Automatically indent and insert new list items" indent-and-new-item)))

(defcustom haddock-enable-wiki-links nil
  "Syntax highlighting for wiki links.
Set this to a non-nil value to turn on wiki link support by default.
Support can be toggled later using the `haddock-toggle-wiki-links'
function or \\[haddock-toggle-wiki-links]."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.2"))

(defcustom haddock-wiki-link-alias-first t
  "When non-nil, treat aliased wiki links like [[alias text|PageName]].
Otherwise, they will be treated as [[PageName|alias text]]."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp)

(defcustom haddock-wiki-link-search-subdirectories nil
  "When non-nil, search for wiki link targets in subdirectories.
This is the default search behavior for GitHub and is
automatically set to t in `gfm-mode'."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.2"))

(defcustom haddock-wiki-link-search-parent-directories nil
  "When non-nil, search for wiki link targets in parent directories.
This is the default search behavior of Ikiwiki."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.2"))

(defcustom haddock-wiki-link-fontify-missing nil
  "When non-nil, change wiki link face according to existence of target files.
This is expensive because it requires checking for the file each time the buffer
changes or the user switches windows.  It is disabled by default because it may
cause lag when typing on slower machines."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.2"))

(defcustom haddock-uri-types
  '("acap" "cid" "data" "dav" "fax" "file" "ftp"
    "gopher" "http" "https" "imap" "ldap" "mailto"
    "mid" "message" "modem" "news" "nfs" "nntp"
    "pop" "prospero" "rtsp" "service" "sip" "tel"
    "telnet" "tip" "urn" "vemmi" "wais")
  "Link types for syntax highlighting of URIs."
  :group 'haddock
  :type '(repeat (string :tag "URI scheme")))

(defcustom haddock-url-compose-char
  '(?∞ ?… ?⋯ ?# ?★ ?⚓)
  "Placeholder character for hidden URLs.
This may be a single character or a list of characters. In case
of a list, the first one that satisfies `char-displayable-p' will
be used."
  :type '(choice
          (character :tag "Single URL replacement character")
          (repeat :tag "List of possible URL replacement characters"
                  character))
  :package-version '(haddock-mode . "2.3"))

(defcustom haddock-blockquote-display-char
  '("▌" "┃" ">")
  "String to display when hiding blockquote markup.
This may be a single string or a list of string. In case of a
list, the first one that satisfies `char-displayable-p' will be
used."
  :type 'string
  :type '(choice
          (string :tag "Single blockquote display string")
          (repeat :tag "List of possible blockquote display strings" string))
  :package-version '(haddock-mode . "2.3"))

(defcustom haddock-hr-display-char
  '(?─ ?━ ?-)
  "Character for hiding horizontal rule markup.
This may be a single character or a list of characters.  In case
of a list, the first one that satisfies `char-displayable-p' will
be used."
  :group 'haddock
  :type '(choice
          (character :tag "Single HR display character")
          (repeat :tag "List of possible HR display characters" character))
  :package-version '(haddock-mode . "2.3"))

(defcustom haddock-definition-display-char
  '(?⁘ ?⁙ ?≡ ?⌑ ?◊ ?:)
  "Character for replacing definition list markup.
This may be a single character or a list of characters.  In case
of a list, the first one that satisfies `char-displayable-p' will
be used."
  :type '(choice
          (character :tag "Single definition list character")
          (repeat :tag "List of possible definition list characters" character))
  :package-version '(haddock-mode . "2.3"))

(defcustom haddock-enable-math nil
  "Syntax highlighting for inline LaTeX and itex expressions.
Set this to a non-nil value to turn on math support by default.
Math support can be enabled, disabled, or toggled later using
`haddock-toggle-math' or \\[haddock-toggle-math]."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp)
(make-variable-buffer-local 'haddock-enable-math)

(defcustom haddock-enable-html t
  "Enable font-lock support for HTML tags and attributes."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.4"))

(defcustom haddock-css-paths nil
  "URL of CSS file to link to in the output XHTML."
  :group 'haddock
  :type '(repeat (string :tag "CSS File Path")))

(defcustom haddock-content-type "text/html"
  "Content type string for the http-equiv header in XHTML output.
When set to an empty string, this attribute is omitted.  Defaults to
`text/html'."
  :group 'haddock
  :type 'string)

(defcustom haddock-coding-system nil
  "Character set string for the http-equiv header in XHTML output.
Defaults to `buffer-file-coding-system' (and falling back to
`utf-8' when not available).  Common settings are `iso-8859-1'
and `iso-latin-1'.  Use `list-coding-systems' for more choices."
  :group 'haddock
  :type 'coding-system)

(defcustom haddock-export-kill-buffer t
  "Kill output buffer after HTML export.
When non-nil, kill the HTML output buffer after
exporting with `haddock-export'."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.4"))

(defcustom haddock-xhtml-header-content ""
  "Additional content to include in the XHTML <head> block."
  :group 'haddock
  :type 'string)

(defcustom haddock-xhtml-body-preamble ""
  "Content to include in the XHTML <body> block, before the output."
  :group 'haddock
  :type 'string
  :safe 'stringp
  :package-version '(haddock-mode . "2.4"))

(defcustom haddock-xhtml-body-epilogue ""
  "Content to include in the XHTML <body> block, after the output."
  :group 'haddock
  :type 'string
  :safe 'stringp
  :package-version '(haddock-mode . "2.4"))

(defcustom haddock-xhtml-standalone-regexp
  "^\\(<\\?xml\\|<!DOCTYPE\\|<html\\)"
  "Regexp indicating whether `haddock-command' output is standalone XHTML."
  :group 'haddock
  :type 'regexp)

(defcustom haddock-link-space-sub-char "_"
  "Character to use instead of spaces when mapping wiki links to filenames."
  :group 'haddock
  :type 'string)

(defcustom haddock-reference-location 'header
  "Position where new reference definitions are inserted in the document."
  :group 'haddock
  :type '(choice (const :tag "At the end of the document" end)
                 (const :tag "Immediately after the current block" immediately)
                 (const :tag "At the end of the subtree" subtree)
                 (const :tag "Before next header" header)))

(defcustom haddock-footnote-location 'end
  "Position where new footnotes are inserted in the document."
  :group 'haddock
  :type '(choice (const :tag "At the end of the document" end)
                 (const :tag "Immediately after the current block" immediately)
                 (const :tag "At the end of the subtree" subtree)
                 (const :tag "Before next header" header)))

(defcustom haddock-footnote-display '((raise 0.2) (height 0.8))
  "Display specification for footnote markers and inline footnotes.
By default, footnote text is reduced in size and raised.  Set to
nil to disable this."
  :group 'haddock
  :type '(choice (sexp :tag "Display specification")
                 (const :tag "Don't set display property" nil))
  :package-version '(haddock-mode . "2.4"))

(defcustom haddock-sub-superscript-display
  '(((raise -0.3) (height 0.7)) . ((raise 0.3) (height 0.7)))
  "Display specification for subscript and superscripts.
The car is used for subscript, the cdr is used for superscripts."
  :group 'haddock
  :type '(cons (choice (sexp :tag "Subscript form")
                       (const :tag "No lowering" nil))
               (choice (sexp :tag "Superscript form")
                       (const :tag "No raising" nil)))
  :package-version '(haddock-mode . "2.4"))

(defcustom haddock-unordered-list-item-prefix "  * "
  "String inserted before unordered list items."
  :group 'haddock
  :type 'string)

(defcustom haddock-nested-imenu-heading-index t
  "Use nested or flat imenu heading index.
A nested index may provide more natural browsing from the menu,
but a flat list may allow for faster keyboard navigation via tab
completion."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.2"))

(defcustom haddock-add-footnotes-to-imenu t
  "Add footnotes to end of imenu heading index."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.4"))

(defcustom haddock-make-gfm-checkboxes-buttons t
  "When non-nil, make GFM checkboxes into buttons."
  :group 'haddock
  :type 'boolean)

(defcustom haddock-use-pandoc-style-yaml-metadata nil
  "When non-nil, allow YAML metadata anywhere in the document."
  :group 'haddock
  :type 'boolean)

(defcustom haddock-split-window-direction 'any
  "Preference for splitting windows for static and live preview.
The default value is 'any, which instructs Emacs to use
`split-window-sensibly' to automatically choose how to split
windows based on the values of `split-width-threshold' and
`split-height-threshold' and the available windows.  To force
vertically split (left and right) windows, set this to 'vertical
or 'right.  To force horizontally split (top and bottom) windows,
set this to 'horizontal or 'below."
  :group 'haddock
  :type '(choice (const :tag "Automatic" any)
                 (const :tag "Right (vertical)" right)
                 (const :tag "Below (horizontal)" below))
  :package-version '(haddock-mode . "2.2"))

(defcustom haddock-live-preview-window-function
  'haddock-live-preview-window-eww
  "Function to display preview of Haddock output within Emacs.
Function must update the buffer containing the preview and return
the buffer."
  :group 'haddock
  :type 'function)

(defcustom haddock-live-preview-delete-export 'delete-on-destroy
  "Delete exported HTML file when using `haddock-live-preview-export'.
If set to 'delete-on-export, delete on every export. When set to
'delete-on-destroy delete when quitting from command
`haddock-live-preview-mode'. Never delete if set to nil."
  :group 'haddock
  :type '(choice
          (const :tag "Delete on every export" delete-on-export)
          (const :tag "Delete when quitting live preview" delete-on-destroy)
          (const :tag "Never delete" nil)))

(defcustom haddock-list-indent-width 4
  "Depth of indentation for haddock lists.
Used in `haddock-demote-list-item' and
`haddock-promote-list-item'."
  :group 'haddock
  :type 'integer)

(defcustom haddock-enable-prefix-prompts t
  "Display prompts for certain prefix commands.
Set to nil to disable these prompts."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.3"))

(defcustom haddock-gfm-additional-languages nil
  "Extra languages made available when inserting GFM code blocks.
Language strings must have be trimmed of whitespace and not
contain any curly braces. They may be of arbitrary
capitalization, though."
  :group 'haddock
  :type '(repeat (string :validate haddock-validate-language-string)))

(defcustom haddock-gfm-use-electric-backquote t
  "Use `haddock-electric-backquote' when backquote is hit three times."
  :group 'haddock
  :type 'boolean)

(defcustom haddock-gfm-downcase-languages t
  "If non-nil, downcase suggested languages.
This applies to insertions done with
`haddock-electric-backquote'."
  :group 'haddock
  :type 'boolean)

(defcustom haddock-edit-code-block-default-mode 'normal-mode
  "Default mode to use for editing code blocks.
This mode is used when automatic detection fails, such as for GFM
code blocks with no language specified."
  :group 'haddock
  :type '(choice function (const :tag "None" nil))
  :package-version '(haddock-mode . "2.4"))

(defcustom haddock-gfm-uppercase-checkbox nil
  "If non-nil, use [X] for completed checkboxes, [x] otherwise."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp)

(defcustom haddock-hide-urls nil
  "Hide URLs of inline links and reference tags of reference links.
Such URLs will be replaced by a single customizable
character, defined by `haddock-url-compose-char', but are still part
of the buffer.  Links can be edited interactively with
\\[haddock-insert-link] or, for example, by deleting the final
parenthesis to remove the invisibility property. You can also
hover your mouse pointer over the link text to see the URL.
Set this to a non-nil value to turn this feature on by default.
You can interactively set the value of this variable by calling
`haddock-toggle-url-hiding', pressing \\[haddock-toggle-url-hiding],
or from the menu Haddock > Links & Images menu."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.3"))
(make-variable-buffer-local 'haddock-hide-urls)

(defcustom haddock-translate-filename-function #'identity
  "Function to use to translate filenames when following links.
\\<haddock-mode-map>\\[haddock-follow-thing-at-point] and \\[haddock-follow-link-at-point]
call this function with the filename as only argument whenever
they encounter a filename (instead of a URL) to be visited and
use its return value instead of the filename in the link.  For
example, if absolute filenames are actually relative to a server
root directory, you can set
`haddock-translate-filename-function' to a function that
prepends the root directory to the given filename."
  :group 'haddock
  :type 'function
  :risky t
  :package-version '(haddock-mode . "2.4"))

(defcustom haddock-max-image-size nil
  "Maximum width and height for displayed inline images.
This variable may be nil or a cons cell (MAX-WIDTH . MAX-HEIGHT).
When nil, use the actual size.  Otherwise, use ImageMagick to
resize larger images to be of the given maximum dimensions.  This
requires Emacs to be built with ImageMagick support."
  :group 'haddock
  :package-version '(haddock-mode . "2.4")
  :type '(choice
          (const :tag "Use actual image width" nil)
          (cons (choice (sexp :tag "Maximum width in pixels")
                        (const :tag "No maximum width" nil))
                (choice (sexp :tag "Maximum height in pixels")
                        (const :tag "No maximum height" nil)))))


;;; Haddock-Specific `rx' Macro ==============================================

;; Based on python-rx from python.el.
(eval-and-compile
  (defconst haddock-rx-constituents
    `((newline . ,(rx "\n"))
      (indent . ,(rx (or (repeat 4 " ") "\t")))
      (block-end . ,(rx (and (or (one-or-more (zero-or-more blank) "\n") line-end))))
      (numeral . ,(rx (and (one-or-more (any "0-9#")) ".")))
      (bullet . ,(rx (any "*+:-")))
      (list-marker . ,(rx (or (and (one-or-more (any "0-9#")) ".")
                              (any "*+:-"))))
      (checkbox . ,(rx "[" (any " xX") "]")))
    "Haddock-specific sexps for `haddock-rx'")

  (defun haddock-rx-to-string (form &optional no-group)
    "Haddock mode specialized `rx-to-string' function.
This variant supports named Haddock expressions in FORM.
NO-GROUP non-nil means don't put shy groups around the result."
    (let ((rx-constituents (append haddock-rx-constituents rx-constituents)))
      (rx-to-string form no-group)))

  (defmacro haddock-rx (&rest regexps)
    "Haddock mode specialized rx macro.
This variant of `rx' supports common Haddock named REGEXPS."
    (cond ((null regexps)
           (error "No regexp"))
          ((cdr regexps)
           (haddock-rx-to-string `(and ,@regexps) t))
          (t
           (haddock-rx-to-string (car regexps) t)))))


;;; Regular Expressions =======================================================

(defconst haddock-regex-comment-start
  "<!--"
  "Regular expression matches HTML comment opening.")

(defconst haddock-regex-comment-end
  "--[ \t]*>"
  "Regular expression matches HTML comment closing.")

(defconst haddock-regex-link-inline
  "\\(!\\)?\\(\\[\\)\\([^]^][^]]*\\|\\)\\(\\]\\)\\((\\)\\([^)]*?\\)\\(?:\\s-+\\(\"[^\"]*\"\\)\\)?\\()\\)"
  "Regular expression for a [text](file) or an image link ![text](file).
Group 1 matches the leading exclamation point (optional).
Group 2 matches the opening square bracket.
Group 3 matches the text inside the square brackets.
Group 4 matches the closing square bracket.
Group 5 matches the opening parenthesis.
Group 6 matches the URL.
Group 7 matches the title (optional).
Group 8 matches the closing parenthesis.")

(defconst haddock-regex-link-reference
  "\\(!\\)?\\(\\[\\)\\([^]^][^]]*\\|\\)\\(\\]\\)[ ]?\\(\\[\\)\\([^]]*?\\)\\(\\]\\)"
  "Regular expression for a reference link [text][id].
Group 1 matches the leading exclamation point (optional).
Group 2 matches the opening square bracket for the link text.
Group 3 matches the text inside the square brackets.
Group 4 matches the closing square bracket for the link text.
Group 5 matches the opening square bracket for the reference label.
Group 6 matches the reference label.
Group 7 matches the closing square bracket for the reference label.")

(defconst haddock-regex-reference-definition
  "^ \\{0,3\\}\\(\\[\\)\\([^]\n]+?\\)\\(\\]\\)\\(:\\)\\s *\\(.*?\\)\\s *\\( \"[^\"]*\"$\\|$\\)"
  "Regular expression for a reference definition.
Group 1 matches the opening square bracket.
Group 2 matches the reference label.
Group 3 matches the closing square bracket.
Group 4 matches the colon.
Group 5 matches the URL.
Group 6 matches the title attribute (optional).")

(defconst haddock-regex-footnote
  "\\(\\[\\^\\)\\(.+?\\)\\(\\]\\)"
  "Regular expression for a footnote marker [^fn].
Group 1 matches the opening square bracket and carat.
Group 2 matches only the label, without the surrounding markup.
Group 3 matches the closing square bracket.")

(defconst haddock-regex-header
  "^\\(?:\\([^\r\n\t -].*\\)\n\\(?:\\(=+\\)\\|\\(-+\\)\\)\\|\\(#+[ \t]+\\)\\(.*?\\)\\([ \t]*#*\\)\\)$"
  "Regexp identifying Haddock headings.
Group 1 matches the text of a setext heading.
Group 2 matches the underline of a level-1 setext heading.
Group 3 matches the underline of a level-2 setext heading.
Group 4 matches the opening hash marks of an atx heading and whitespace.
Group 5 matches the text, without surrounding whitespace, of an atx heading.
Group 6 matches the closing whitespace and hash marks of an atx heading.")

(defconst haddock-regex-header-setext
  "^\\([^\r\n\t -].*\\)\n\\(=+\\|-+\\)$"
  "Regular expression for generic setext-style (underline) headers.")

(defconst haddock-regex-header-atx
  "^\\(#+\\)[ \t]+\\(.*?\\)[ \t]*\\(#*\\)$"
  "Regular expression for generic atx-style (hash mark) headers.")

(defconst haddock-regex-hr
  (rx line-start
      (group (or (and (repeat 3 (and "*" (? " "))) (* (any "* ")))
                 (and (repeat 3 (and "-" (? " "))) (* (any "- ")))
                 (and (repeat 3 (and "_" (? " "))) (* (any "_ ")))))
      line-end)
  "Regular expression for matching Haddock horizontal rules.")

(defconst haddock-regex-code
  "\\(?:\\`\\|[^\\]\\)\\(\\(`+\\)\\(\\(?:.\\|\n[^\n]\\)*?[^`]\\)\\(\\2\\)\\)\\(?:[^`]\\|\\'\\)"
  "Regular expression for matching inline code fragments.

Group 1 matches the entire code fragment including the backquotes.
Group 2 matches the opening backquotes.
Group 3 matches the code fragment itself, without backquotes.
Group 4 matches the closing backquotes.

The leading, unnumbered group ensures that the leading backquote
character is not escaped.
The last group, also unnumbered, requires that the character
following the code fragment is not a backquote.
Note that \\(?:.\\|\n[^\n]\\) matches any character, including newlines,
but not two newlines in a row.")

(defconst haddock-regex-kbd
  "\\(<kbd>\\)\\(\\(?:.\\|\n[^\n]\\)*?\\)\\(</kbd>\\)"
  "Regular expression for matching <kbd> tags.
Groups 1 and 3 match the opening and closing tags.
Group 2 matches the key sequence.")

(defconst haddock-regex-gfm-code-block-open
  "^[[:blank:]]*\\(```\\)\\([[:blank:]]*{?[[:blank:]]*\\)\\([^[:space:]]+?\\)?\\(?:[[:blank:]]+\\(.+?\\)\\)?\\([[:blank:]]*}?[[:blank:]]*\\)$"
  "Regular expression matching opening of GFM code blocks.
Group 1 matches the opening three backquotes and any following whitespace.
Group 2 matches the opening brace (optional) and surrounding whitespace.
Group 3 matches the language identifier (optional).
Group 4 matches the info string (optional).
Group 5 matches the closing brace (optional), whitespace, and newline.
Groups need to agree with `haddock-regex-tilde-fence-begin'.")

(defconst haddock-regex-gfm-code-block-close
 "^[[:blank:]]*\\(```\\)\\(\\s *?\\)$"
 "Regular expression matching closing of GFM code blocks.
Group 1 matches the closing three backquotes.
Group 2 matches any whitespace and the final newline.")

(defconst haddock-regex-pre
  "^\\(    \\|\t\\).*$"
  "Regular expression for matching preformatted text sections.")

(defconst haddock-regex-list
  (haddock-rx line-start
               ;; 1. Leading whitespace
               (group (* blank))
               ;; 2. List marker: a numeral, bullet, or colon
               (group list-marker)
               ;; 3. Trailing whitespace
               (group (+ blank))
               ;; 4. Optional checkbox for GFM task list items
               (opt (group (and checkbox (* blank)))))
  "Regular expression for matching list items.")

(defconst haddock-regex-bold
  "\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\([^ \n\t\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\3\\)\\)"
  "Regular expression for matching bold text.
Group 1 matches the character before the opening asterisk or
underscore, if any, ensuring that it is not a backslash escape.
Group 2 matches the entire expression, including delimiters.
Groups 3 and 5 matches the opening and closing delimiters.
Group 4 matches the text inside the delimiters.")

(defconst haddock-regex-italic
  "\\(?:^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \n\t\\]\\|[^ \n\t*]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\2\\)\\)"
  "Regular expression for matching italic text.
The leading unnumbered matches the character before the opening
asterisk or underscore, if any, ensuring that it is not a
backslash escape.
Group 1 matches the entire expression, including delimiters.
Groups 2 and 4 matches the opening and closing delimiters.
Group 3 matches the text inside the delimiters.")

(defconst haddock-regex-strike-through
  "\\(^\\|[^\\]\\)\\(\\(~~\\)\\([^ \n\t\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(~~\\)\\)"
  "Regular expression for matching strike-through text.
Group 1 matches the character before the opening tilde, if any,
ensuring that it is not a backslash escape.
Group 2 matches the entire expression, including delimiters.
Groups 3 and 5 matches the opening and closing delimiters.
Group 4 matches the text inside the delimiters.")

(defconst haddock-regex-gfm-italic
  "\\(?:^\\|\\s-\\)\\(\\([*_]\\)\\([^ \\]\\2\\|[^ ]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\2\\)\\)"
  "Regular expression for matching italic text in GitHub Flavored Haddock.
Underscores in words are not treated as special.
Group 1 matches the entire expression, including delimiters.
Groups 2 and 4 matches the opening and closing delimiters.
Group 3 matches the text inside the delimiters.")

(defconst haddock-regex-blockquote
  "^[ \t]*\\([A-Z]?>\\)\\([ \t]*\\)\\(.*\\)$"
  "Regular expression for matching blockquote lines.
Also accounts for a potential capital letter preceding the angle
bracket, for use with Leanpub blocks (asides, warnings, info
blocks, etc.).
Group 1 matches the leading angle bracket.
Group 2 matches the separating whitespace.
Group 3 matches the text.")

(defconst haddock-regex-line-break
  "[^ \n\t][ \t]*\\(  \\)$"
  "Regular expression for matching line breaks.")

(defconst haddock-regex-wiki-link
  "\\(?:^\\|[^\\]\\)\\(\\(\\[\\[\\)\\([^]|]+\\)\\(?:\\(|\\)\\([^]]+\\)\\)?\\(\\]\\]\\)\\)"
  "Regular expression for matching wiki links.
This matches typical bracketed [[WikiLinks]] as well as 'aliased'
wiki links of the form [[PageName|link text]].
The meanings of the first and second components depend
on the value of `haddock-wiki-link-alias-first'.

Group 1 matches the entire link.
Group 2 matches the opening square brackets.
Group 3 matches the first component of the wiki link.
Group 4 matches the pipe separator, when present.
Group 5 matches the second component of the wiki link, when present.
Group 6 matches the closing square brackets.")

(defconst haddock-regex-uri
  (concat "\\(" (regexp-opt haddock-uri-types) ":[^]\t\n\r<>,;() ]+\\)")
  "Regular expression for matching inline URIs.")

(defconst haddock-regex-angle-uri
  (concat "\\(<\\)\\(" (regexp-opt haddock-uri-types) ":[^]\t\n\r<>,;()]+\\)\\(>\\)")
  "Regular expression for matching inline URIs in angle brackets.")

(defconst haddock-regex-email
  "<\\(\\(?:\\sw\\|\\s_\\|\\s.\\)+@\\(?:\\sw\\|\\s_\\|\\s.\\)+\\)>"
  "Regular expression for matching inline email addresses.")

(defsubst haddock-make-regex-link-generic ()
  "Make regular expression for matching any recognized link."
  (concat "\\(?:" haddock-regex-link-inline
          (when haddock-enable-wiki-links
            (concat "\\|" haddock-regex-wiki-link))
          "\\|" haddock-regex-link-reference
          "\\|" haddock-regex-angle-uri "\\)"))

(defconst haddock-regex-gfm-checkbox
  " \\(\\[[ xX]\\]\\) "
  "Regular expression for matching GFM checkboxes.
Group 1 matches the text to become a button.")

(defconst haddock-regex-blank-line
  "^[[:blank:]]*$"
  "Regular expression that matches a blank line.")

(defconst haddock-regex-block-separator
  "\n[\n\t\f ]*\n"
  "Regular expression for matching block boundaries.")

(defconst haddock-regex-block-separator-noindent
  (concat "\\(\\`\\|\\(" haddock-regex-block-separator "\\)[^\n\t\f ]\\)")
  "Regexp for block separators before lines with no indentation.")

(defconst haddock-regex-math-inline-single
  "\\(?:^\\|[^\\]\\)\\(\\$\\)\\(\\(?:[^\\$]\\|\\\\.\\)*\\)\\(\\$\\)"
  "Regular expression for itex $..$ math mode expressions.
Groups 1 and 3 match the opening and closing dollar signs.
Group 2 matches the mathematical expression contained within.")

(defconst haddock-regex-math-inline-double
  "\\(?:^\\|[^\\]\\)\\(\\$\\$\\)\\(\\(?:[^\\$]\\|\\\\.\\)*\\)\\(\\$\\$\\)"
  "Regular expression for itex $$..$$ math mode expressions.
Groups 1 and 3 match opening and closing dollar signs.
Group 2 matches the mathematical expression contained within.")

(defconst haddock-regex-math-display
  (rx line-start (* blank)
      (group (group (repeat 1 2 "\\")) "[")
      (group (*? anything))
      (group (backref 2) "]")
      line-end)
  "Regular expression for \[..\] or \\[..\\] display math.
Groups 1 and 4 match the opening and closing markup.
Group 3 matches the mathematical expression contained within.
Group 2 matches the opening slashes, and is used internally to
match the closing slashes.")

(defsubst haddock-make-tilde-fence-regex (num-tildes &optional end-of-line)
  "Return regexp matching a tilde code fence at least NUM-TILDES long.
END-OF-LINE is the regexp construct to indicate end of line; $ if
missing."
  (format "%s%d%s%s" "^[[:blank:]]*\\([~]\\{" num-tildes ",\\}\\)"
          (or end-of-line "$")))

(defconst haddock-regex-tilde-fence-begin
  (haddock-make-tilde-fence-regex
   3 "\\([[:blank:]]*{?\\)[[:blank:]]*\\([^[:space:]]+?\\)?\\(?:[[:blank:]]+\\(.+?\\)\\)?\\([[:blank:]]*}?[[:blank:]]*\\)$")
  "Regular expression for matching tilde-fenced code blocks.
Group 1 matches the opening tildes.
Group 2 matches (optional) opening brace and surrounding whitespace.
Group 3 matches the language identifier (optional).
Group 4 matches the info string (optional).
Group 5 matches the closing brace (optional) and any surrounding whitespace.
Groups need to agree with `haddock-regex-gfm-code-block-open'.")

(defconst haddock-regex-declarative-metadata
  "^\\([[:alpha:]][[:alpha:] _-]*?\\)\\([:=][ \t]*\\)\\(.*\\)$"
  "Regular expression for matching declarative metadata statements.
This matches MultiHaddock metadata as well as YAML and TOML
assignments such as the following:

    variable: value

or

    variable = value")

(defconst haddock-regex-pandoc-metadata
  "^\\(%\\)\\([ \t]*\\)\\(.*\\(?:\n[ \t]+.*\\)*\\)"
  "Regular expression for matching Pandoc metadata.")

(defconst haddock-regex-yaml-metadata-border
  "\\(-\\{3\\}\\)$"
  "Regular expression for matching YAML metadata.")

(defconst haddock-regex-yaml-pandoc-metadata-end-border
  "^\\(\\.\\{3\\}\\|\\-\\{3\\}\\)$"
  "Regular expression for matching YAML metadata end borders.")

(defsubst haddock-get-yaml-metadata-start-border ()
  "Return YAML metadata start border depending upon whether Pandoc is used."
  (concat
   (if haddock-use-pandoc-style-yaml-metadata "^" "\\`")
   haddock-regex-yaml-metadata-border))

(defsubst haddock-get-yaml-metadata-end-border (_)
  "Return YAML metadata end border depending upon whether Pandoc is used."
  (if haddock-use-pandoc-style-yaml-metadata
      haddock-regex-yaml-pandoc-metadata-end-border
    haddock-regex-yaml-metadata-border))

(defconst haddock-regex-inline-attributes
  "[ \t]*\\({:?\\)[ \t]*\\(\\(#[[:alpha:]_.:-]+\\|\\.[[:alpha:]_.:-]+\\|\\w+=['\"]?[^\n'\"]*['\"]?\\),?[ \t]*\\)+\\(}\\)[ \t]*$"
  "Regular expression for matching inline identifiers or attribute lists.
Compatible with Pandoc, Python Haddock, PHP Haddock Extra, and Leanpub.")

(defconst haddock-regex-leanpub-sections
  (concat
   "^\\({\\)\\("
   (regexp-opt '("frontmatter" "mainmatter" "backmatter" "appendix" "pagebreak"))
   "\\)\\(}\\)[ \t]*\n")
  "Regular expression for Leanpub section markers and related syntax.")

(defconst haddock-regex-sub-superscript
  "\\(?:^\\|[^\\~^]\\)\\(\\([~^]\\)\\([[:alnum:]]+\\)\\(\\2\\)\\)"
  "The regular expression matching a sub- or superscript.
The leading un-numbered group matches the character before the
opening tilde or carat, if any, ensuring that it is not a
backslash escape, carat, or tilde.
Group 1 matches the entire expression, including markup.
Group 2 matches the opening markup--a tilde or carat.
Group 3 matches the text inside the delimiters.
Group 4 matches the closing markup--a tilde or carat.")

(defconst haddock-regex-include
  "^\\(<<\\)\\(?:\\(\\[\\)\\(.*\\)\\(\\]\\)\\)?\\(?:\\((\\)\\(.*\\)\\()\\)\\)?\\(?:\\({\\)\\(.*\\)\\(}\\)\\)?$"
  "Regular expression matching common forms of include syntax.
Marked 2, Leanpub, and other processors support some of these forms:

<<[sections/section1.md]
<<(folder/filename)
<<[Code title](folder/filename)
<<{folder/raw_file.html}

Group 1 matches the opening two angle brackets.
Groups 2-4 match the opening square bracket, the text inside,
and the closing square bracket, respectively.
Groups 5-7 match the opening parenthesis, the text inside, and
the closing parenthesis.
Groups 8-10 match the opening brace, the text inside, and the brace.")

(defconst haddock-regex-pandoc-inline-footnote
  "\\(\\^\\)\\(\\[\\)\\(\\(?:.\\|\n[^\n]\\)*?\\)\\(\\]\\)"
  "Regular expression for Pandoc inline footnote^[footnote text].
Group 1 matches the opening caret.
Group 2 matches the opening square bracket.
Group 3 matches the footnote text, without the surrounding markup.
Group 4 matches the closing square bracket.")

(defconst haddock-regex-html-attr
  "\\(\\<[[:alpha:]:-]+\\>\\)\\(\\s-*\\(=\\)\\s-*\\(\".*?\"\\|'.*?'\\|[^'\">[:space:]]+\\)?\\)?"
  "Regular expression for matching HTML attributes and values.
Group 1 matches the attribute name.
Group 2 matches the following whitespace, equals sign, and value, if any.
Group 3 matches the equals sign, if any.
Group 4 matches single-, double-, or un-quoted attribute values.")

(defconst haddock-regex-html-tag
  (concat "\\(</?\\)\\(\\w+\\)\\(\\(\\s-+" haddock-regex-html-attr
          "\\)+\\s-*\\|\\s-*\\)\\(/?>\\)")
  "Regular expression for matching HTML tags.
Groups 1 and 9 match the beginning and ending angle brackets and slashes.
Group 2 matches the tag name.
Group 3 matches all attributes and whitespace following the tag name.")

(defconst haddock-regex-html-entity
  "\\(&#?[[:alnum:]]+;\\)"
  "Regular expression for matching HTML entities.")


;;; Syntax ====================================================================

(defvar haddock--syntax-properties
  (list 'haddock-tilde-fence-begin nil
        'haddock-tilde-fence-end nil
        'haddock-fenced-code nil
        'haddock-yaml-metadata-begin nil
        'haddock-yaml-metadata-end nil
        'haddock-yaml-metadata-section nil
        'haddock-gfm-block-begin nil
        'haddock-gfm-block-end nil
        'haddock-gfm-code nil
        'haddock-list-item nil
        'haddock-pre nil
        'haddock-blockquote nil
        'haddock-hr nil
        'haddock-comment nil
        'haddock-heading nil
        'haddock-heading-1-setext nil
        'haddock-heading-2-setext nil
        'haddock-heading-1-atx nil
        'haddock-heading-2-atx nil
        'haddock-heading-3-atx nil
        'haddock-heading-4-atx nil
        'haddock-heading-5-atx nil
        'haddock-heading-6-atx nil
        'haddock-metadata-key nil
        'haddock-metadata-value nil
        'haddock-metadata-markup nil)
  "Property list of all Haddock syntactic properties.")

(defsubst haddock-in-comment-p (&optional pos)
  "Return non-nil if POS is in a comment.
If POS is not given, use point instead."
  (get-text-property (or pos (point)) 'haddock-comment))

(defun haddock-syntax-propertize-extend-region (start end)
  "Extend START to END region to include an entire block of text.
This helps improve syntax analysis for block constructs.
Returns a cons (NEW-START . NEW-END) or nil if no adjustment should be made.
Function is called repeatedly until it returns nil. For details, see
`syntax-propertize-extend-region-functions'."
  (save-match-data
    (save-excursion
      (let* ((new-start (progn (goto-char start)
                               (skip-chars-forward "\n")
                               (if (re-search-backward "\n\n" nil t)
                                   (min start (match-end 0))
                                 (point-min))))
             (new-end (progn (goto-char end)
                             (skip-chars-backward "\n")
                             (if (re-search-forward "\n\n" nil t)
                                 (max end (match-beginning 0))
                               (point-max))))
             (code-match (haddock-code-block-at-pos new-start))
             (new-start (or (and code-match (cl-first code-match)) new-start))
             (code-match (and (< end (point-max)) (haddock-code-block-at-pos end)))
             (new-end (or (and code-match (cl-second code-match)) new-end)))
        (unless (and (eq new-start start) (eq new-end end))
          (cons new-start (min new-end (point-max))))))))

(defun haddock-font-lock-extend-region-function (start end _)
  "Used in `jit-lock-after-change-extend-region-functions'.
Delegates to `haddock-syntax-propertize-extend-region'. START
and END are the previous region to refontify."
  (let ((res (haddock-syntax-propertize-extend-region start end)))
    (when res
      ;; syntax-propertize-function is not called when character at
      ;; (point-max) is deleted, but font-lock-extend-region-functions
      ;; are called.  Force a syntax property update in that case.
      (when (= end (point-max))
        ;; This function is called in a buffer modification hook.
        ;; `haddock-syntax-propertize' doesn't save the match data,
        ;; so we have to do it here.
        (save-match-data
          (haddock-syntax-propertize (car res) (cdr res))))
      (setq jit-lock-start (car res)
            jit-lock-end (cdr res)))))

(defun haddock--cur-list-item-bounds ()
  "Return a list describing the list item at point.
Assumes that match data is set for `haddock-regex-list'.  See the
documentation for `haddock-cur-list-item-bounds' for the format of
the returned list."
  (save-excursion
    (let* ((begin (match-beginning 0))
           (indent (length (match-string-no-properties 1)))
           (nonlist-indent (- (match-end 3) (match-beginning 0)))
           (marker (buffer-substring-no-properties
                    (match-beginning 2) (match-end 3)))
           (checkbox (match-string-no-properties 4))
           (match (butlast (match-data t)))
           (end (haddock-cur-list-item-end nonlist-indent)))
      (list begin end indent nonlist-indent marker checkbox match))))

(defun haddock--append-list-item-bounds (marker indent cur-bounds bounds)
  "Update list item BOUNDS given list MARKER, block INDENT, and CUR-BOUNDS.
Here, MARKER is a string representing the type of list and INDENT
is an integer giving the indentation, in spaces, of the current
block.  CUR-BOUNDS is a list of the form returned by
`haddock-cur-list-item-bounds' and BOUNDS is a list of bounds
values for parent list items.  When BOUNDS is nil, it means we are
at baseline (not inside of a nested list)."
  (let ((prev-indent (or (cl-third (car bounds)) 0)))
    (cond
     ;; New list item at baseline.
     ((and marker (null bounds))
      (list cur-bounds))
     ;; List item with greater indentation (four or more spaces).
     ;; Increase list level by consing CUR-BOUNDS onto BOUNDS.
     ((and marker (>= indent (+ prev-indent 4)))
      (cons cur-bounds bounds))
     ;; List item with greater or equal indentation (less than four spaces).
     ;; Keep list level the same by replacing the car of BOUNDS.
     ((and marker (>= indent prev-indent))
      (cons cur-bounds (cdr bounds)))
     ;; Lesser indentation level.
     ;; Pop appropriate number of elements off BOUNDS list (e.g., lesser
     ;; indentation could move back more than one list level).  Note
     ;; that this block need not be the beginning of list item.
     ((< indent prev-indent)
      (while (and (> (length bounds) 1)
                  (setq prev-indent (cl-third (cadr bounds)))
                  (< indent (+ prev-indent 4)))
        (setq bounds (cdr bounds)))
      (cons cur-bounds bounds))
     ;; Otherwise, do nothing.
     (t bounds))))

(defun haddock-syntax-propertize-list-items (start end)
  "Propertize list items from START to END.
Stores nested list item information in the `haddock-list-item'
text property to make later syntax analysis easier.  The value of
this property is a list with elements of the form (begin . end)
giving the bounds of the current and parent list items."
  (save-excursion
    (goto-char start)
    (let (bounds level pre-regexp)
      ;; Find a baseline point with zero list indentation
      (haddock-search-backward-baseline)
      ;; Search for all list items between baseline and END
      (while (and (< (point) end)
                  (re-search-forward haddock-regex-list end 'limit))
        ;; Level of list nesting
        (setq level (length bounds))
        ;; Pre blocks need to be indented one level past the list level
        (setq pre-regexp (format "^\\(    \\|\t\\)\\{%d\\}" (1+ level)))
        (beginning-of-line)
        (cond
         ;; Reset at headings, horizontal rules, and top-level blank lines.
         ;; Propertize baseline when in range.
         ((haddock-new-baseline)
          (setq bounds nil))
         ;; Make sure this is not a line from a pre block
         ((looking-at-p pre-regexp))
         ;; If not, then update levels and propertize list item when in range.
         (t
          (let* ((indent (current-indentation))
                 (cur-bounds (haddock--cur-list-item-bounds))
                 (first (cl-first cur-bounds))
                 (last (cl-second cur-bounds))
                 (marker (cl-fifth cur-bounds)))
            (setq bounds (haddock--append-list-item-bounds
                          marker indent cur-bounds bounds))
          (when (and (<= start (point)) (<= (point) end))
            (put-text-property first last 'haddock-list-item bounds)))))
        (end-of-line)))))

(defun haddock-syntax-propertize-pre-blocks (start end)
  "Match preformatted text blocks from START to END."
  (save-excursion
    (goto-char start)
    (let ((levels (haddock-calculate-list-levels))
          indent pre-regexp close-regexp open close)
      (while (and (< (point) end) (not close))
        ;; Search for a region with sufficient indentation
        (if (null levels)
            (setq indent 1)
          (setq indent (1+ (length levels))))
        (setq pre-regexp (format "^\\(    \\|\t\\)\\{%d\\}" indent))
        (setq close-regexp (format "^\\(    \\|\t\\)\\{0,%d\\}\\([^ \t]\\)" (1- indent)))

        (cond
         ;; If not at the beginning of a line, move forward
         ((not (bolp)) (forward-line))
         ;; Move past blank lines
         ((haddock-cur-line-blank-p) (forward-line))
         ;; At headers and horizontal rules, reset levels
         ((haddock-new-baseline) (forward-line) (setq levels nil))
         ;; If the current line has sufficient indentation, mark out pre block
         ;; The opening should be preceded by a blank line.
         ((and (haddock-prev-line-blank) (looking-at pre-regexp))
          (setq open (match-beginning 0))
          (while (and (or (looking-at-p pre-regexp) (haddock-cur-line-blank-p))
                      (not (eobp)))
            (forward-line))
          (skip-syntax-backward "-")
          (setq close (point)))
         ;; If current line has a list marker, update levels, move to end of block
         ((looking-at haddock-regex-list)
          (setq levels (haddock-update-list-levels
                        (match-string 2) (current-indentation) levels))
          (haddock-end-of-text-block))
         ;; If this is the end of the indentation level, adjust levels accordingly.
         ;; Only match end of indentation level if levels is not the empty list.
         ((and (car levels) (looking-at-p close-regexp))
          (setq levels (haddock-update-list-levels
                        nil (current-indentation) levels))
          (haddock-end-of-text-block))
         (t (haddock-end-of-text-block))))

      (when (and open close)
        ;; Set text property data
        (put-text-property open close 'haddock-pre (list open close))
        ;; Recursively search again
        (haddock-syntax-propertize-pre-blocks (point) end)))))

(defconst haddock-fenced-block-pairs
  `(((,haddock-regex-tilde-fence-begin haddock-tilde-fence-begin)
     (haddock-make-tilde-fence-regex haddock-tilde-fence-end)
     haddock-fenced-code)
    ((haddock-get-yaml-metadata-start-border haddock-yaml-metadata-begin)
     (haddock-get-yaml-metadata-end-border haddock-yaml-metadata-end)
     haddock-yaml-metadata-section)
    ((,haddock-regex-gfm-code-block-open haddock-gfm-block-begin)
     (,haddock-regex-gfm-code-block-close haddock-gfm-block-end)
     haddock-gfm-code))
  "Mapping of regular expressions to \"fenced-block\" constructs.
These constructs are distinguished by having a distinctive start
and end pattern, both of which take up an entire line of text,
but no special pattern to identify text within the fenced
blocks (unlike blockquotes and indented-code sections).

Each element within this list takes the form:

  ((START-REGEX-OR-FUN START-PROPERTY)
   (END-REGEX-OR-FUN END-PROPERTY)
   MIDDLE-PROPERTY)

Each *-REGEX-OR-FUN element can be a regular expression as a string, or a
function which evaluates to same. Functions for START-REGEX-OR-FUN accept no
arguments, but functions for END-REGEX-OR-FUN accept a single numerical argument
which is the length of the first group of the START-REGEX-OR-FUN match, which
can be ignored if unnecessary. `haddock-maybe-funcall-regexp' is used to
evaluate these into \"real\" regexps.

The *-PROPERTY elements are the text properties applied to each part of the
block construct when it is matched using
`haddock-syntax-propertize-fenced-block-constructs'. START-PROPERTY is applied
to the text matching START-REGEX-OR-FUN, END-PROPERTY to END-REGEX-OR-FUN, and
MIDDLE-PROPERTY to the text in between the two. The value of *-PROPERTY is the
`match-data' when the regexp was matched to the text. In the case of
MIDDLE-PROPERTY, the value is a false match data of the form '(begin end), with
begin and end set to the edges of the \"middle\" text. This makes fontification
easier.")

(defun haddock-text-property-at-point (prop)
  (get-text-property (point) prop))

(defsubst haddock-maybe-funcall-regexp (object &optional arg)
  (cond ((functionp object)
         (if arg (funcall object arg) (funcall object)))
        ((stringp object) object)
        (t (error "Object cannot be turned into regex"))))

(defsubst haddock-get-start-fence-regexp ()
  "Return regexp to find all \"start\" sections of fenced block constructs.
Which construct is actually contained in the match must be found separately."
  (mapconcat
   #'identity
   (mapcar (lambda (entry) (haddock-maybe-funcall-regexp (caar entry)))
           haddock-fenced-block-pairs)
   "\\|"))

(defun haddock-get-fenced-block-begin-properties ()
  (cl-mapcar (lambda (entry) (cl-cadar entry)) haddock-fenced-block-pairs))

(defun haddock-get-fenced-block-end-properties ()
  (cl-mapcar (lambda (entry) (cl-cadadr entry)) haddock-fenced-block-pairs))

(defun haddock-get-fenced-block-middle-properties ()
  (cl-mapcar #'cl-third haddock-fenced-block-pairs))

(defun haddock-find-previous-prop (prop &optional lim)
  "Find previous place where property PROP is non-nil, up to LIM.
Return a cons of (pos . property). pos is point if point contains
non-nil PROP."
  (let ((res
         (if (get-text-property (point) prop) (point)
           (previous-single-property-change
            (point) prop nil (or lim (point-min))))))
    (when (and (not (get-text-property res prop))
               (> res (point-min))
               (get-text-property (1- res) prop))
      (cl-decf res))
    (when (and res (get-text-property res prop)) (cons res prop))))

(defun haddock-find-next-prop (prop &optional lim)
  "Find next place where property PROP is non-nil, up to LIM.
Return a cons of (POS . PROPERTY) where POS is point if point
contains non-nil PROP."
  (let ((res
         (if (get-text-property (point) prop) (point)
           (next-single-property-change
            (point) prop nil (or lim (point-max))))))
    (when (and res (get-text-property res prop)) (cons res prop))))

(defun haddock-min-of-seq (map-fn seq)
  "Apply MAP-FN to SEQ and return element of SEQ with minimum value of MAP-FN."
  (cl-loop for el in seq
           with min = 1.0e+INF          ; infinity
           with min-el = nil
           do (let ((res (funcall map-fn el)))
                (when (< res min)
                  (setq min res)
                  (setq min-el el)))
           finally return min-el))

(defun haddock-max-of-seq (map-fn seq)
  "Apply MAP-FN to SEQ and return element of SEQ with maximum value of MAP-FN."
  (cl-loop for el in seq
           with max = -1.0e+INF          ; negative infinity
           with max-el = nil
           do (let ((res (funcall map-fn el)))
                (when (and res (> res max))
                  (setq max res)
                  (setq max-el el)))
           finally return max-el))

(defun haddock-find-previous-block ()
  "Find previous block.
Detect whether `haddock-syntax-propertize-fenced-block-constructs' was
unable to propertize the entire block, but was able to propertize the beginning
of the block. If so, return a cons of (pos . property) where the beginning of
the block was propertized."
  (let ((start-pt (point))
        (closest-open
         (haddock-max-of-seq
          #'car
          (cl-remove-if
           #'null
           (cl-mapcar
            #'haddock-find-previous-prop
            (haddock-get-fenced-block-begin-properties))))))
    (when closest-open
      (let* ((length-of-open-match
              (let ((match-d
                     (get-text-property (car closest-open) (cdr closest-open))))
                (- (cl-fourth match-d) (cl-third match-d))))
             (end-regexp
              (haddock-maybe-funcall-regexp
               (cl-caadr
                (cl-find-if
                 (lambda (entry) (eq (cl-cadar entry) (cdr closest-open)))
                 haddock-fenced-block-pairs))
               length-of-open-match))
             (end-prop-loc
              (save-excursion
                (save-match-data
                  (goto-char (car closest-open))
                  (and (re-search-forward end-regexp start-pt t)
                       (match-beginning 0))))))
        (and (not end-prop-loc) closest-open)))))

(defun haddock-get-fenced-block-from-start (prop)
  "Return limits of an enclosing fenced block from its start, using PROP.
Return value is a list usable as `match-data'."
  (catch 'no-rest-of-block
    (let* ((correct-entry
            (cl-find-if
             (lambda (entry) (eq (cl-cadar entry) prop))
             haddock-fenced-block-pairs))
           (begin-of-begin (cl-first (haddock-text-property-at-point prop)))
           (middle-prop (cl-third correct-entry))
           (end-prop (cl-cadadr correct-entry))
           (end-of-end
            (save-excursion
              (goto-char (match-end 0))   ; end of begin
              (unless (eobp) (forward-char))
              (let ((mid-prop-v (haddock-text-property-at-point middle-prop)))
                (if (not mid-prop-v)    ; no middle
                    (progn
                      ;; try to find end by advancing one
                      (let ((end-prop-v
                             (haddock-text-property-at-point end-prop)))
                        (if end-prop-v (cl-second end-prop-v)
                          (throw 'no-rest-of-block nil))))
                  (set-match-data mid-prop-v)
                  (goto-char (match-end 0))   ; end of middle
                  (beginning-of-line)         ; into end
                  (cl-second (haddock-text-property-at-point end-prop)))))))
      (list begin-of-begin end-of-end))))

(defun haddock-get-fenced-block-from-middle (prop)
  "Return limits of an enclosing fenced block from its middle, using PROP.
Return value is a list usable as `match-data'."
  (let* ((correct-entry
          (cl-find-if
           (lambda (entry) (eq (cl-third entry) prop))
           haddock-fenced-block-pairs))
         (begin-prop (cl-cadar correct-entry))
         (begin-of-begin
          (save-excursion
            (goto-char (match-beginning 0))
            (unless (bobp) (forward-line -1))
            (beginning-of-line)
            (cl-first (haddock-text-property-at-point begin-prop))))
         (end-prop (cl-cadadr correct-entry))
         (end-of-end
          (save-excursion
            (goto-char (match-end 0))
            (beginning-of-line)
            (cl-second (haddock-text-property-at-point end-prop)))))
    (list begin-of-begin end-of-end)))

(defun haddock-get-fenced-block-from-end (prop)
  "Return limits of an enclosing fenced block from its end, using PROP.
Return value is a list usable as `match-data'."
  (let* ((correct-entry
          (cl-find-if
           (lambda (entry) (eq (cl-cadadr entry) prop))
           haddock-fenced-block-pairs))
         (end-of-end (cl-second (haddock-text-property-at-point prop)))
         (middle-prop (cl-third correct-entry))
         (begin-prop (cl-cadar correct-entry))
         (begin-of-begin
          (save-excursion
            (goto-char (match-beginning 0)) ; beginning of end
            (unless (bobp) (backward-char)) ; into middle
            (let ((mid-prop-v (haddock-text-property-at-point middle-prop)))
              (if (not mid-prop-v)
                  (progn
                    (beginning-of-line)
                    (cl-first (haddock-text-property-at-point begin-prop)))
                (set-match-data mid-prop-v)
                (goto-char (match-beginning 0))   ; beginning of middle
                (unless (bobp) (forward-line -1)) ; into beginning
                (beginning-of-line)
                (cl-first (haddock-text-property-at-point begin-prop)))))))
    (list begin-of-begin end-of-end)))

(defun haddock-get-enclosing-fenced-block-construct (&optional pos)
  "Get \"fake\" match data for block enclosing POS.
Returns fake match data which encloses the start, middle, and end
of the block construct enclosing POS, if it exists. Used in
`haddock-code-block-at-pos'."
  (save-excursion
    (when pos (goto-char pos))
    (beginning-of-line)
    (car
     (cl-remove-if
      #'null
      (cl-mapcar
       (lambda (fun-and-prop)
         (cl-destructuring-bind (fun prop) fun-and-prop
           (when prop
             (save-match-data
               (set-match-data (haddock-text-property-at-point prop))
               (funcall fun prop)))))
       `((haddock-get-fenced-block-from-start
          ,(cl-find-if
            #'haddock-text-property-at-point
            (haddock-get-fenced-block-begin-properties)))
         (haddock-get-fenced-block-from-middle
          ,(cl-find-if
            #'haddock-text-property-at-point
            (haddock-get-fenced-block-middle-properties)))
         (haddock-get-fenced-block-from-end
          ,(cl-find-if
            #'haddock-text-property-at-point
            (haddock-get-fenced-block-end-properties)))))))))

(defun haddock-propertize-end-match (reg end fence-spec middle-begin)
  "Get match for REG up to END, if exists, and propertize appropriately.
FENCE-SPEC is an entry in `haddock-fenced-block-pairs' and
MIDDLE-BEGIN is the start of the \"middle\" section of the block."
  (when (re-search-forward reg end t)
    (let ((close-begin (match-beginning 0)) ; Start of closing line.
          (close-end (match-end 0))         ; End of closing line.
          (close-data (match-data t)))      ; Match data for closing line.
      ;; Propertize middle section of fenced block.
      (put-text-property middle-begin close-begin
                         (cl-third fence-spec)
                         (list middle-begin close-begin))
      ;; If the block is a YAML block, propertize the declarations inside
      (haddock-syntax-propertize-yaml-metadata middle-begin close-begin)
      ;; Propertize closing line of fenced block.
      (put-text-property close-begin close-end
                         (cl-cadadr fence-spec) close-data))))

(defun haddock-syntax-propertize-fenced-block-constructs (start end)
  "Propertize according to `haddock-fenced-block-pairs' from START to END.
If unable to propertize an entire block (if the start of a block is within START
and END, but the end of the block is not), propertize the start section of a
block, then in a subsequent call propertize both middle and end by finding the
start which was previously propertized."
  (let ((start-reg (haddock-get-start-fence-regexp)))
    (save-excursion
      (goto-char start)
      ;; start from previous unclosed block, if exists
      (let ((prev-begin-block (haddock-find-previous-block)))
        (when prev-begin-block
          (let* ((correct-entry
                  (cl-find-if (lambda (entry)
                                (eq (cdr prev-begin-block) (cl-cadar entry)))
                              haddock-fenced-block-pairs))
                 (enclosed-text-start (1+ (car prev-begin-block)))
                 (start-length
                  (save-excursion
                    (goto-char (car prev-begin-block))
                    (string-match
                     (haddock-maybe-funcall-regexp
                      (caar correct-entry))
                     (buffer-substring
                      (point-at-bol) (point-at-eol)))
                    (- (match-end 1) (match-beginning 1))))
                 (end-reg (haddock-maybe-funcall-regexp
                           (cl-caadr correct-entry) start-length)))
            (haddock-propertize-end-match
             end-reg end correct-entry enclosed-text-start))))
      ;; find all new blocks within region
      (while (re-search-forward start-reg end t)
        ;; we assume the opening constructs take up (only) an entire line,
        ;; so we re-check the current line
        (let* ((cur-line (buffer-substring (point-at-bol) (point-at-eol)))
               ;; find entry in `haddock-fenced-block-pairs' corresponding
               ;; to regex which was matched
               (correct-entry
                (cl-find-if
                 (lambda (fenced-pair)
                   (string-match-p
                    (haddock-maybe-funcall-regexp (caar fenced-pair))
                    cur-line))
                 haddock-fenced-block-pairs))
               (enclosed-text-start
                (save-excursion (1+ (point-at-eol))))
               (end-reg
                (haddock-maybe-funcall-regexp
                 (cl-caadr correct-entry)
                 (if (and (match-beginning 1) (match-end 1))
                     (- (match-end 1) (match-beginning 1))
                   0))))
          ;; get correct match data
          (save-excursion
            (beginning-of-line)
            (re-search-forward
             (haddock-maybe-funcall-regexp (caar correct-entry))
             (point-at-eol)))
          ;; mark starting, even if ending is outside of region
          (put-text-property (match-beginning 0) (match-end 0)
                             (cl-cadar correct-entry) (match-data t))
          (haddock-propertize-end-match
           end-reg end correct-entry enclosed-text-start))))))

(defun haddock-syntax-propertize-blockquotes (start end)
  "Match blockquotes from START to END."
  (save-excursion
    (goto-char start)
    (while (and (re-search-forward haddock-regex-blockquote end t)
                (not (haddock-code-block-at-pos (match-beginning 0))))
      (put-text-property (match-beginning 0) (match-end 0)
                         'haddock-blockquote
                         (match-data t)))))

(defun haddock-syntax-propertize-hrs (start end)
  "Match horizontal rules from START to END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward haddock-regex-hr end t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (goto-char beg)
        (unless (or (haddock-on-heading-p)
                    (haddock-code-block-at-point-p))
          (put-text-property beg end 'haddock-hr (match-data t)))
        (goto-char end)))))

(defun haddock-syntax-propertize-yaml-metadata (start end)
  "Propertize elements inside YAML metadata blocks from START to END.
Assumes region from START and END is already known to be the interior
region of a YAML metadata block as propertized by
`haddock-syntax-propertize-fenced-block-constructs'."
  (save-excursion
    (goto-char start)
    (cl-loop
     while (re-search-forward haddock-regex-declarative-metadata end t)
     do (progn
          (put-text-property (match-beginning 1) (match-end 1)
                             'haddock-metadata-key (match-data t))
          (put-text-property (match-beginning 2) (match-end 2)
                             'haddock-metadata-markup (match-data t))
          (put-text-property (match-beginning 3) (match-end 3)
                             'haddock-metadata-value (match-data t))))))

(defun haddock-syntax-propertize-headings (start end)
  "Match headings of type SYMBOL with REGEX from START to END."
  (goto-char start)
  (while (re-search-forward haddock-regex-header end t)
    (unless (haddock-code-block-at-pos (match-beginning 0))
      (put-text-property
       (match-beginning 0) (match-end 0) 'haddock-heading
       (match-data t))
      (put-text-property
       (match-beginning 0) (match-end 0)
       (cond ((match-string-no-properties 2) 'haddock-heading-1-setext)
             ((match-string-no-properties 3) 'haddock-heading-2-setext)
             (t (let ((atx-level (length (haddock-trim-whitespace
                                          (match-string-no-properties 4)))))
                  (intern (format "haddock-heading-%d-atx" atx-level)))))
       (match-data t)))))

(defun haddock-syntax-propertize-comments (start end)
  "Match HTML comments from the START to END."
  (let* ((in-comment (nth 4 (syntax-ppss)))
         (comment-begin (nth 8 (syntax-ppss))))
    (goto-char start)
    (cond
     ;; Comment start
     ((and (not in-comment)
           (re-search-forward haddock-regex-comment-start end t)
           (not (haddock-inline-code-at-point-p))
           (not (haddock-code-block-at-point-p)))
      (let ((open-beg (match-beginning 0)))
        (put-text-property open-beg (1+ open-beg)
                           'syntax-table (string-to-syntax "<"))
        (haddock-syntax-propertize-comments
         (min (1+ (match-end 0)) end (point-max)) end)))
     ;; Comment end
     ((and in-comment comment-begin
           (re-search-forward haddock-regex-comment-end end t))
      (let ((comment-end (match-end 0)))
        (put-text-property (1- comment-end) comment-end
                           'syntax-table (string-to-syntax ">"))
        ;; Remove any other text properties inside the comment
        (remove-text-properties comment-begin comment-end
                                haddock--syntax-properties)
        (put-text-property comment-begin comment-end
                           'haddock-comment (list comment-begin comment-end))
        (haddock-syntax-propertize-comments
         (min (1+ comment-end) end (point-max)) end)))
     ;; Nothing found
     (t nil))))

(defun haddock-syntax-propertize (start end)
  "Function used as `syntax-propertize-function'.
START and END delimit region to propertize."
  (with-silent-modifications
    (save-excursion
      (remove-text-properties start end haddock--syntax-properties)
      (haddock-syntax-propertize-fenced-block-constructs start end)
      (haddock-syntax-propertize-list-items start end)
      (haddock-syntax-propertize-pre-blocks start end)
      (haddock-syntax-propertize-blockquotes start end)
      (haddock-syntax-propertize-headings start end)
      (haddock-syntax-propertize-hrs start end)
      (haddock-syntax-propertize-comments start end))))


;;; Markup Hiding =============================================================

(defconst haddock-markup-properties
  '(face haddock-markup-face invisible haddock-markup)
  "List of properties and values to apply to markup.")

(defconst haddock-language-keyword-properties
  '(face haddock-language-keyword-face invisible haddock-markup)
  "List of properties and values to apply to code block language names.")

(defconst haddock-language-info-properties
  '(face haddock-language-info-face invisible haddock-markup)
  "List of properties and values to apply to code block language info strings.")

(defconst haddock-include-title-properties
  '(face haddock-link-title-face invisible haddock-markup)
  "List of properties and values to apply to included code titles.")

(defcustom haddock-hide-markup nil
  "Determines whether markup in the buffer will be hidden.
When set to nil, all markup is displayed in the buffer as it
appears in the file.  An exception is when `haddock-hide-urls'
is non-nil.
Set this to a non-nil value to turn this feature on by default.
You can interactively toggle the value of this variable with
`haddock-toggle-markup-hiding', \\[haddock-toggle-markup-hiding],
or from the Haddock > Show & Hide menu.

Markup hiding works by adding text properties to positions in the
buffer---either the `invisible' property or the `display' property
in cases where alternative glyphs are used (e.g., list bullets).
This does not, however, affect printing or other output.
Functions such as `htmlfontify-buffer' and `ps-print-buffer' will
not honor these text properties.  For printing, it would be better
to first convert to HTML or PDF (e.g,. using Pandoc)."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.3"))
(make-variable-buffer-local 'haddock-hide-markup)

(defun haddock-toggle-markup-hiding (&optional arg)
  "Toggle the display or hiding of markup.
With a prefix argument ARG, enable markup hiding if ARG is positive,
and disable it otherwise.
See `haddock-hide-markup' for additional details."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq haddock-hide-markup
        (if (eq arg 'toggle)
            (not haddock-hide-markup)
          (> (prefix-numeric-value arg) 0)))
  (if haddock-hide-markup
      (progn (add-to-invisibility-spec 'haddock-markup)
             (message "haddock-mode markup hiding enabled"))
    (progn (remove-from-invisibility-spec 'haddock-markup)
           (message "haddock-mode markup hiding disabled")))
  (haddock-reload-extensions))


;;; Font Lock =================================================================

(require 'font-lock)

(defvar haddock-italic-face 'haddock-italic-face
  "Face name to use for italic text.")

(defvar haddock-bold-face 'haddock-bold-face
  "Face name to use for bold text.")

(defvar haddock-strike-through-face 'haddock-strike-through-face
  "Face name to use for strike-through text.")

(defvar haddock-header-delimiter-face 'haddock-header-delimiter-face
  "Face name to use as a base for header delimiters.")

(defvar haddock-header-rule-face 'haddock-header-rule-face
  "Face name to use as a base for header rules.")

(defvar haddock-header-face 'haddock-header-face
  "Face name to use as a base for headers.")

(defvar haddock-header-face-1 'haddock-header-face-1
  "Face name to use for level-1 headers.")

(defvar haddock-header-face-2 'haddock-header-face-2
  "Face name to use for level-2 headers.")

(defvar haddock-header-face-3 'haddock-header-face-3
  "Face name to use for level-3 headers.")

(defvar haddock-header-face-4 'haddock-header-face-4
  "Face name to use for level-4 headers.")

(defvar haddock-header-face-5 'haddock-header-face-5
  "Face name to use for level-5 headers.")

(defvar haddock-header-face-6 'haddock-header-face-6
  "Face name to use for level-6 headers.")

(defvar haddock-inline-code-face 'haddock-inline-code-face
  "Face name to use for inline code.")

(defvar haddock-list-face 'haddock-list-face
  "Face name to use for list markers.")

(defvar haddock-blockquote-face 'haddock-blockquote-face
  "Face name to use for blockquote.")

(defvar haddock-pre-face 'haddock-pre-face
  "Face name to use for preformatted text.")

(defvar haddock-language-keyword-face 'haddock-language-keyword-face
  "Face name to use for programming language identifiers.")

(defvar haddock-language-info-face 'haddock-language-info-face
  "Face name to use for programming info strings.")

(defvar haddock-link-face 'haddock-link-face
  "Face name to use for links.")

(defvar haddock-missing-link-face 'haddock-missing-link-face
  "Face name to use for links where the linked file does not exist.")

(defvar haddock-reference-face 'haddock-reference-face
  "Face name to use for reference.")

(defvar haddock-footnote-marker-face 'haddock-footnote-marker-face
  "Face name to use for footnote markers.")

(defvar haddock-url-face 'haddock-url-face
  "Face name to use for URLs.")

(defvar haddock-link-title-face 'haddock-link-title-face
  "Face name to use for reference link titles.")

(defvar haddock-line-break-face 'haddock-line-break-face
  "Face name to use for hard line breaks.")

(defvar haddock-comment-face 'haddock-comment-face
  "Face name to use for HTML comments.")

(defvar haddock-math-face 'haddock-math-face
  "Face name to use for LaTeX expressions.")

(defvar haddock-metadata-key-face 'haddock-metadata-key-face
  "Face name to use for metadata keys.")

(defvar haddock-metadata-value-face 'haddock-metadata-value-face
  "Face name to use for metadata values.")

(defvar haddock-gfm-checkbox-face 'haddock-gfm-checkbox-face
  "Face name to use for GFM checkboxes.")

(defvar haddock-highlight-face 'haddock-highlight-face
  "Face name to use for mouse highlighting.")

(defvar haddock-markup-face 'haddock-markup-face
  "Face name to use for markup elements.")

(make-obsolete-variable 'haddock-italic-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-bold-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-strike-through-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-header-delimiter-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-header-rule-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-header-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-header-face-1 "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-header-face-2 "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-header-face-3 "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-header-face-4 "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-header-face-5 "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-header-face-6 "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-inline-code-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-list-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-blockquote-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-pre-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-language-keyword-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-language-info-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-link-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-missing-link-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-reference-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-footnote-marker-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-url-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-link-title-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-line-break-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-comment-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-math-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-metadata-key-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-metadata-value-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-gfm-checkbox-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-highlight-face "Use face name directly" "v2.4")
(make-obsolete-variable 'haddock-markup-face "Use face name directly" "v2.4")

(defgroup haddock-faces nil
  "Faces used in Haddock Mode"
  :group 'haddock
  :group 'faces)

(defface haddock-italic-face
  '((t (:inherit italic)))
  "Face for italic text."
  :group 'haddock-faces)

(defface haddock-bold-face
  '((t (:inherit bold)))
  "Face for bold text."
  :group 'haddock-faces)

(defface haddock-strike-through-face
  '((t (:strike-through t)))
  "Face for strike-through text."
  :group 'haddock-faces)

(defface haddock-markup-face
  '((t (:inherit shadow :slant normal :weight normal)))
  "Face for markup elements."
  :group 'haddock-faces)

(defface haddock-header-rule-face
  '((t (:inherit haddock-markup-face)))
  "Base face for headers rules."
  :group 'haddock-faces)

(defface haddock-header-delimiter-face
  '((t (:inherit haddock-markup-face)))
  "Base face for headers hash delimiter."
  :group 'haddock-faces)

(defface haddock-list-face
  '((t (:inherit haddock-markup-face)))
  "Face for list item markers."
  :group 'haddock-faces)

(defface haddock-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Face for blockquote sections."
  :group 'haddock-faces)

(defface haddock-code-face
  '((t (:inherit fixed-pitch)))
  "Face for inline code, pre blocks, and fenced code blocks.
This may be used, for example, to add a contrasting background to
inline code fragments and code blocks."
  :group 'haddock-faces)

(defface haddock-inline-code-face
  '((t (:inherit (haddock-code-face font-lock-constant-face))))
  "Face for inline code."
  :group 'haddock-faces)

(defface haddock-pre-face
  '((t (:inherit (haddock-code-face font-lock-constant-face))))
  "Face for preformatted text."
  :group 'haddock-faces)

(defface haddock-table-face
  '((t (:inherit (haddock-code-face))))
  "Face for tables."
  :group 'haddock-faces)

(defface haddock-language-keyword-face
  '((t (:inherit font-lock-type-face)))
  "Face for programming language identifiers."
  :group 'haddock-faces)

(defface haddock-language-info-face
  '((t (:inherit font-lock-string-face)))
  "Face for programming language info strings."
  :group 'haddock-faces)

(defface haddock-link-face
  '((t (:inherit link)))
  "Face for links."
  :group 'haddock-faces)

(defface haddock-missing-link-face
  '((t (:inherit font-lock-warning-face)))
  "Face for missing links."
  :group 'haddock-faces)

(defface haddock-reference-face
  '((t (:inherit haddock-markup-face)))
  "Face for link references."
  :group 'haddock-faces)

(define-obsolete-face-alias 'haddock-footnote-face
  'haddock-footnote-marker-face "v2.3")

(defface haddock-footnote-marker-face
  '((t (:inherit haddock-markup-face)))
  "Face for footnote markers."
  :group 'haddock-faces)

(defface haddock-footnote-text-face
  '((t (:inherit font-lock-comment-face)))
  "Face for footnote text."
  :group 'haddock-faces)

(defface haddock-url-face
  '((t (:inherit font-lock-string-face)))
  "Face for URLs that are part of markup.
For example, this applies to URLs in inline links:
[link text](http://example.com/)."
  :group 'haddock-faces)

(defface haddock-plain-url-face
  '((t (:inherit haddock-link-face)))
  "Face for URLs that are also links.
For example, this applies to plain angle bracket URLs:
<http://example.com/>."
  :group 'haddock-faces)

(defface haddock-link-title-face
  '((t (:inherit font-lock-comment-face)))
  "Face for reference link titles."
  :group 'haddock-faces)

(defface haddock-line-break-face
  '((t (:inherit font-lock-constant-face :underline t)))
  "Face for hard line breaks."
  :group 'haddock-faces)

(defface haddock-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for HTML comments."
  :group 'haddock-faces)

(defface haddock-math-face
  '((t (:inherit font-lock-string-face)))
  "Face for LaTeX expressions."
  :group 'haddock-faces)

(defface haddock-metadata-key-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for metadata keys."
  :group 'haddock-faces)

(defface haddock-metadata-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for metadata values."
  :group 'haddock-faces)

(defface haddock-gfm-checkbox-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for GFM checkboxes."
  :group 'haddock-faces)

(defface haddock-highlight-face
  '((t (:inherit highlight)))
  "Face for mouse highlighting."
  :group 'haddock-faces)

(defface haddock-hr-face
  '((t (:inherit haddock-markup-face)))
  "Face for horizontal rules."
  :group 'haddock-faces)

(defface haddock-html-tag-name-face
  '((t (:inherit font-lock-type-face)))
  "Face for HTML tag names."
  :group 'haddock-faces)

(defface haddock-html-tag-delimiter-face
  '((t (:inherit haddock-markup-face)))
  "Face for HTML tag delimiters."
  :group 'haddock-faces)

(defface haddock-html-attr-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for HTML attribute names."
  :group 'haddock-faces)

(defface haddock-html-attr-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for HTML attribute values."
  :group 'haddock-faces)

(defface haddock-html-entity-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for HTML entities."
  :group 'haddock-faces)

(defcustom haddock-header-scaling nil
  "Whether to use variable-height faces for headers.
When non-nil, `haddock-header-face' will inherit from
`variable-pitch' and the scaling values in
`haddock-header-scaling-values' will be applied to
headers of levels one through six respectively."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (haddock-update-header-faces value))
  :group 'haddock-faces
  :package-version '(haddock-mode . "2.2"))

(defcustom haddock-header-scaling-values
  '(2.0 1.7 1.4 1.1 1.0 1.0)
  "List of scaling values for headers of level one through six.
Used when `haddock-header-scaling' is non-nil."
  :type 'list
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (haddock-update-header-faces haddock-header-scaling value))
  :group 'haddock-faces)

(defun haddock-make-header-faces ()
  "Build the faces used for Haddock headers."
  (let ((inherit-faces '(font-lock-function-name-face)))
    (when haddock-header-scaling
      (setq inherit-faces (cons 'variable-pitch inherit-faces)))
    (defface haddock-header-face
      `((t (:inherit ,inherit-faces :weight bold)))
      "Base face for headers."
      :group 'haddock-faces))
  (dotimes (num 6)
    (let* ((num1 (1+ num))
           (face-name (intern (format "haddock-header-face-%s" num1)))
           (scale (if haddock-header-scaling
                      (float (nth num haddock-header-scaling-values))
                    1.0)))
      (eval
       `(defface ,face-name
          '((t (:inherit haddock-header-face :height ,scale)))
          (format "Face for level %s headers.
You probably don't want to customize this face directly. Instead
you can customize the base face `haddock-header-face' or the
variable-height variable `haddock-header-scaling'." ,num1)
          :group 'haddock-faces)))))

(haddock-make-header-faces)

(defun haddock-update-header-faces (&optional scaling scaling-values)
  "Update header faces, depending on if header SCALING is desired.
If so, use given list of SCALING-VALUES relative to the baseline
size of `haddock-header-face'."
  (dotimes (num 6)
    (let* ((face-name (intern (format "haddock-header-face-%s" (1+ num))))
           (scale (cond ((not scaling) 1.0)
                        (scaling-values (float (nth num scaling-values)))
                        (t (float (nth num haddock-header-scaling-values))))))
      (unless (get face-name 'saved-face) ; Don't update customized faces
        (set-face-attribute face-name nil :height scale)))))

(defun haddock-syntactic-face (state)
  "Return font-lock face for characters with given STATE.
See `font-lock-syntactic-face-function' for details."
  (let ((in-comment (nth 4 state)))
    (cond
     (in-comment 'haddock-comment-face)
     (t nil))))

(defcustom haddock-list-item-bullets
  '("●" "◎" "○" "◆" "◇" "►" "•")
  "List of bullets to use for unordered lists.
It can contain any number of symbols, which will be repeated.
Depending on your font, some reasonable choices are:
♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ❀ ◆ ◖ ▶ ► • ★ ▸."
  :group 'haddock
  :type '(repeat (string :tag "Bullet character"))
  :package-version '(haddock-mode . "2.3"))

(defun haddock--footnote-marker-properties ()
  "Return a font-lock facespec expression for footnote marker text."
  `(face haddock-footnote-marker-face
         ,@(when haddock-hide-markup
             `(display ,haddock-footnote-display))))

(defun haddock--pandoc-inline-footnote-properties ()
  "Return a font-lock facespec expression for Pandoc inline footnote text."
  `(face haddock-footnote-text-face
         ,@(when haddock-hide-markup
             `(display ,haddock-footnote-display))))

(defvar haddock-mode-font-lock-keywords
  `((haddock-match-yaml-metadata-begin . ((1 'haddock-markup-face)))
    (haddock-match-yaml-metadata-end . ((1 'haddock-markup-face)))
    (haddock-match-yaml-metadata-key . ((1 'haddock-metadata-key-face)
                                         (2 'haddock-markup-face)
                                         (3 'haddock-metadata-value-face)))
    (haddock-match-gfm-open-code-blocks . ((1 haddock-markup-properties)
                                            (2 haddock-markup-properties nil t)
                                            (3 haddock-language-keyword-properties nil t)
                                            (4 haddock-language-info-properties nil t)
                                            (5 haddock-markup-properties nil t)))
    (haddock-match-gfm-close-code-blocks . ((0 haddock-markup-properties)))
    (haddock-fontify-gfm-code-blocks)
    (haddock-fontify-tables)
    (haddock-match-fenced-start-code-block . ((1 haddock-markup-properties)
                                               (2 haddock-markup-properties nil t)
                                               (3 haddock-language-keyword-properties nil t)
                                               (4 haddock-language-info-properties nil t)
                                               (5 haddock-markup-properties nil t)))
    (haddock-match-fenced-end-code-block . ((0 haddock-markup-properties)))
    (haddock-fontify-fenced-code-blocks)
    (haddock-match-pre-blocks . ((0 'haddock-pre-face)))
    (haddock-fontify-headings)
    (haddock-match-declarative-metadata . ((1 'haddock-metadata-key-face)
                                              (2 'haddock-markup-face)
                                              (3 'haddock-metadata-value-face)))
    (haddock-match-pandoc-metadata . ((1 'haddock-markup-face)
                                       (2 'haddock-markup-face)
                                       (3 'haddock-metadata-value-face)))
    (haddock-fontify-hrs)
    (haddock-match-code . ((1 haddock-markup-properties prepend)
                            (2 'haddock-inline-code-face prepend)
                            (3 haddock-markup-properties prepend)))
    (,haddock-regex-kbd . ((1 haddock-markup-properties)
                            (2 'haddock-inline-code-face)
                            (3 haddock-markup-properties)))
    (haddock-fontify-angle-uris)
    (,haddock-regex-email . 'haddock-plain-url-face)
    (haddock-match-html-tag . ((1 'haddock-html-tag-delimiter-face t)
                                (2 'haddock-html-tag-name-face t)
                                (3 'haddock-html-tag-delimiter-face t)
                                ;; Anchored matcher for HTML tag attributes
                                (,haddock-regex-html-attr
                                 ;; Before searching, move past tag
                                 ;; name; set limit at tag close.
                                 (progn
                                   (goto-char (match-end 2)) (match-end 3))
                                 nil
                                 . ((1 'haddock-html-attr-name-face)
                                    (3 'haddock-html-tag-delimiter-face nil t)
                                    (4 'haddock-html-attr-value-face nil t)))))
    (,haddock-regex-html-entity . 'haddock-html-entity-face)
    (haddock-fontify-list-items)
    (,haddock-regex-footnote . ((1 haddock-markup-properties)    ; [^
                                 (2 (haddock--footnote-marker-properties)) ; label
                                 (3 haddock-markup-properties)))  ; ]
    (,haddock-regex-pandoc-inline-footnote . ((1 haddock-markup-properties)   ; ^
                                               (2 haddock-markup-properties)   ; [
                                               (3 (haddock--pandoc-inline-footnote-properties)) ; text
                                               (4 haddock-markup-properties))) ; ]
    (haddock-match-includes . ((1 haddock-markup-properties)
                                (2 haddock-markup-properties nil t)
                                (3 haddock-include-title-properties nil t)
                                (4 haddock-markup-properties nil t)
                                (5 haddock-markup-properties)
                                (6 'haddock-url-face)
                                (7 haddock-markup-properties)))
    (haddock-fontify-inline-links)
    (haddock-fontify-reference-links)
    (,haddock-regex-reference-definition . ((1 'haddock-markup-face) ; [
                                             (2 'haddock-reference-face) ; label
                                             (3 'haddock-markup-face)    ; ]
                                             (4 'haddock-markup-face)    ; :
                                             (5 'haddock-url-face)       ; url
                                             (6 'haddock-link-title-face))) ; "title" (optional)
    (haddock-fontify-plain-uris)
    ;; Math mode $..$
    (haddock-match-math-single . ((1 'haddock-markup-face prepend)
                                   (2 'haddock-math-face append)
                                   (3 'haddock-markup-face prepend)))
    ;; Math mode $$..$$
    (haddock-match-math-double . ((1 'haddock-markup-face prepend)
                                   (2 'haddock-math-face append)
                                   (3 'haddock-markup-face prepend)))
    ;; Math mode \[..\] and \\[..\\]
    (haddock-match-math-display . ((1 'haddock-markup-face prepend)
                                    (3 'haddock-math-face append)
                                    (4 'haddock-markup-face prepend)))
    (haddock-match-bold . ((1 haddock-markup-properties prepend)
                            (2 'haddock-bold-face append)
                            (3 haddock-markup-properties prepend)))
    (haddock-match-italic . ((1 haddock-markup-properties prepend)
                              (2 'haddock-italic-face append)
                              (3 haddock-markup-properties prepend)))
    (,haddock-regex-strike-through . ((3 haddock-markup-properties)
                                       (4 'haddock-strike-through-face)
                                       (5 haddock-markup-properties)))
    (,haddock-regex-line-break . (1 'haddock-line-break-face prepend))
    (haddock-fontify-sub-superscripts)
    (haddock-match-inline-attributes . ((0 haddock-markup-properties prepend)))
    (haddock-match-leanpub-sections . ((0 haddock-markup-properties)))
    (haddock-fontify-blockquotes)
    (haddock-match-wiki-link . ((0 'haddock-link-face prepend))))
  "Syntax highlighting for Haddock files.")

(define-obsolete-variable-alias
 'haddock-mode-font-lock-keywords-basic
 'haddock-mode-font-lock-keywords "v2.4")

;; Footnotes
(defvar haddock-footnote-counter 0
  "Counter for footnote numbers.")
(make-variable-buffer-local 'haddock-footnote-counter)

(defconst haddock-footnote-chars
  "[[:alnum:]-]"
  "Regular expression matching any character that is allowed in a footnote identifier.")

(defconst haddock-regex-footnote-definition
  (concat "^ \\{0,3\\}\\[\\(\\^" haddock-footnote-chars "*?\\)\\]:\\(?:[ \t]+\\|$\\)")
  "Regular expression matching a footnote definition, capturing the label.")


;;; Compatibility =============================================================

(defun haddock-replace-regexp-in-string (regexp rep string)
  "Replace ocurrences of REGEXP with REP in STRING.
This is a compatibility wrapper to provide `replace-regexp-in-string'
in XEmacs 21."
  (if (featurep 'xemacs)
      (replace-in-string string regexp rep)
    (replace-regexp-in-string regexp rep string)))

;; `haddock-use-region-p' is a compatibility function which checks
;; for an active region, with fallbacks for older Emacsen and XEmacs.
(eval-and-compile
  (cond
   ;; Emacs 24 and newer
   ((fboundp 'use-region-p)
    (defalias 'haddock-use-region-p 'use-region-p))
   ;; XEmacs
   ((fboundp 'region-active-p)
    (defalias 'haddock-use-region-p 'region-active-p))))

;; Use new names for outline-mode functions in Emacs 25 and later.
(eval-and-compile
  (defalias 'haddock-hide-sublevels
    (if (fboundp 'outline-hide-sublevels)
        'outline-hide-sublevels
      'hide-sublevels))
  (defalias 'haddock-show-all
    (if (fboundp 'outline-show-all)
        'outline-show-all
      'show-all))
  (defalias 'haddock-hide-body
    (if (fboundp 'outline-hide-body)
        'outline-hide-body
      'hide-body))
  (defalias 'haddock-show-children
    (if (fboundp 'outline-show-children)
        'outline-show-children
      'show-children))
  (defalias 'haddock-show-subtree
    (if (fboundp 'outline-show-subtree)
        'outline-show-subtree
      'show-subtree))
  (defalias 'haddock-hide-subtree
    (if (fboundp 'outline-hide-subtree)
        'outline-hide-subtree
      'hide-subtree)))

;; Provide directory-name-p to Emacs 24
(defsubst haddock-directory-name-p (name)
  "Return non-nil if NAME ends with a directory separator character.
Taken from `directory-name-p' from Emacs 25 and provided here for
backwards compatibility."
  (let ((len (length name))
        (lastc ?.))
    (if (> len 0)
        (setq lastc (aref name (1- len))))
    (or (= lastc ?/)
        (and (memq system-type '(windows-nt ms-dos))
             (= lastc ?\\)))))

;; Provide a function to find files recursively in Emacs 24.
(defalias 'haddock-directory-files-recursively
  (if (fboundp 'directory-files-recursively)
      'directory-files-recursively
    (lambda (dir regexp)
    "Return list of all files under DIR that have file names matching REGEXP.
This function works recursively.  Files are returned in \"depth first\"
order, and files from each directory are sorted in alphabetical order.
Each file name appears in the returned list in its absolute form.
Based on `directory-files-recursively' from Emacs 25 and provided
here for backwards compatibility."
  (let ((result nil)
        (files nil)
        ;; When DIR is "/", remote file names like "/method:" could
        ;; also be offered.  We shall suppress them.
        (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
                        'string<))
      (unless (member file '("./" "../"))
        (if (haddock-directory-name-p file)
            (let* ((leaf (substring file 0 (1- (length file))))
                   (full-file (expand-file-name leaf dir)))
              (setq result
                    (nconc result (haddock-directory-files-recursively
                                   full-file regexp))))
          (when (string-match-p regexp file)
            (push (expand-file-name file dir) files)))))
    (nconc result (nreverse files))))))

(defun haddock-flyspell-check-word-p ()
  "Return t if `flyspell' should check word just before point.
Used for `flyspell-generic-check-word-predicate'."
  (save-excursion
    (goto-char (1- (point)))
    (not (or (haddock-code-block-at-point-p)
             (haddock-inline-code-at-point-p)
             (haddock-in-comment-p)
             (let ((faces (get-text-property (point) 'face)))
               (if (listp faces)
                   (or (memq 'haddock-reference-face faces)
                       (memq 'haddock-markup-face faces)
                       (memq 'haddock-plain-url-face faces)
                       (memq 'haddock-inline-code-face faces)
                       (memq 'haddock-url-face faces))
                 (memq faces '(haddock-reference-face
                               haddock-markup-face
                               haddock-plain-url-face
                               haddock-inline-code-face
                               haddock-url-face))))))))

(defun haddock-font-lock-ensure ()
  "Provide `font-lock-ensure' in Emacs 24."
  (if (fboundp 'font-lock-ensure)
      (font-lock-ensure)
    (with-no-warnings
      ;; Suppress warning about non-interactive use of
      ;; `font-lock-fontify-buffer' in Emacs 25.
      (font-lock-fontify-buffer))))


;;; Haddock Parsing Functions ================================================

(define-obsolete-function-alias
  'haddock-cur-line-blank 'haddock-cur-line-blank-p "v2.4")
(define-obsolete-function-alias
  'haddock-next-line-blank 'haddock-next-line-blank-p "v2.4")

(defun haddock-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at-p haddock-regex-blank-line)))

(defun haddock-prev-line-blank ()
  "Return t if the previous line is blank and nil otherwise.
If we are at the first line, then consider the previous line to be blank."
  (or (= (line-beginning-position) (point-min))
      (save-excursion
        (forward-line -1)
        (looking-at haddock-regex-blank-line))))

(defun haddock-prev-line-blank-p ()
  "Like `haddock-prev-line-blank', but preserve `match-data'."
  (save-match-data (haddock-prev-line-blank)))

(defun haddock-next-line-blank-p ()
  "Return t if the next line is blank and nil otherwise.
If we are at the last line, then consider the next line to be blank."
  (or (= (line-end-position) (point-max))
      (save-excursion
        (forward-line 1)
        (haddock-cur-line-blank-p))))

(defun haddock-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line.
Return 0 if the current line is the first line in the buffer."
  (save-excursion
    (if (= (line-beginning-position) (point-min))
        0
      (forward-line -1)
      (current-indentation))))

(defun haddock-next-line-indent ()
  "Return the number of leading whitespace characters in the next line.
Return 0 if line is the last line in the buffer."
  (save-excursion
    (if (= (line-end-position) (point-max))
        0
      (forward-line 1)
      (current-indentation))))

(defun haddock-new-baseline ()
  "Determine if the current line begins a new baseline level.
Assume point is positioned at beginning of line."
  (or (looking-at haddock-regex-header)
      (looking-at haddock-regex-hr)
      (and (= (current-indentation) 0)
           (not (looking-at haddock-regex-list))
           (haddock-prev-line-blank))))

(defun haddock-search-backward-baseline ()
  "Search backward baseline point with no indentation and not a list item."
  (end-of-line)
  (let (stop)
    (while (not (or stop (bobp)))
      (re-search-backward haddock-regex-block-separator-noindent nil t)
      (when (match-end 2)
        (goto-char (match-end 2))
        (cond
         ((haddock-new-baseline)
          (setq stop t))
         ((looking-at-p haddock-regex-list)
          (setq stop nil))
         (t (setq stop t)))))))

(defun haddock-update-list-levels (marker indent levels)
  "Update list levels given list MARKER, block INDENT, and current LEVELS.
Here, MARKER is a string representing the type of list, INDENT is an integer
giving the indentation, in spaces, of the current block, and LEVELS is a
list of the indentation levels of parent list items.  When LEVELS is nil,
it means we are at baseline (not inside of a nested list)."
  (cond
   ;; New list item at baseline.
   ((and marker (null levels))
    (setq levels (list indent)))
   ;; List item with greater indentation (four or more spaces).
   ;; Increase list level.
   ((and marker (>= indent (+ (car levels) 4)))
    (setq levels (cons indent levels)))
   ;; List item with greater or equal indentation (less than four spaces).
   ;; Do not increase list level.
   ((and marker (>= indent (car levels)))
    levels)
   ;; Lesser indentation level.
   ;; Pop appropriate number of elements off LEVELS list (e.g., lesser
   ;; indentation could move back more than one list level).  Note
   ;; that this block need not be the beginning of list item.
   ((< indent (car levels))
    (while (and (> (length levels) 1)
                (< indent (+ (cadr levels) 4)))
      (setq levels (cdr levels)))
    levels)
   ;; Otherwise, do nothing.
   (t levels)))

(defun haddock-calculate-list-levels ()
  "Calculate list levels at point.
Return a list of the form (n1 n2 n3 ...) where n1 is the
indentation of the deepest nested list item in the branch of
the list at the point, n2 is the indentation of the parent
list item, and so on.  The depth of the list item is therefore
the length of the returned list.  If the point is not at or
immediately  after a list item, return nil."
  (save-excursion
    (let ((first (point)) levels indent pre-regexp)
      ;; Find a baseline point with zero list indentation
      (haddock-search-backward-baseline)
      ;; Search for all list items between baseline and LOC
      (while (and (< (point) first)
                  (re-search-forward haddock-regex-list first t))
        (setq pre-regexp (format "^\\(    \\|\t\\)\\{%d\\}" (1+ (length levels))))
        (beginning-of-line)
        (cond
         ;; Make sure this is not a header or hr
         ((haddock-new-baseline) (setq levels nil))
         ;; Make sure this is not a line from a pre block
         ((looking-at-p pre-regexp))
         ;; If not, then update levels
         (t
          (setq indent (current-indentation))
          (setq levels (haddock-update-list-levels (match-string 2)
                                                    indent levels))))
        (end-of-line))
      levels)))

(defun haddock-prev-list-item (level)
  "Search backward from point for a list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent prev)
    (setq prev (point))
    (forward-line -1)
    (setq indent (current-indentation))
    (while
        (cond
         ;; List item
         ((and (looking-at-p haddock-regex-list)
               (setq bounds (haddock-cur-list-item-bounds)))
          (cond
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq prev (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq prev nil)
            nil)
           ;; Stop at beginning of buffer
           ((bobp) (setq prev nil))
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)))
         ;; Stop at beginning of buffer
         ((bobp) (setq prev nil))
         ;; Continue if current line is blank
         ((haddock-cur-line-blank-p) t)
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the next is blank
         ((and (< indent level)
               (haddock-next-line-blank-p))
          (setq prev nil))
         ;; Stop at a header
         ((looking-at-p haddock-regex-header) (setq prev nil))
         ;; Stop at a horizontal rule
         ((looking-at-p haddock-regex-hr) (setq prev nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line -1)
      (setq indent (current-indentation)))
    prev))

(defun haddock-next-list-item (level)
  "Search forward from point for the next list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent next)
    (setq next (point))
    (if (looking-at haddock-regex-header-setext)
        (goto-char (match-end 0)))
    (forward-line)
    (setq indent (current-indentation))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) nil)
         ;; Continue if the current line is blank
         ((haddock-cur-line-blank-p) t)
         ;; List item
         ((and (looking-at-p haddock-regex-list)
               (setq bounds (haddock-cur-list-item-bounds)))
          (cond
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq next (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq next nil)
            nil)))
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (haddock-prev-line-blank-p))
          (setq next nil))
         ;; Stop at a header
         ((looking-at-p haddock-regex-header) (setq next nil))
         ;; Stop at a horizontal rule
         ((looking-at-p haddock-regex-hr) (setq next nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (current-indentation)))
    next))

(defun haddock-cur-list-item-end (level)
  "Move to end of list item with pre-marker indentation LEVEL.
Return the point at the end when a list item was found at the
original point.  If the point is not in a list item, do nothing."
  (let (indent)
    (forward-line)
    (setq indent (current-indentation))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) nil)
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Continue if the current line is blank
         ((looking-at haddock-regex-blank-line) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (haddock-prev-line-blank))
          nil)
         ;; Stop at a new list items of the same or lesser
         ;; indentation, headings, and horizontal rules.
         ((looking-at (concat "\\(?:" haddock-regex-list
                              "\\|" haddock-regex-header
                              "\\|" haddock-regex-hr "\\)"))
          nil)
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (current-indentation)))
    ;; Don't skip over whitespace for empty list items (marker and
    ;; whitespace only), just move to end of whitespace.
    (if (save-excursion
          (beginning-of-line)
          (looking-at (concat haddock-regex-list "[ \t]*$")))
        (goto-char (match-end 3))
      (skip-chars-backward " \t\n"))
    (end-of-line)
    (point)))

(defun haddock-cur-list-item-bounds ()
  "Return bounds for list item at point.
Return a list of the following form:

    (begin end indent nonlist-indent marker checkbox match)

The named components are:

  - begin: Position of beginning of list item, including leading indentation.
  - end: Position of the end of the list item, including list item text.
  - indent: Number of characters of indentation before list marker (an integer).
  - nonlist-indent: Number characters of indentation, list
    marker, and whitespace following list marker (an integer).
  - marker: String containing the list marker and following whitespace
            (e.g., \"- \" or \"* \").
  - checkbox: String containing the GFM checkbox portion, if any,
    including any trailing whitespace before the text
    begins (e.g., \"[x] \").
  - match: match data for haddock-regex-list

As an example, for the following unordered list item

   - item

the returned list would be

    (1 14 3 5 \"- \" nil (1 6 1 4 4 5 5 6))

If the point is not inside a list item, return nil."
  (car (get-text-property (point-at-bol) 'haddock-list-item)))

(defun haddock-list-item-at-point-p ()
  "Return t if there is a list item at the point and nil otherwise."
  (save-match-data (haddock-cur-list-item-bounds)))

(defun haddock-prev-list-item-bounds ()
  "Return bounds of previous item in the same list of any level.
The return value has the same form as that of
`haddock-cur-list-item-bounds'."
  (save-excursion
    (let ((cur-bounds (haddock-cur-list-item-bounds))
          (beginning-of-list (save-excursion (haddock-beginning-of-list)))
          stop)
      (when cur-bounds
        (goto-char (nth 0 cur-bounds))
        (while (and (not stop) (not (bobp))
                    (re-search-backward haddock-regex-list
                                        beginning-of-list t))
          (unless (or (looking-at haddock-regex-hr)
                      (haddock-code-block-at-point-p))
            (setq stop (point))))
        (haddock-cur-list-item-bounds)))))

(defun haddock-next-list-item-bounds ()
  "Return bounds of next item in the same list of any level.
The return value has the same form as that of
`haddock-cur-list-item-bounds'."
  (save-excursion
    (let ((cur-bounds (haddock-cur-list-item-bounds))
          (end-of-list (save-excursion (haddock-end-of-list)))
          stop)
      (when cur-bounds
        (goto-char (nth 0 cur-bounds))
        (end-of-line)
        (while (and (not stop) (not (eobp))
                    (re-search-forward haddock-regex-list
                                       end-of-list t))
          (unless (or (looking-at haddock-regex-hr)
                      (haddock-code-block-at-point-p))
            (setq stop (point))))
        (when stop
          (haddock-cur-list-item-bounds))))))

(defun haddock-beginning-of-list ()
  "Move point to beginning of list at point, if any."
  (interactive)
  (let ((orig-point (point))
        (list-begin (save-excursion
                      (haddock-search-backward-baseline)
                      ;; Stop at next list item, regardless of the indentation.
                      (haddock-next-list-item (point-max))
                      (when (looking-at haddock-regex-list)
                        (point)))))
    (when (and list-begin (<= list-begin orig-point))
      (goto-char list-begin))))

(defun haddock-end-of-list ()
  "Move point to end of list at point, if any."
  (interactive)
  (let ((start (point))
        (end (save-excursion
               (when (haddock-beginning-of-list)
                 ;; Items can't have nonlist-indent <= 1, so this
                 ;; moves past all list items.
                 (haddock-next-list-item 1)
                 (skip-syntax-backward "-")
                 (unless (eobp) (forward-char 1))
                 (point)))))
    (when (and end (>= end start))
      (goto-char end))))

(defun haddock-up-list ()
  "Move point to beginning of parent list item."
  (interactive)
  (let ((cur-bounds (haddock-cur-list-item-bounds)))
    (when cur-bounds
      (haddock-prev-list-item (1- (nth 3 cur-bounds)))
      (let ((up-bounds (haddock-cur-list-item-bounds)))
        (when (and up-bounds (< (nth 3 up-bounds) (nth 3 cur-bounds)))
          (point))))))

(defun haddock-bounds-of-thing-at-point (thing)
  "Call `bounds-of-thing-at-point' for THING with slight modifications.
Does not include trailing newlines when THING is 'line.  Handles the
end of buffer case by setting both endpoints equal to the value of
`point-max', since an empty region will trigger empty markup insertion.
Return bounds of form (beg . end) if THING is found, or nil otherwise."
  (let* ((bounds (bounds-of-thing-at-point thing))
         (a (car bounds))
         (b (cdr bounds)))
    (when bounds
      (when (eq thing 'line)
        (cond ((and (eobp) (haddock-cur-line-blank-p))
               (setq a b))
              ((char-equal (char-before b) ?\^J)
               (setq b (1- b)))))
      (cons a b))))

(defun haddock-reference-definition (reference)
  "Find out whether Haddock REFERENCE is defined.
REFERENCE should not include the square brackets.
When REFERENCE is defined, return a list of the form (text start end)
containing the definition text itself followed by the start and end
locations of the text.  Otherwise, return nil.
Leave match data for `haddock-regex-reference-definition'
intact additional processing."
  (let ((reference (downcase reference)))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward haddock-regex-reference-definition nil t)
          (when (string= reference (downcase (match-string-no-properties 2)))
            (throw 'found
                   (list (match-string-no-properties 5)
                         (match-beginning 5) (match-end 5)))))))))

(defun haddock-get-defined-references ()
  "Return all defined reference labels and their line numbers (not including square brackets)."
  (save-excursion
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward haddock-regex-reference-definition nil t)
        (let ((target (match-string-no-properties 2)))
          (cl-pushnew
           (cons (downcase target)
                 (haddock-line-number-at-pos (match-beginning 2)))
           refs :test #'equal :key #'car)))
      (reverse refs))))

(defun haddock-get-used-uris ()
  "Return a list of all used URIs in the buffer."
  (save-excursion
    (goto-char (point-min))
    (let (uris)
      (while (re-search-forward
              (concat "\\(?:" haddock-regex-link-inline
                      "\\|" haddock-regex-angle-uri
                      "\\|" haddock-regex-uri
                      "\\|" haddock-regex-email
                      "\\)")
              nil t)
        (unless (or (haddock-inline-code-at-point-p)
                    (haddock-code-block-at-point-p))
          (cl-pushnew (or (match-string-no-properties 6)
                          (match-string-no-properties 10)
                          (match-string-no-properties 12)
                          (match-string-no-properties 13))
                      uris :test #'equal)))
      (reverse uris))))

(defun haddock-inline-code-at-pos (pos)
  "Return non-nil if there is an inline code fragment at POS.
Return nil otherwise.  Set match data according to
`haddock-match-code' upon success.
This function searches the block for a code fragment that
contains the point using `haddock-match-code'.  We do this
because `thing-at-point-looking-at' does not work reliably with
`haddock-regex-code'.

The match data is set as follows:
Group 1 matches the opening backquotes.
Group 2 matches the code fragment itself, without backquotes.
Group 3 matches the closing backquotes."
  (save-excursion
    (goto-char pos)
    (let ((old-point (point))
          (end-of-block (progn (haddock-end-of-text-block) (point)))
          found)
      (haddock-beginning-of-text-block)
      (while (and (haddock-match-code end-of-block)
                  (setq found t)
                  (< (match-end 0) old-point)))
      (and found                              ; matched something
           (<= (match-beginning 0) old-point) ; match contains old-point
           (> (match-end 0) old-point)))))

(defun haddock-inline-code-at-pos-p (pos)
  "Return non-nil if there is an inline code fragment at POS.
Like `haddock-inline-code-at-pos`, but preserves match data."
  (save-match-data (haddock-inline-code-at-pos pos)))

(defun haddock-inline-code-at-point ()
  "Return non-nil if the point is at an inline code fragment.
See `haddock-inline-code-at-pos' for details."
  (haddock-inline-code-at-pos (point)))

(defun haddock-inline-code-at-point-p (&optional pos)
  "Return non-nil if there is inline code at the POS.
This is a predicate function counterpart to
`haddock-inline-code-at-point' which does not modify the match
data.  See `haddock-code-block-at-point-p' for code blocks."
  (save-match-data (haddock-inline-code-at-pos (or pos (point)))))

(make-obsolete 'haddock-code-at-point-p 'haddock-inline-code-at-point-p "v2.2")

(defun haddock-code-block-at-pos (pos)
  "Return match data list if there is a code block at POS.
Uses text properties at the beginning of the line position.
This includes pre blocks, tilde-fenced code blocks, and GFM
quoted code blocks.  Return nil otherwise."
  (let ((bol (save-excursion (goto-char pos) (point-at-bol))))
    (or (get-text-property bol 'haddock-pre)
        (let* ((bounds (haddock-get-enclosing-fenced-block-construct pos))
               (second (cl-second bounds)))
          (if second
              ;; chunks are right open
              (when (< pos second)
                bounds)
            bounds)))))

;; Function was renamed to emphasize that it does not modify match-data.
(defalias 'haddock-code-block-at-point 'haddock-code-block-at-point-p)

(defun haddock-code-block-at-point-p (&optional pos)
  "Return non-nil if there is a code block at the POS.
This includes pre blocks, tilde-fenced code blocks, and GFM
quoted code blocks.  This function does not modify the match
data.  See `haddock-inline-code-at-point-p' for inline code."
  (save-match-data (haddock-code-block-at-pos (or pos (point)))))

(defun haddock-heading-at-point (&optional pos)
  "Return non-nil if there is a heading at the POS.
Set match data for `haddock-regex-header'."
  (let ((match-data (get-text-property (or pos (point)) 'haddock-heading)))
    (when match-data
      (set-match-data match-data)
      t)))

(defun haddock-pipe-at-bol-p ()
  "Return non-nil if the line begins with a pipe symbol.
This may be useful for tables and Pandoc's line_blocks extension."
  (char-equal (char-after (point-at-bol)) ?|))


;;; Haddock Font Lock Matching Functions =====================================

(defun haddock-range-property-any (begin end prop prop-values)
  "Return t if PROP from BEGIN to END is equal to one of the given PROP-VALUES.
Also returns t if PROP is a list containing one of the PROP-VALUES.
Return nil otherwise."
  (let (props)
    (catch 'found
      (dolist (loc (number-sequence begin end))
        (when (setq props (get-text-property loc prop))
          (cond ((listp props)
                 ;; props is a list, check for membership
                 (dolist (val prop-values)
                   (when (memq val props) (throw 'found loc))))
                (t
                 ;; props is a scalar, check for equality
                 (dolist (val prop-values)
                   (when (eq val props) (throw 'found loc))))))))))

(defun haddock-range-properties-exist (begin end props)
  (cl-loop
   for loc in (number-sequence begin end)
   with result = nil
   while (not
          (setq result
                (cl-some (lambda (prop) (get-text-property loc prop)) props)))
   finally return result))

(defun haddock-match-inline-generic (regex last &optional faceless)
  "Match inline REGEX from the point to LAST.
When FACELESS is non-nil, do not return matches where faces have been applied."
  (when (re-search-forward regex last t)
    (let ((bounds (haddock-code-block-at-pos (match-beginning 1)))
          (face (and faceless (text-property-not-all
                               (match-beginning 0) (match-end 0) 'face nil))))
      (cond
       ;; In code block: move past it and recursively search again
       (bounds
        (when (< (goto-char (cl-second bounds)) last)
          (haddock-match-inline-generic regex last faceless)))
       ;; When faces are found in the match range, skip over the match and
       ;; recursively search again.
       (face
        (when (< (goto-char (match-end 0)) last)
          (haddock-match-inline-generic regex last faceless)))
       ;; Keep match data and return t when in bounds.
       (t
        (<= (match-end 0) last))))))

(defun haddock-match-code (last)
  "Match inline code fragments from point to LAST."
  (unless (bobp)
    (backward-char 1))
  (when (haddock-search-until-condition
         (lambda ()
           (and
            ;; Advance point in case of failure, but without exceeding last.
            (goto-char (min (1+ (match-beginning 1)) last))
            (not (haddock-in-comment-p (match-beginning 1)))
            (not (haddock-in-comment-p (match-end 1)))
            (not (haddock-code-block-at-pos (match-beginning 1)))))
         haddock-regex-code last t)
      (set-match-data (list (match-beginning 1) (match-end 1)
                            (match-beginning 2) (match-end 2)
                            (match-beginning 3) (match-end 3)
                            (match-beginning 4) (match-end 4)))
      (goto-char (min (1+ (match-end 0)) last (point-max)))
      t))

(defun haddock-match-bold (last)
  "Match inline bold from the point to LAST."
  (when (haddock-match-inline-generic haddock-regex-bold last)
    (let ((begin (match-beginning 2))
          (end (match-end 2)))
      (if (or (haddock-inline-code-at-pos-p begin)
              (haddock-inline-code-at-pos-p end)
              (haddock-in-comment-p)
              (haddock-range-property-any
               begin begin 'face '(haddock-url-face
                                   haddock-plain-url-face))
              (haddock-range-property-any
               begin end 'face '(haddock-hr-face
                                 haddock-math-face)))
          (progn (goto-char (min (1+ begin) last))
                 (when (< (point) last)
                   (haddock-match-italic last)))
        (set-match-data (list (match-beginning 2) (match-end 2)
                              (match-beginning 3) (match-end 3)
                              (match-beginning 4) (match-end 4)
                              (match-beginning 5) (match-end 5)))
        t))))

(defun haddock-match-italic (last)
  "Match inline italics from the point to LAST."
  (let ((regex (if (memq major-mode '(gfm-mode gfm-view-mode))
                   haddock-regex-gfm-italic haddock-regex-italic)))
    (when (haddock-match-inline-generic regex last)
      (let ((begin (match-beginning 1))
            (end (match-end 1)))
        (if (or (haddock-inline-code-at-pos-p begin)
                (haddock-inline-code-at-pos-p end)
                (haddock-in-comment-p)
                (haddock-range-property-any
                 begin begin 'face '(haddock-url-face
                                     haddock-plain-url-face))
                (haddock-range-property-any
                 begin end 'face '(haddock-bold-face
                                   haddock-list-face
                                   haddock-hr-face
                                   haddock-math-face)))
            (progn (goto-char (min (1+ begin) last))
                   (when (< (point) last)
                     (haddock-match-italic last)))
          (set-match-data (list (match-beginning 1) (match-end 1)
                                (match-beginning 2) (match-end 2)
                                (match-beginning 3) (match-end 3)
                                (match-beginning 4) (match-end 4)))
          t)))))

(defun haddock-match-math-generic (regex last)
  "Match REGEX from point to LAST.
REGEX is either `haddock-regex-math-inline-single' for matching
$..$ or `haddock-regex-math-inline-double' for matching $$..$$."
  (when (and haddock-enable-math (haddock-match-inline-generic regex last))
    (let ((begin (match-beginning 1)) (end (match-end 1)))
      (prog1
          (if (or (haddock-range-property-any
                   begin end 'face
                   '(haddock-inline-code-face haddock-bold-face))
                  (haddock-range-properties-exist
                   begin end
                   (haddock-get-fenced-block-middle-properties)))
              (haddock-match-math-generic regex last)
            t)
        (goto-char (1+ (match-end 0)))))))

(defun haddock-match-list-items (last)
  "Match list items from point to LAST."
  (let* ((first (point))
         (pos first)
         (prop 'haddock-list-item)
         (bounds (car (get-text-property pos prop))))
    (while
        (and (or (null (setq bounds (car (get-text-property pos prop))))
                 (< (cl-first bounds) pos))
             (< (point) last)
             (setq pos (next-single-property-change pos prop nil last))
             (goto-char pos)))
    (when bounds
      (set-match-data (cl-seventh bounds))
      ;; Step at least one character beyond point. Otherwise
      ;; `font-lock-fontify-keywords-region' infloops.
      (goto-char (min (1+ (max (point-at-eol) first))
                      (point-max)))
      t)))

(defun haddock-match-math-single (last)
  "Match single quoted $..$ math from point to LAST."
  (haddock-match-math-generic haddock-regex-math-inline-single last))

(defun haddock-match-math-double (last)
  "Match double quoted $$..$$ math from point to LAST."
  (haddock-match-math-generic haddock-regex-math-inline-double last))

(defun haddock-match-math-display (last)
  "Match bracketed display math \[..\] and \\[..\\] from point to LAST."
  (haddock-match-math-generic haddock-regex-math-display last))

(defun haddock-match-propertized-text (property last)
  "Match text with PROPERTY from point to LAST.
Restore match data previously stored in PROPERTY."
  (let ((saved (get-text-property (point) property))
        pos)
    (unless saved
      (setq pos (next-single-property-change (point) property nil last))
      (unless (= pos last)
        (setq saved (get-text-property pos property))))
    (when saved
      (set-match-data saved)
      ;; Step at least one character beyond point. Otherwise
      ;; `font-lock-fontify-keywords-region' infloops.
      (goto-char (min (1+ (max (match-end 0) (point)))
                      (point-max)))
      saved)))

(defun haddock-match-pre-blocks (last)
  "Match preformatted blocks from point to LAST.
Use data stored in 'haddock-pre text property during syntax
analysis."
  (haddock-match-propertized-text 'haddock-pre last))

(defun haddock-match-gfm-code-blocks (last)
  "Match GFM quoted code blocks from point to LAST.
Use data stored in 'haddock-gfm-code text property during syntax
analysis."
  (haddock-match-propertized-text 'haddock-gfm-code last))

(defun haddock-match-gfm-open-code-blocks (last)
  (haddock-match-propertized-text 'haddock-gfm-block-begin last))

(defun haddock-match-gfm-close-code-blocks (last)
  (haddock-match-propertized-text 'haddock-gfm-block-end last))

(defun haddock-match-fenced-code-blocks (last)
  "Match fenced code blocks from the point to LAST."
  (haddock-match-propertized-text 'haddock-fenced-code last))

(defun haddock-match-fenced-start-code-block (last)
  (haddock-match-propertized-text 'haddock-tilde-fence-begin last))

(defun haddock-match-fenced-end-code-block (last)
  (haddock-match-propertized-text 'haddock-tilde-fence-end last))

(defun haddock-match-blockquotes (last)
  "Match blockquotes from point to LAST.
Use data stored in 'haddock-blockquote text property during syntax
analysis."
  (haddock-match-propertized-text 'haddock-blockquote last))

(defun haddock-match-hr (last)
  "Match horizontal rules comments from the point to LAST."
  (haddock-match-propertized-text 'haddock-hr last))

(defun haddock-match-comments (last)
  "Match HTML comments from the point to LAST."
  (when (and (skip-syntax-forward "^<" last))
    (let ((beg (point)))
      (when (and (skip-syntax-forward "^>" last) (< (point) last))
        (forward-char)
        (set-match-data (list beg (point)))
        t))))

(defun haddock-match-generic-links (last ref)
  "Match inline links from point to LAST.
When REF is non-nil, match reference links instead of standard
links with URLs.
This function should only be used during font-lock, as it
determines syntax based on the presence of faces for previously
processed elements."
  ;; Search for the next potential link (not in a code block).
  (let ((prohibited-faces '(haddock-pre-face
                            haddock-code-face
                            haddock-inline-code-face
                            haddock-comment-face))
        found)
    (while
        (and (not found) (< (point) last)
             (progn
               ;; Clear match data to test for a match after functions returns.
               (set-match-data nil)
               ;; Preliminary regular expression search so we can return
               ;; quickly upon failure.  This doesn't handle malformed links
               ;; or nested square brackets well, so if it passes we back up
               ;; continue with a more precise search.
               (re-search-forward
                (if ref
                    haddock-regex-link-reference
                  haddock-regex-link-inline)
                last 'limit)))
      ;; Keep searching if this is in a code block, inline code, or a
      ;; comment, or if it is include syntax. The link text portion
      ;; (group 3) may contain inline code or comments, but the
      ;; markup, URL, and title should not be part of such elements.
      (if (or (haddock-range-property-any
               (match-beginning 0) (match-end 2) 'face prohibited-faces)
              (haddock-range-property-any
               (match-beginning 4) (match-end 0) 'face prohibited-faces)
              (and (char-equal (char-after (point-at-bol)) ?<)
                   (char-equal (char-after (1+ (point-at-bol))) ?<)))
          (set-match-data nil)
        (setq found t))))
  ;; Match opening exclamation point (optional) and left bracket.
  (when (match-beginning 2)
    (let* ((bang (match-beginning 1))
           (first-begin (match-beginning 2))
           ;; Find end of block to prevent matching across blocks.
           (end-of-block (save-excursion
                           (progn
                             (goto-char (match-beginning 2))
                             (haddock-end-of-text-block)
                             (point))))
           ;; Move over balanced expressions to closing right bracket.
           ;; Catch unbalanced expression errors and return nil.
           (first-end (condition-case nil
                           (and (goto-char first-begin)
                                (scan-sexps (point) 1))
                         (error nil)))
           ;; Continue with point at CONT-POINT upon failure.
           (cont-point (min (1+ first-begin) last))
           second-begin second-end url-begin url-end
           title-begin title-end)
      ;; When bracket found, in range, and followed by a left paren/bracket...
      (when (and first-end (< first-end end-of-block) (goto-char first-end)
                 (char-equal (char-after (point)) (if ref ?\[ ?\()))
        ;; Scan across balanced expressions for closing parenthesis/bracket.
        (setq second-begin (point)
              second-end (condition-case nil
                            (scan-sexps (point) 1)
                          (error nil)))
        ;; Check that closing parenthesis/bracket is in range.
        (if (and second-end (<= second-end end-of-block) (<= second-end last))
            (progn
              ;; Search for (optional) title inside closing parenthesis
              (when (and (not ref) (search-forward "\"" second-end t))
                (setq title-begin (1- (point))
                      title-end (and (goto-char second-end)
                                     (search-backward "\"" (1+ title-begin) t))
                      title-end (and title-end (1+ title-end))))
              ;; Store URL/reference range
              (setq url-begin (1+ second-begin)
                    url-end (1- (or title-begin second-end)))
              ;; Set match data, move point beyond link, and return
              (set-match-data
               (list (or bang first-begin) second-end  ; 0 - all
                     bang (and bang (1+ bang))         ; 1 - bang
                     first-begin (1+ first-begin)      ; 2 - markup
                     (1+ first-begin) (1- first-end)   ; 3 - link text
                     (1- first-end) first-end          ; 4 - markup
                     second-begin (1+ second-begin)    ; 5 - markup
                     url-begin url-end                 ; 6 - url/reference
                     title-begin title-end             ; 7 - title
                     (1- second-end) second-end))      ; 8 - markup
              ;; Nullify cont-point and leave point at end and
              (setq cont-point nil)
              (goto-char second-end))
          ;; If no closing parenthesis in range, update continuation point
          (setq cont-point (min end-of-block second-begin))))
      (cond
       ;; On failure, continue searching at cont-point
       ((and cont-point (< cont-point last))
        (goto-char cont-point)
        (haddock-match-generic-links last ref))
       ;; No more text, return nil
       ((and cont-point (= cont-point last))
        nil)
       ;; Return t if a match occurred
       (t t)))))

(defun haddock-match-angle-uris (last)
  "Match angle bracket URIs from point to LAST."
  (when (haddock-match-inline-generic haddock-regex-angle-uri last)
    (goto-char (1+ (match-end 0)))))

(defun haddock-match-plain-uris (last)
  "Match plain URIs from point to LAST."
  (when (haddock-match-inline-generic haddock-regex-uri last t)
    (goto-char (1+ (match-end 0)))))

(defvar haddock-conditional-search-function #'re-search-forward
  "Conditional search function used in `haddock-search-until-condition'.
Made into a variable to allow for dynamic let-binding.")

(defun haddock-search-until-condition (condition &rest args)
  (let (ret)
    (while (and (not ret) (apply haddock-conditional-search-function args))
      (setq ret (funcall condition)))
    ret))

(defun haddock-match-generic-metadata (regexp last)
  "Match metadata declarations specified by REGEXP from point to LAST.
These declarations must appear inside a metadata block that begins at
the beginning of the buffer and ends with a blank line (or the end of
the buffer)."
  (let* ((first (point))
         (end-re "\n[ \t]*\n\\|\n\\'\\|\\'")
         (block-begin (goto-char 1))
         (block-end (re-search-forward end-re nil t)))
    (if (and block-end (> first block-end))
        ;; Don't match declarations if there is no metadata block or if
        ;; the point is beyond the block.  Move point to point-max to
        ;; prevent additional searches and return return nil since nothing
        ;; was found.
        (progn (goto-char (point-max)) nil)
      ;; If a block was found that begins before LAST and ends after
      ;; point, search for declarations inside it.  If the starting is
      ;; before the beginning of the block, start there. Otherwise,
      ;; move back to FIRST.
      (goto-char (if (< first block-begin) block-begin first))
      (if (re-search-forward regexp (min last block-end) t)
          ;; If a metadata declaration is found, set match-data and return t.
          (let ((key-beginning (match-beginning 1))
                (key-end (match-end 1))
                (markup-begin (match-beginning 2))
                (markup-end (match-end 2))
                (value-beginning (match-beginning 3)))
            (set-match-data (list key-beginning (point) ; complete metadata
                                  key-beginning key-end ; key
                                  markup-begin markup-end ; markup
                                  value-beginning (point))) ; value
            t)
        ;; Otherwise, move the point to last and return nil
        (goto-char last)
        nil))))

(defun haddock-match-declarative-metadata (last)
  "Match declarative metadata from the point to LAST."
  (haddock-match-generic-metadata haddock-regex-declarative-metadata last))

(defun haddock-match-pandoc-metadata (last)
  "Match Pandoc metadata from the point to LAST."
  (haddock-match-generic-metadata haddock-regex-pandoc-metadata last))

(defun haddock-match-yaml-metadata-begin (last)
  (haddock-match-propertized-text 'haddock-yaml-metadata-begin last))

(defun haddock-match-yaml-metadata-end (last)
  (haddock-match-propertized-text 'haddock-yaml-metadata-end last))

(defun haddock-match-yaml-metadata-key (last)
  (haddock-match-propertized-text 'haddock-metadata-key last))

(defun haddock-match-wiki-link (last)
  "Match wiki links from point to LAST."
  (when (and haddock-enable-wiki-links
             (not haddock-wiki-link-fontify-missing)
             (haddock-match-inline-generic haddock-regex-wiki-link last))
    (let ((begin (match-beginning 1)) (end (match-end 1)))
      (if (or (haddock-in-comment-p begin)
              (haddock-in-comment-p end)
              (haddock-inline-code-at-pos-p begin)
              (haddock-inline-code-at-pos-p end)
              (haddock-code-block-at-pos begin))
          (progn (goto-char (min (1+ begin) last))
                 (when (< (point) last)
                   (haddock-match-wiki-link last)))
        (set-match-data (list begin end))
        t))))

(defun haddock-match-inline-attributes (last)
  "Match inline attributes from point to LAST."
  (when (haddock-match-inline-generic haddock-regex-inline-attributes last)
    (unless (or (haddock-inline-code-at-pos-p (match-beginning 0))
                (haddock-inline-code-at-pos-p (match-end 0))
                (haddock-in-comment-p))
      t)))

(defun haddock-match-leanpub-sections (last)
  "Match Leanpub section markers from point to LAST."
  (when (haddock-match-inline-generic haddock-regex-leanpub-sections last)
    (unless (or (haddock-inline-code-at-pos-p (match-beginning 0))
                (haddock-inline-code-at-pos-p (match-end 0))
                (haddock-in-comment-p))
      t)))

(defun haddock-match-includes (last)
  "Match include statements from point to LAST.
Sets match data for the following seven groups:
Group 1: opening two angle brackets
Group 2: opening title delimiter (optional)
Group 3: title text (optional)
Group 4: closing title delimiter (optional)
Group 5: opening filename delimiter
Group 6: filename
Group 7: closing filename delimiter"
  (when (haddock-match-inline-generic haddock-regex-include last)
    (let ((valid (not (or (haddock-in-comment-p (match-beginning 0))
                          (haddock-in-comment-p (match-end 0))
                          (haddock-code-block-at-pos (match-beginning 0))))))
      (cond
       ;; Parentheses and maybe square brackets, but no curly braces:
       ;; match optional title in square brackets and file in parentheses.
       ((and valid (match-beginning 5)
             (not (match-beginning 8)))
        (set-match-data (list (match-beginning 1) (match-end 7)
                              (match-beginning 1) (match-end 1)
                              (match-beginning 2) (match-end 2)
                              (match-beginning 3) (match-end 3)
                              (match-beginning 4) (match-end 4)
                              (match-beginning 5) (match-end 5)
                              (match-beginning 6) (match-end 6)
                              (match-beginning 7) (match-end 7))))
       ;; Only square brackets present: match file in square brackets.
       ((and valid (match-beginning 2)
             (not (match-beginning 5))
             (not (match-beginning 7)))
        (set-match-data (list (match-beginning 1) (match-end 4)
                              (match-beginning 1) (match-end 1)
                              nil nil
                              nil nil
                              nil nil
                              (match-beginning 2) (match-end 2)
                              (match-beginning 3) (match-end 3)
                              (match-beginning 4) (match-end 4))))
       ;; Only curly braces present: match file in curly braces.
       ((and valid (match-beginning 8)
             (not (match-beginning 2))
             (not (match-beginning 5)))
        (set-match-data (list (match-beginning 1) (match-end 10)
                              (match-beginning 1) (match-end 1)
                              nil nil
                              nil nil
                              nil nil
                              (match-beginning 8) (match-end 8)
                              (match-beginning 9) (match-end 9)
                              (match-beginning 10) (match-end 10))))
       (t
        ;; Not a valid match, move to next line and search again.
        (forward-line)
        (when (< (point) last)
          (setq valid (haddock-match-includes last)))))
      valid)))

(defun haddock-match-html-tag (last)
  "Match HTML tags from point to LAST."
  (when (and haddock-enable-html
             (haddock-match-inline-generic haddock-regex-html-tag last t))
    (set-match-data (list (match-beginning 0) (match-end 0)
                          (match-beginning 1) (match-end 1)
                          (match-beginning 2) (match-end 2)
                          (match-beginning 9) (match-end 9)))
    t))


;;; Haddock Font Fontification Functions =====================================

(defun haddock--first-displayable (seq)
  "Return the first displayable character or string in SEQ.
SEQ may be an atom or a sequence."
  (let ((seq (if (listp seq) seq (list seq))))
    (cond ((stringp (car seq))
           (cl-find-if
            (lambda (str)
              (and (mapcar #'char-displayable-p (string-to-list str))))
            seq))
          ((characterp (car seq))
           (cl-find-if #'char-displayable-p seq)))))

(defun haddock--marginalize-string (level)
  "Generate atx markup string of given LEVEL for left margin."
  (let ((margin-left-space-count
         (- haddock-marginalize-headers-margin-width level)))
    (concat (make-string margin-left-space-count ? )
                           (make-string level ?#))))

(defun haddock-marginalize-update-current ()
  "Update the window configuration to create a left margin."
  ;; Emacs 25 or later is needed for window-font-width and default-font-width.
  (if (and (fboundp 'window-font-width) (fboundp 'default-font-width))
      (let* ((header-delimiter-font-width
              (window-font-width nil 'haddock-header-delimiter-face))
             (margin-pixel-width (* haddock-marginalize-headers-margin-width
                                    header-delimiter-font-width))
             (margin-char-width (/ margin-pixel-width (default-font-width))))
        (set-window-margins nil margin-char-width))
    ;; As a fallback, simply set margin based on character count.
    (set-window-margins nil haddock-marginalize-headers-margin-width)))

(defun haddock-fontify-headings (last)
  "Add text properties to headings from point to LAST."
  (when (haddock-match-propertized-text 'haddock-heading last)
    (let* ((level (haddock-outline-level))
           (heading-face
            (intern (format "haddock-header-face-%d" level)))
           (heading-props `(face ,heading-face))
           (left-markup-props
            `(face haddock-header-delimiter-face
                   ,@(cond
                      (haddock-hide-markup
                       `(display ""))
                      (haddock-marginalize-headers
                       `(display ((margin left-margin)
                                  ,(haddock--marginalize-string level)))))))
           (right-markup-props
            `(face haddock-header-delimiter-face
                   ,@(when haddock-hide-markup `(display ""))))
           (rule-props `(face haddock-header-rule-face
                              ,@(when haddock-hide-markup `(display "")))))
      (if (match-end 1)
          ;; Setext heading
          (progn (add-text-properties
                  (match-beginning 1) (match-end 1) heading-props)
                 (if (= level 1)
                     (add-text-properties
                      (match-beginning 2) (match-end 2) rule-props)
                   (add-text-properties
                    (match-beginning 3) (match-end 3) rule-props)))
        ;; atx heading
        (add-text-properties
         (match-beginning 4) (match-end 4) left-markup-props)
        (add-text-properties
         (match-beginning 5) (match-end 5) heading-props)
        (when (match-end 6)
          (add-text-properties
           (match-beginning 6) (match-end 6) right-markup-props))))
    t))

(defun haddock-fontify-tables (last)
  (when (and (re-search-forward "|" last t)
             (haddock-table-at-point-p))
    (font-lock-append-text-property
     (line-beginning-position) (min (1+ (line-end-position)) (point-max))
     'face 'haddock-table-face)
    (forward-line 1)
    t))

(defun haddock-fontify-blockquotes (last)
  "Apply font-lock properties to blockquotes from point to LAST."
  (when (haddock-match-blockquotes last)
    (let ((display-string
           (haddock--first-displayable haddock-blockquote-display-char)))
      (add-text-properties
       (match-beginning 1) (match-end 1)
       (if haddock-hide-markup
           `(face haddock-blockquote-face display ,display-string)
         `(face haddock-markup-face)))
      (font-lock-append-text-property
       (match-beginning 0) (match-end 0) 'face 'haddock-blockquote-face)
      t)))

(defun haddock-fontify-list-items (last)
  "Apply font-lock properties to list markers from point to LAST."
  (when (haddock-match-list-items last)
    (let* ((indent (length (match-string-no-properties 1)))
           (level (/ indent 4)) ;; level = 0, 1, 2, ...
           (bullet (nth (mod level (length haddock-list-item-bullets))
                        haddock-list-item-bullets)))
      (add-text-properties
       (match-beginning 2) (match-end 2) '(face haddock-list-face))
      (when haddock-hide-markup
        (cond
         ;; Unordered lists
         ((string-match-p "[\\*\\+-]" (match-string 2))
          (add-text-properties
           (match-beginning 2) (match-end 2) `(display ,bullet)))
         ;; Definition lists
         ((string-equal ":" (match-string 2))
          (let ((display-string
                 (char-to-string (haddock--first-displayable
                                  haddock-definition-display-char))))
            (add-text-properties (match-beginning 2) (match-end 2)
                                 `(display ,display-string)))))))
    t))

(defun haddock-fontify-hrs (last)
  "Add text properties to horizontal rules from point to LAST."
  (when (haddock-match-hr last)
    (let ((hr-char (haddock--first-displayable haddock-hr-display-char)))
      (add-text-properties
       (match-beginning 0) (match-end 0)
       `(face haddock-hr-face
              font-lock-multiline t
              ,@(when (and haddock-hide-markup hr-char)
                  `(display ,(make-string
                              (window-body-width) hr-char)))))
      t)))

(defun haddock-fontify-sub-superscripts (last)
  "Apply text properties to sub- and superscripts from point to LAST."
  (when (haddock-search-until-condition
         (lambda () (and (not (haddock-code-block-at-point-p))
                         (not (haddock-inline-code-at-point-p))
                         (not (haddock-in-comment-p))))
         haddock-regex-sub-superscript last t)
    (let* ((subscript-p (string= (match-string 2) "~"))
           (props
            (if subscript-p
                (car haddock-sub-superscript-display)
              (cdr haddock-sub-superscript-display)))
           (mp (list 'face 'haddock-markup-face
                     'invisible 'haddock-markup)))
      (when haddock-hide-markup
        (put-text-property (match-beginning 3) (match-end 3)
                           'display props))
      (add-text-properties (match-beginning 2) (match-end 2) mp)
      (add-text-properties (match-beginning 4) (match-end 4) mp)
      t)))


;;; Syntax Table ==============================================================

(defvar haddock-mode-syntax-table
  (let ((tab (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\" "." tab)
    tab)
  "Syntax table for `haddock-mode'.")


;;; Element Insertion =========================================================

(defun haddock-ensure-blank-line-before ()
  "If previous line is not already blank, insert a blank line before point."
  (unless (bolp) (insert "\n"))
  (unless (or (bobp) (looking-back "\n\\s-*\n" nil)) (insert "\n")))

(defun haddock-ensure-blank-line-after ()
  "If following line is not already blank, insert a blank line after point.
Return the point where it was originally."
  (save-excursion
    (unless (eolp) (insert "\n"))
    (unless (or (eobp) (looking-at-p "\n\\s-*\n")) (insert "\n"))))

(defun haddock-wrap-or-insert (s1 s2 &optional thing beg end)
  "Insert the strings S1 and S2, wrapping around region or THING.
If a region is specified by the optional BEG and END arguments,
wrap the strings S1 and S2 around that region.
If there is an active region, wrap the strings S1 and S2 around
the region.  If there is not an active region but the point is at
THING, wrap that thing (which defaults to word).  Otherwise, just
insert S1 and S2 and place the point in between.  Return the
bounds of the entire wrapped string, or nil if nothing was wrapped
and S1 and S2 were only inserted."
  (let (a b bounds new-point)
    (cond
     ;; Given region
     ((and beg end)
      (setq a beg
            b end
            new-point (+ (point) (length s1))))
     ;; Active region
     ((haddock-use-region-p)
      (setq a (region-beginning)
            b (region-end)
            new-point (+ (point) (length s1))))
     ;; Thing (word) at point
     ((setq bounds (haddock-bounds-of-thing-at-point (or thing 'word)))
      (setq a (car bounds)
            b (cdr bounds)
            new-point (+ (point) (length s1))))
     ;; No active region and no word
     (t
      (setq a (point)
            b (point))))
    (goto-char b)
    (insert s2)
    (goto-char a)
    (insert s1)
    (when new-point (goto-char new-point))
    (if (= a b)
        nil
      (setq b (+ b (length s1) (length s2)))
      (cons a b))))

(defun haddock-point-after-unwrap (cur prefix suffix)
  "Return desired position of point after an unwrapping operation.
CUR gives the position of the point before the operation.
Additionally, two cons cells must be provided.  PREFIX gives the
bounds of the prefix string and SUFFIX gives the bounds of the
suffix string."
  (cond ((< cur (cdr prefix)) (car prefix))
        ((< cur (car suffix)) (- cur (- (cdr prefix) (car prefix))))
        ((<= cur (cdr suffix))
         (- cur (+ (- (cdr prefix) (car prefix))
                   (- cur (car suffix)))))
        (t cur)))

(defun haddock-unwrap-thing-at-point (regexp all text)
  "Remove prefix and suffix of thing at point and reposition the point.
When the thing at point matches REGEXP, replace the subexpression
ALL with the string in subexpression TEXT.  Reposition the point
in an appropriate location accounting for the removal of prefix
and suffix strings.  Return new bounds of string from group TEXT.
When REGEXP is nil, assumes match data is already set."
  (when (or (null regexp)
            (thing-at-point-looking-at regexp))
    (let ((cur (point))
          (prefix (cons (match-beginning all) (match-beginning text)))
          (suffix (cons (match-end text) (match-end all)))
          (bounds (cons (match-beginning text) (match-end text))))
      ;; Replace the thing at point
      (replace-match (match-string text) t t nil all)
      ;; Reposition the point
      (goto-char (haddock-point-after-unwrap cur prefix suffix))
      ;; Adjust bounds
      (setq bounds (cons (car prefix)
                         (- (cdr bounds) (- (cdr prefix) (car prefix))))))))

(defun haddock-unwrap-things-in-region (beg end regexp all text)
  "Remove prefix and suffix of all things in region from BEG to END.
When a thing in the region matches REGEXP, replace the
subexpression ALL with the string in subexpression TEXT.
Return a cons cell containing updated bounds for the region."
  (save-excursion
    (goto-char beg)
    (let ((removed 0) len-all len-text)
      (while (re-search-forward regexp (- end removed) t)
        (setq len-all (length (match-string-no-properties all)))
        (setq len-text (length (match-string-no-properties text)))
        (setq removed (+ removed (- len-all len-text)))
        (replace-match (match-string text) t t nil all))
      (cons beg (- end removed)))))

(defun haddock-insert-hr (arg)
  "Insert or replace a horizonal rule.
By default, use the first element of `haddock-hr-strings'.  When
ARG is non-nil, as when given a prefix, select a different
element as follows.  When prefixed with \\[universal-argument],
use the last element of `haddock-hr-strings' instead.  When
prefixed with an integer from 1 to the length of
`haddock-hr-strings', use the element in that position instead."
  (interactive "*P")
  (when (thing-at-point-looking-at haddock-regex-hr)
    (delete-region (match-beginning 0) (match-end 0)))
  (haddock-ensure-blank-line-before)
  (cond ((equal arg '(4))
         (insert (car (reverse haddock-hr-strings))))
        ((and (integerp arg) (> arg 0)
              (<= arg (length haddock-hr-strings)))
         (insert (nth (1- arg) haddock-hr-strings)))
        (t
         (insert (car haddock-hr-strings))))
  (haddock-ensure-blank-line-after))

(defun haddock-insert-bold ()
  "Insert markup to make a region or word bold.
If there is an active region, make the region bold.  If the point
is at a non-bold word, make the word bold.  If the point is at a
bold word or phrase, remove the bold markup.  Otherwise, simply
insert bold delimiters and place the point in between them."
  (interactive)
  (let ((delim (if haddock-bold-underscore "__" "**")))
    (if (haddock-use-region-p)
        ;; Active region
        (let ((bounds (haddock-unwrap-things-in-region
                       (region-beginning) (region-end)
                       haddock-regex-bold 2 4)))
          (haddock-wrap-or-insert delim delim nil (car bounds) (cdr bounds)))
      ;; Bold markup removal, bold word at point, or empty markup insertion
      (if (thing-at-point-looking-at haddock-regex-bold)
          (haddock-unwrap-thing-at-point nil 2 4)
        (haddock-wrap-or-insert delim delim 'word nil nil)))))

(defun haddock-insert-italic ()
  "Insert markup to make a region or word italic.
If there is an active region, make the region italic.  If the point
is at a non-italic word, make the word italic.  If the point is at an
italic word or phrase, remove the italic markup.  Otherwise, simply
insert italic delimiters and place the point in between them."
  (interactive)
  (let ((delim (if haddock-italic-underscore "_" "*")))
    (if (haddock-use-region-p)
        ;; Active region
        (let ((bounds (haddock-unwrap-things-in-region
                       (region-beginning) (region-end)
                       haddock-regex-italic 1 3)))
          (haddock-wrap-or-insert delim delim nil (car bounds) (cdr bounds)))
      ;; Italic markup removal, italic word at point, or empty markup insertion
      (if (thing-at-point-looking-at haddock-regex-italic)
          (haddock-unwrap-thing-at-point nil 1 3)
        (haddock-wrap-or-insert delim delim 'word nil nil)))))

(defun haddock-insert-strike-through ()
  "Insert markup to make a region or word strikethrough.
If there is an active region, make the region strikethrough.  If the point
is at a non-bold word, make the word strikethrough.  If the point is at a
strikethrough word or phrase, remove the strikethrough markup.  Otherwise,
simply insert bold delimiters and place the point in between them."
  (interactive)
  (let ((delim "~~"))
    (if (haddock-use-region-p)
        ;; Active region
        (let ((bounds (haddock-unwrap-things-in-region
                       (region-beginning) (region-end)
                       haddock-regex-strike-through 2 4)))
          (haddock-wrap-or-insert delim delim nil (car bounds) (cdr bounds)))
      ;; Strikethrough markup removal, strikethrough word at point, or empty markup insertion
      (if (thing-at-point-looking-at haddock-regex-strike-through)
          (haddock-unwrap-thing-at-point nil 2 4)
        (haddock-wrap-or-insert delim delim 'word nil nil)))))

(defun haddock-insert-code ()
  "Insert markup to make a region or word an inline code fragment.
If there is an active region, make the region an inline code
fragment.  If the point is at a word, make the word an inline
code fragment.  Otherwise, simply insert code delimiters and
place the point in between them."
  (interactive)
  (if (haddock-use-region-p)
      ;; Active region
      (let ((bounds (haddock-unwrap-things-in-region
                     (region-beginning) (region-end)
                     haddock-regex-code 1 3)))
        (haddock-wrap-or-insert "`" "`" nil (car bounds) (cdr bounds)))
    ;; Code markup removal, code markup for word, or empty markup insertion
    (if (haddock-inline-code-at-point)
        (haddock-unwrap-thing-at-point nil 0 2)
      (haddock-wrap-or-insert "`" "`" 'word nil nil))))

(defun haddock-insert-kbd ()
  "Insert markup to wrap region or word in <kbd> tags.
If there is an active region, use the region.  If the point is at
a word, use the word.  Otherwise, simply insert <kbd> tags and
place the point in between them."
  (interactive)
  (if (haddock-use-region-p)
      ;; Active region
      (let ((bounds (haddock-unwrap-things-in-region
                     (region-beginning) (region-end)
                     haddock-regex-kbd 0 2)))
        (haddock-wrap-or-insert "<kbd>" "</kbd>" nil (car bounds) (cdr bounds)))
    ;; Markup removal, markup for word, or empty markup insertion
    (if (thing-at-point-looking-at haddock-regex-kbd)
        (haddock-unwrap-thing-at-point nil 0 2)
      (haddock-wrap-or-insert "<kbd>" "</kbd>" 'word nil nil))))

(defun haddock-insert-inline-link (text url &optional title)
  "Insert an inline link with TEXT pointing to URL.
Optionally, the user can provide a TITLE."
  (let ((cur (point)))
    (setq title (and title (concat " \"" title "\"")))
    (insert (concat "[" text "](" url title ")"))
    (cond ((not text) (goto-char (+ 1 cur)))
          ((not url) (goto-char (+ 3 (length text) cur))))))

(defun haddock-insert-inline-image (text url &optional title)
  "Insert an inline link with alt TEXT pointing to URL.
Optionally, also provide a TITLE."
  (let ((cur (point)))
    (setq title (and title (concat " \"" title "\"")))
    (insert (concat "![" text "](" url title ")"))
    (cond ((not text) (goto-char (+ 2 cur)))
          ((not url) (goto-char (+ 4 (length text) cur))))))

(defun haddock-insert-reference-link (text label &optional url title)
  "Insert a reference link and, optionally, a reference definition.
The link TEXT will be inserted followed by the optional LABEL.
If a URL is given, also insert a definition for the reference
LABEL according to `haddock-reference-location'.  If a TITLE is
given, it will be added to the end of the reference definition
and will be used to populate the title attribute when converted
to XHTML.  If URL is nil, insert only the link portion (for
example, when a reference label is already defined)."
  (insert (concat "[" text "][" label "]"))
  (when url
    (haddock-insert-reference-definition
     (if (string-equal label "") text label)
     url title)))

(defun haddock-insert-reference-image (text label &optional url title)
  "Insert a reference image and, optionally, a reference definition.
The alt TEXT will be inserted followed by the optional LABEL.
If a URL is given, also insert a definition for the reference
LABEL according to `haddock-reference-location'.  If a TITLE is
given, it will be added to the end of the reference definition
and will be used to populate the title attribute when converted
to XHTML.  If URL is nil, insert only the link portion (for
example, when a reference label is already defined)."
  (insert (concat "![" text "][" label "]"))
  (when url
    (haddock-insert-reference-definition
     (if (string-equal label "") text label)
     url title)))

(defun haddock-insert-reference-definition (label &optional url title)
  "Add definition for reference LABEL with URL and TITLE.
LABEL is a Haddock reference label without square brackets.
URL and TITLE are optional.  When given, the TITLE will
be used to populate the title attribute when converted to XHTML."
  ;; END specifies where to leave the point upon return
  (let ((end (point)))
    (cl-case haddock-reference-location
      (end         (goto-char (point-max)))
      (immediately (haddock-end-of-text-block))
      (subtree     (haddock-end-of-subtree))
      (header      (haddock-end-of-defun)))
    ;; Skip backwards over local variables.  This logic is similar to the one
    ;; used in ‘hack-local-variables’.
    (when (and enable-local-variables (eobp))
      (search-backward "\n\f" (max (- (point) 3000) (point-min)) :move)
      (when (let ((case-fold-search t))
              (search-forward "Local Variables:" nil :move))
        (beginning-of-line 0)
        (when (eq (char-before) ?\n) (backward-char))))
    (unless (or (haddock-cur-line-blank-p)
                (thing-at-point-looking-at haddock-regex-reference-definition))
      (insert "\n"))
    (insert "\n[" label "]: ")
    (if url
        (insert url)
      ;; When no URL is given, leave point at END following the colon
      (setq end (point)))
    (when (> (length title) 0)
      (insert " \"" title "\""))
    (unless (looking-at-p "\n")
      (insert "\n"))
    (goto-char end)
    (when url
      (message
       (haddock--substitute-command-keys
        "Reference [%s] was defined, press \\[haddock-do] to jump there")
       label))))

(define-obsolete-function-alias
  'haddock-insert-inline-link-dwim 'haddock-insert-link "v2.3")
(define-obsolete-function-alias
  'haddock-insert-reference-link-dwim 'haddock-insert-link "v2.3")

(defun haddock--insert-link-or-image (image)
  "Interactively insert new or update an existing link or image.
When IMAGE is non-nil, insert an image.  Otherwise, insert a link.
This is an internal function called by
`haddock-insert-link' and `haddock-insert-image'."
  (cl-multiple-value-bind (begin end text uri ref title)
      (if (haddock-use-region-p)
          ;; Use region as either link text or URL as appropriate.
          (let ((region (buffer-substring-no-properties
                         (region-beginning) (region-end))))
            (if (string-match haddock-regex-uri region)
                ;; Region contains a URL; use it as such.
                (list (region-beginning) (region-end)
                      nil (match-string 0 region) nil nil)
              ;; Region doesn't contain a URL, so use it as text.
              (list (region-beginning) (region-end)
                    region nil nil nil)))
        ;; Extract and use properties of existing link, if any.
        (haddock-link-at-pos (point)))
    (let* ((ref (when ref (concat "[" ref "]")))
           (defined-refs (append
                          (mapcar (lambda (ref) (concat "[" ref "]"))
                                  (mapcar #'car (haddock-get-defined-references)))))
           (used-uris (haddock-get-used-uris))
           (uri-or-ref (completing-read
                        "URL or [reference]: "
                        (append defined-refs used-uris)
                        nil nil (or uri ref)))
           (ref (cond ((string-match "\\`\\[\\(.*\\)\\]\\'" uri-or-ref)
                       (match-string 1 uri-or-ref))
                      ((string-equal "" uri-or-ref)
                       "")))
           (uri (unless ref uri-or-ref))
           (text-prompt (if image
                            "Alt text: "
                          (if ref
                              "Link text: "
                            "Link text (blank for plain URL): ")))
           (text (read-string text-prompt text))
           (text (if (= (length text) 0) nil text))
           (plainp (and uri (not text)))
           (implicitp (string-equal ref ""))
           (ref (if implicitp text ref))
           (definedp (and ref (haddock-reference-definition ref)))
           (ref-url (unless (or uri definedp)
                      (completing-read "Reference URL: " used-uris)))
           (title (unless (or plainp definedp)
                    (read-string "Title (tooltip text, optional): " title)))
           (title (if (= (length title) 0) nil title)))
      (when (and image implicitp)
        (user-error "Reference required: implicit image references are invalid"))
      (when (and begin end)
        (delete-region begin end))
      (cond
       ((and (not image) uri text)
        (haddock-insert-inline-link text uri title))
       ((and image uri text)
        (haddock-insert-inline-image text uri title))
       ((and ref text)
        (if image
            (haddock-insert-reference-image text (unless implicitp ref) nil title)
          (haddock-insert-reference-link text (unless implicitp ref) nil title))
        (unless definedp
          (haddock-insert-reference-definition ref ref-url title)))
       ((and (not image) uri)
        (haddock-insert-uri uri))))))

(defun haddock-insert-link ()
  "Insert new or update an existing link, with interactive prompts.
If the point is at an existing link or URL, update the link text,
URL, reference label, and/or title.  Otherwise, insert a new link.
The type of link inserted (inline, reference, or plain URL)
depends on which values are provided:

*   If a URL and TEXT are given, insert an inline link: [TEXT](URL).
*   If [REF] and TEXT are given, insert a reference link: [TEXT][REF].
*   If only TEXT is given, insert an implicit reference link: [TEXT][].
*   If only a URL is given, insert a plain link: <URL>.

In other words, to create an implicit reference link, leave the
URL prompt empty and to create a plain URL link, leave the link
text empty.

If there is an active region, use the text as the default URL, if
it seems to be a URL, or link text value otherwise.

If a given reference is not defined, this function will
additionally prompt for the URL and optional title.  In this case,
the reference definition is placed at the location determined by
`haddock-reference-location'.

Through updating the link, this function can be used to convert a
link of one type (inline, reference, or plain) to another type by
selectively adding or removing information via the prompts."
  (interactive)
  (haddock--insert-link-or-image nil))

(defun haddock-insert-image ()
  "Insert new or update an existing image, with interactive prompts.
If the point is at an existing image, update the alt text, URL,
reference label, and/or title. Otherwise, insert a new image.
The type of image inserted (inline or reference) depends on which
values are provided:

*   If a URL and ALT-TEXT are given, insert an inline image:
    ![ALT-TEXT](URL).
*   If [REF] and ALT-TEXT are given, insert a reference image:
    ![ALT-TEXT][REF].

If there is an active region, use the text as the default URL, if
it seems to be a URL, or alt text value otherwise.

If a given reference is not defined, this function will
additionally prompt for the URL and optional title.  In this case,
the reference definition is placed at the location determined by
`haddock-reference-location'.

Through updating the image, this function can be used to convert an
image of one type (inline or reference) to another type by
selectively adding or removing information via the prompts."
  (interactive)
  (haddock--insert-link-or-image t))

(defun haddock-insert-uri (&optional uri)
  "Insert markup for an inline URI.
If there is an active region, use it as the URI.  If the point is
at a URI, wrap it with angle brackets.  If the point is at an
inline URI, remove the angle brackets.  Otherwise, simply insert
angle brackets place the point between them."
  (interactive)
  (if (haddock-use-region-p)
      ;; Active region
      (let ((bounds (haddock-unwrap-things-in-region
                     (region-beginning) (region-end)
                     haddock-regex-angle-uri 0 2)))
        (haddock-wrap-or-insert "<" ">" nil (car bounds) (cdr bounds)))
    ;; Markup removal, URI at point, new URI, or empty markup insertion
    (if (thing-at-point-looking-at haddock-regex-angle-uri)
        (haddock-unwrap-thing-at-point nil 0 2)
      (if uri
          (insert "<" uri ">")
        (haddock-wrap-or-insert "<" ">" 'url nil nil)))))

(defun haddock-insert-wiki-link ()
  "Insert a wiki link of the form [[WikiLink]].
If there is an active region, use the region as the link text.
If the point is at a word, use the word as the link text.  If
there is no active region and the point is not at word, simply
insert link markup."
  (interactive)
  (if (haddock-use-region-p)
      ;; Active region
      (haddock-wrap-or-insert "[[" "]]" nil (region-beginning) (region-end))
    ;; Markup removal, wiki link at at point, or empty markup insertion
    (if (thing-at-point-looking-at haddock-regex-wiki-link)
        (if (or haddock-wiki-link-alias-first
                (null (match-string 5)))
            (haddock-unwrap-thing-at-point nil 1 3)
          (haddock-unwrap-thing-at-point nil 1 5))
      (haddock-wrap-or-insert "[[" "]]"))))

(defun haddock-remove-header ()
  "Remove header markup if point is at a header.
Return bounds of remaining header text if a header was removed
and nil otherwise."
  (interactive "*")
  (or (haddock-unwrap-thing-at-point haddock-regex-header-atx 0 2)
      (haddock-unwrap-thing-at-point haddock-regex-header-setext 0 1)))

(defun haddock-insert-header (&optional level text setext)
  "Insert or replace header markup.
The level of the header is specified by LEVEL and header text is
given by TEXT.  LEVEL must be an integer from 1 and 6, and the
default value is 1.
When TEXT is nil, the header text is obtained as follows.
If there is an active region, it is used as the header text.
Otherwise, the current line will be used as the header text.
If there is not an active region and the point is at a header,
remove the header markup and replace with level N header.
Otherwise, insert empty header markup and place the point in
between.
The style of the header will be atx (hash marks) unless
SETEXT is non-nil, in which case a setext-style (underlined)
header will be inserted."
  (interactive "p\nsHeader text: ")
  (setq level (min (max (or level 1) 1) (if setext 2 6)))
  ;; Determine header text if not given
  (when (null text)
    (if (haddock-use-region-p)
        ;; Active region
        (setq text (delete-and-extract-region (region-beginning) (region-end)))
      ;; No active region
      (haddock-remove-header)
      (setq text (delete-and-extract-region
                  (line-beginning-position) (line-end-position)))
      (when (and setext (string-match-p "^[ \t]*$" text))
        (setq text (read-string "Header text: "))))
    (setq text (haddock-compress-whitespace-string text)))
  ;; Insertion with given text
  (haddock-ensure-blank-line-before)
  (let (hdr)
    (cond (setext
           (setq hdr (make-string (string-width text) (if (= level 2) ?- ?=)))
           (insert text "\n" hdr))
          (t
           (setq hdr (make-string level ?#))
           (insert hdr " " text)
           (when (null haddock-asymmetric-header) (insert " " hdr)))))
  (haddock-ensure-blank-line-after)
  ;; Leave point at end of text
  (cond (setext
         (backward-char (1+ (string-width text))))
        ((null haddock-asymmetric-header)
         (backward-char (1+ level)))))

(defun haddock-insert-header-dwim (&optional arg setext)
  "Insert or replace header markup.
The level and type of the header are determined automatically by
the type and level of the previous header, unless a prefix
argument is given via ARG.
With a numeric prefix valued 1 to 6, insert a header of the given
level, with the type being determined automatically (note that
only level 1 or 2 setext headers are possible).

With a \\[universal-argument] prefix (i.e., when ARG is (4)),
promote the heading by one level.
With two \\[universal-argument] prefixes (i.e., when ARG is (16)),
demote the heading by one level.
When SETEXT is non-nil, prefer setext-style headers when
possible (levels one and two).

When there is an active region, use it for the header text.  When
the point is at an existing header, change the type and level
according to the rules above.
Otherwise, if the line is not empty, create a header using the
text on the current line as the header text.
Finally, if the point is on a blank line, insert empty header
markup (atx) or prompt for text (setext).
See `haddock-insert-header' for more details about how the
header text is determined."
  (interactive "*P")
  (let (level)
    (save-excursion
      (when (or (thing-at-point-looking-at haddock-regex-header)
                (re-search-backward haddock-regex-header nil t))
        ;; level of current or previous header
        (setq level (haddock-outline-level))
        ;; match group 1 indicates a setext header
        (setq setext (match-end 1))))
    ;; check prefix argument
    (cond
     ((and (equal arg '(4)) level (> level 1)) ;; C-u
      (cl-decf level))
     ((and (equal arg '(16)) level (< level 6)) ;; C-u C-u
      (cl-incf level))
     (arg ;; numeric prefix
      (setq level (prefix-numeric-value arg))))
    ;; setext headers must be level one or two
    (and level (setq setext (and setext (<= level 2))))
    ;; insert the heading
    (haddock-insert-header level nil setext)))

(defun haddock-insert-header-setext-dwim (&optional arg)
  "Insert or replace header markup, with preference for setext.
See `haddock-insert-header-dwim' for details, including how ARG is handled."
  (interactive "*P")
  (haddock-insert-header-dwim arg t))

(defun haddock-insert-header-atx-1 ()
  "Insert a first level atx-style (hash mark) header.
See `haddock-insert-header'."
  (interactive "*")
  (haddock-insert-header 1 nil nil))

(defun haddock-insert-header-atx-2 ()
  "Insert a level two atx-style (hash mark) header.
See `haddock-insert-header'."
  (interactive "*")
  (haddock-insert-header 2 nil nil))

(defun haddock-insert-header-atx-3 ()
  "Insert a level three atx-style (hash mark) header.
See `haddock-insert-header'."
  (interactive "*")
  (haddock-insert-header 3 nil nil))

(defun haddock-insert-header-atx-4 ()
  "Insert a level four atx-style (hash mark) header.
See `haddock-insert-header'."
  (interactive "*")
  (haddock-insert-header 4 nil nil))

(defun haddock-insert-header-atx-5 ()
  "Insert a level five atx-style (hash mark) header.
See `haddock-insert-header'."
  (interactive "*")
  (haddock-insert-header 5 nil nil))

(defun haddock-insert-header-atx-6 ()
  "Insert a sixth level atx-style (hash mark) header.
See `haddock-insert-header'."
  (interactive "*")
  (haddock-insert-header 6 nil nil))

(defun haddock-insert-header-setext-1 ()
  "Insert a setext-style (underlined) first-level header.
See `haddock-insert-header'."
  (interactive "*")
  (haddock-insert-header 1 nil t))

(defun haddock-insert-header-setext-2 ()
  "Insert a setext-style (underlined) second-level header.
See `haddock-insert-header'."
  (interactive "*")
  (haddock-insert-header 2 nil t))

(defun haddock-blockquote-indentation (loc)
  "Return string containing necessary indentation for a blockquote at LOC.
Also see `haddock-pre-indentation'."
  (save-excursion
    (goto-char loc)
    (let* ((list-level (length (haddock-calculate-list-levels)))
           (indent ""))
      (dotimes (_ list-level indent)
        (setq indent (concat indent "    "))))))

(defun haddock-insert-blockquote ()
  "Start a blockquote section (or blockquote the region).
If Transient Mark mode is on and a region is active, it is used as
the blockquote text."
  (interactive)
  (if (haddock-use-region-p)
      (haddock-blockquote-region (region-beginning) (region-end))
    (haddock-ensure-blank-line-before)
    (insert (haddock-blockquote-indentation (point)) "> ")
    (haddock-ensure-blank-line-after)))

(defun haddock-block-region (beg end prefix)
  "Format the region using a block prefix.
Arguments BEG and END specify the beginning and end of the
region.  The characters PREFIX will appear at the beginning
of each line."
  (save-excursion
    (let* ((end-marker (make-marker))
           (beg-marker (make-marker))
           (prefix-without-trailing-whitespace
            (replace-regexp-in-string (rx (+ blank) eos) "" prefix)))
      ;; Ensure blank line after and remove extra whitespace
      (goto-char end)
      (skip-syntax-backward "-")
      (set-marker end-marker (point))
      (delete-horizontal-space)
      (haddock-ensure-blank-line-after)
      ;; Ensure blank line before and remove extra whitespace
      (goto-char beg)
      (skip-syntax-forward "-")
      (delete-horizontal-space)
      (haddock-ensure-blank-line-before)
      (set-marker beg-marker (point))
      ;; Insert PREFIX before each line
      (goto-char beg-marker)
      (while (and (< (line-beginning-position) end-marker)
                  (not (eobp)))
        ;; Don’t insert trailing whitespace.
        (insert (if (eolp) prefix-without-trailing-whitespace prefix))
        (forward-line)))))

(defun haddock-blockquote-region (beg end)
  "Blockquote the region.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (haddock-block-region
   beg end (concat (haddock-blockquote-indentation
                    (max (point-min) (1- beg))) "> ")))

(defun haddock-pre-indentation (loc)
  "Return string containing necessary whitespace for a pre block at LOC.
Also see `haddock-blockquote-indentation'."
  (save-excursion
    (goto-char loc)
    (let* ((list-level (length (haddock-calculate-list-levels)))
           indent)
      (dotimes (_ (1+ list-level) indent)
        (setq indent (concat indent "    "))))))

(defun haddock-insert-pre ()
  "Start a preformatted section (or apply to the region).
If Transient Mark mode is on and a region is active, it is marked
as preformatted text."
  (interactive)
  (if (haddock-use-region-p)
      (haddock-pre-region (region-beginning) (region-end))
    (haddock-ensure-blank-line-before)
    (insert (haddock-pre-indentation (point)))
    (haddock-ensure-blank-line-after)))

(defun haddock-pre-region (beg end)
  "Format the region as preformatted text.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (let ((indent (haddock-pre-indentation (max (point-min) (1- beg)))))
    (haddock-block-region beg end indent)))

(defun haddock-electric-backquote (arg)
  "Insert a backquote.
The numeric prefix argument ARG says how many times to repeat the insertion.
Call `haddock-insert-gfm-code-block' interactively
if three backquotes inserted at the beginning of line."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (when (and haddock-gfm-use-electric-backquote (looking-back "^```" nil))
    (replace-match "")
    (call-interactively #'haddock-insert-gfm-code-block)))

(defconst haddock-gfm-recognized-languages
  ;; To reproduce/update, evaluate the let-form in
  ;; scripts/get-recognized-gfm-languages.el. that produces a single long sexp,
  ;; but with appropriate use of a keyboard macro, indenting and filling it
  ;; properly is pretty fast.
  '("1C-Enterprise" "ABAP" "ABNF" "AGS-Script" "AMPL" "ANTLR"
    "API-Blueprint" "APL" "ASN.1" "ASP" "ATS" "ActionScript" "Ada" "Agda"
    "Alloy" "Alpine-Abuild" "Ant-Build-System" "ApacheConf" "Apex"
    "Apollo-Guidance-Computer" "AppleScript" "Arc" "Arduino" "AsciiDoc"
    "AspectJ" "Assembly" "Augeas" "AutoHotkey" "AutoIt" "Awk" "Batchfile"
    "Befunge" "Bison" "BitBake" "Blade" "BlitzBasic" "BlitzMax" "Bluespec"
    "Boo" "Brainfuck" "Brightscript" "Bro" "C#" "C++" "C-ObjDump"
    "C2hs-Haskell" "CLIPS" "CMake" "COBOL" "COLLADA" "CSON" "CSS" "CSV"
    "CWeb" "Cap'n-Proto" "CartoCSS" "Ceylon" "Chapel" "Charity" "ChucK"
    "Cirru" "Clarion" "Clean" "Click" "Clojure" "Closure-Templates"
    "CoffeeScript" "ColdFusion" "ColdFusion-CFC" "Common-Lisp"
    "Component-Pascal" "Cool" "Coq" "Cpp-ObjDump" "Creole" "Crystal"
    "Csound" "Csound-Document" "Csound-Score" "Cuda" "Cycript" "Cython"
    "D-ObjDump" "DIGITAL-Command-Language" "DM" "DNS-Zone" "DTrace"
    "Darcs-Patch" "Dart" "Diff" "Dockerfile" "Dogescript" "Dylan" "EBNF"
    "ECL" "ECLiPSe" "EJS" "EQ" "Eagle" "Ecere-Projects" "Eiffel" "Elixir"
    "Elm" "Emacs-Lisp" "EmberScript" "Erlang" "F#" "FLUX" "Factor" "Fancy"
    "Fantom" "Filebench-WML" "Filterscript" "Formatted" "Forth" "Fortran"
    "FreeMarker" "Frege" "G-code" "GAMS" "GAP" "GCC-Machine-Description"
    "GDB" "GDScript" "GLSL" "GN" "Game-Maker-Language" "Genie" "Genshi"
    "Gentoo-Ebuild" "Gentoo-Eclass" "Gettext-Catalog" "Gherkin" "Glyph"
    "Gnuplot" "Go" "Golo" "Gosu" "Grace" "Gradle" "Grammatical-Framework"
    "Graph-Modeling-Language" "GraphQL" "Graphviz-(DOT)" "Groovy"
    "Groovy-Server-Pages" "HCL" "HLSL" "HTML" "HTML+Django" "HTML+ECR"
    "HTML+EEX" "HTML+ERB" "HTML+PHP" "HTTP" "Hack" "Haml" "Handlebars"
    "Harbour" "Haskell" "Haxe" "Hy" "HyPhy" "IDL" "IGOR-Pro" "INI"
    "IRC-log" "Idris" "Inform-7" "Inno-Setup" "Io" "Ioke" "Isabelle"
    "Isabelle-ROOT" "JFlex" "JSON" "JSON5" "JSONLD" "JSONiq" "JSX"
    "Jasmin" "Java" "Java-Server-Pages" "JavaScript" "Jison" "Jison-Lex"
    "Jolie" "Julia" "Jupyter-Notebook" "KRL" "KiCad" "Kit" "Kotlin" "LFE"
    "LLVM" "LOLCODE" "LSL" "LabVIEW" "Lasso" "Latte" "Lean" "Less" "Lex"
    "LilyPond" "Limbo" "Linker-Script" "Linux-Kernel-Module" "Liquid"
    "Literate-Agda" "Literate-CoffeeScript" "Literate-Haskell"
    "LiveScript" "Logos" "Logtalk" "LookML" "LoomScript" "Lua" "M4"
    "M4Sugar" "MAXScript" "MQL4" "MQL5" "MTML" "MUF" "Makefile" "Mako"
    "Haddock" "Marko" "Mask" "Mathematica" "Matlab" "Maven-POM" "Max"
    "MediaWiki" "Mercury" "Meson" "Metal" "MiniD" "Mirah" "Modelica"
    "Modula-2" "Module-Management-System" "Monkey" "Moocode" "MoonScript"
    "Myghty" "NCL" "NL" "NSIS" "Nemerle" "NetLinx" "NetLinx+ERB" "NetLogo"
    "NewLisp" "Nginx" "Nim" "Ninja" "Nit" "Nix" "Nu" "NumPy" "OCaml"
    "ObjDump" "Objective-C" "Objective-C++" "Objective-J" "Omgrofl" "Opa"
    "Opal" "OpenCL" "OpenEdge-ABL" "OpenRC-runscript" "OpenSCAD"
    "OpenType-Feature-File" "Org" "Ox" "Oxygene" "Oz" "P4" "PAWN" "PHP"
    "PLSQL" "PLpgSQL" "POV-Ray-SDL" "Pan" "Papyrus" "Parrot"
    "Parrot-Assembly" "Parrot-Internal-Representation" "Pascal" "Pep8"
    "Perl" "Perl6" "Pic" "Pickle" "PicoLisp" "PigLatin" "Pike" "Pod"
    "PogoScript" "Pony" "PostScript" "PowerBuilder" "PowerShell"
    "Processing" "Prolog" "Propeller-Spin" "Protocol-Buffer" "Public-Key"
    "Pug" "Puppet" "Pure-Data" "PureBasic" "PureScript" "Python"
    "Python-console" "Python-traceback" "QML" "QMake" "RAML" "RDoc"
    "REALbasic" "REXX" "RHTML" "RHaddock" "RPM-Spec" "RUNOFF" "Racket"
    "Ragel" "Rascal" "Raw-token-data" "Reason" "Rebol" "Red" "Redcode"
    "Regular-Expression" "Ren'Py" "RenderScript" "RobotFramework" "Roff"
    "Rouge" "Ruby" "Rust" "SAS" "SCSS" "SMT" "SPARQL" "SQF" "SQL" "SQLPL"
    "SRecode-Template" "STON" "SVG" "Sage" "SaltStack" "Sass" "Scala"
    "Scaml" "Scheme" "Scilab" "Self" "ShaderLab" "Shell" "ShellSession"
    "Shen" "Slash" "Slim" "Smali" "Smalltalk" "Smarty" "SourcePawn"
    "Spline-Font-Database" "Squirrel" "Stan" "Standard-ML" "Stata"
    "Stylus" "SubRip-Text" "Sublime-Text-Config" "SuperCollider" "Swift"
    "SystemVerilog" "TI-Program" "TLA" "TOML" "TXL" "Tcl" "Tcsh" "TeX"
    "Tea" "Terra" "Text" "Textile" "Thrift" "Turing" "Turtle" "Twig"
    "Type-Language" "TypeScript" "Unified-Parallel-C" "Unity3D-Asset"
    "Unix-Assembly" "Uno" "UnrealScript" "UrWeb" "VCL" "VHDL" "Vala"
    "Verilog" "Vim-script" "Visual-Basic" "Volt" "Vue"
    "Wavefront-Material" "Wavefront-Object" "Web-Ontology-Language"
    "WebAssembly" "WebIDL" "World-of-Warcraft-Addon-Data" "X10" "XC"
    "XCompose" "XML" "XPages" "XProc" "XQuery" "XS" "XSLT" "Xojo" "Xtend"
    "YAML" "YANG" "Yacc" "Zephir" "Zimpl" "desktop" "eC" "edn" "fish"
    "mupad" "nesC" "ooc" "reStructuredText" "wisp" "xBase")
  "Language specifiers recognized by GitHub's syntax highlighting features.")

(defvar haddock-gfm-used-languages nil
  "Language names used in GFM code blocks.")
(make-variable-buffer-local 'haddock-gfm-used-languages)

(defun haddock-trim-whitespace (str)
  (haddock-replace-regexp-in-string
   "\\(?:[[:space:]\r\n]+\\'\\|\\`[[:space:]\r\n]+\\)" "" str))

(defun haddock-clean-language-string (str)
  (haddock-replace-regexp-in-string
   "{\\.?\\|}" "" (haddock-trim-whitespace str)))

(defun haddock-validate-language-string (widget)
  (let ((str (widget-value widget)))
    (unless (string= str (haddock-clean-language-string str))
      (widget-put widget :error (format "Invalid language spec: '%s'" str))
      widget)))

(defun haddock-gfm-get-corpus ()
  "Create corpus of recognized GFM code block languages for the given buffer."
  (let ((given-corpus (append haddock-gfm-additional-languages
                              haddock-gfm-recognized-languages)))
    (append
     haddock-gfm-used-languages
     (if haddock-gfm-downcase-languages (cl-mapcar #'downcase given-corpus)
       given-corpus))))

(defun haddock-gfm-add-used-language (lang)
  "Clean LANG and add to list of used languages."
  (setq haddock-gfm-used-languages
          (cons lang (remove lang haddock-gfm-used-languages))))

(defcustom haddock-spaces-after-code-fence 1
  "Number of space characters to insert after a code fence.
\\<gfm-mode-map>\\[haddock-insert-gfm-code-block] inserts this many spaces between an
opening code fence and an info string."
  :group 'haddock
  :type 'integer
  :safe #'natnump
  :package-version '(haddock-mode . "2.3"))

(defun haddock-insert-gfm-code-block (&optional lang edit)
  "Insert GFM code block for language LANG.
If LANG is nil, the language will be queried from user.  If a
region is active, wrap this region with the markup instead.  If
the region boundaries are not on empty lines, these are added
automatically in order to have the correct markup.  When EDIT is
non-nil (e.g., when \\[universal-argument] is given), edit the
code block in an indirect buffer after insertion."
  (interactive
   (list (let ((completion-ignore-case nil))
           (condition-case nil
               (haddock-clean-language-string
                (completing-read
                 "Programming language: "
                 (haddock-gfm-get-corpus)
                 nil 'confirm (car haddock-gfm-used-languages)
                 'haddock-gfm-language-history))
             (quit "")))
         current-prefix-arg))
  (unless (string= lang "") (haddock-gfm-add-used-language lang))
  (when (> (length lang) 0)
    (setq lang (concat (make-string haddock-spaces-after-code-fence ?\s)
                       lang)))
  (if (haddock-use-region-p)
      (let* ((b (region-beginning)) (e (region-end)) end
             (indent (progn (goto-char b) (current-indentation))))
        (goto-char e)
        ;; if we're on a blank line, don't newline, otherwise the ```
        ;; should go on its own line
        (unless (looking-back "\n" nil)
          (newline))
        (indent-to indent)
        (insert "```")
        (haddock-ensure-blank-line-after)
        (setq end (point))
        (goto-char b)
        ;; if we're on a blank line, insert the quotes here, otherwise
        ;; add a new line first
        (unless (looking-at-p "\n")
          (newline)
          (forward-line -1))
        (haddock-ensure-blank-line-before)
        (indent-to indent)
        (insert "```" lang)
        (haddock-syntax-propertize-fenced-block-constructs (point-at-bol) end))
    (let ((indent (current-indentation)) start-bol)
      (delete-horizontal-space :backward-only)
      (haddock-ensure-blank-line-before)
      (indent-to indent)
      (setq start-bol (point-at-bol))
      (insert "```" lang "\n")
      (indent-to indent)
      (unless edit (insert ?\n))
      (indent-to indent)
      (insert "```")
      (haddock-ensure-blank-line-after)
      (haddock-syntax-propertize-fenced-block-constructs start-bol (point)))
    (end-of-line 0)
    (when edit (haddock-edit-code-block))))

(defun haddock-code-block-lang (&optional pos-prop)
  "Return the language name for a GFM or tilde fenced code block.
The beginning of the block may be described by POS-PROP,
a cons of (pos . prop) giving the position and property
at the beginning of the block."
  (or pos-prop
      (setq pos-prop
            (haddock-max-of-seq
             #'car
             (cl-remove-if
              #'null
              (cl-mapcar
               #'haddock-find-previous-prop
               (haddock-get-fenced-block-begin-properties))))))
  (when pos-prop
    (goto-char (car pos-prop))
    (set-match-data (get-text-property (point) (cdr pos-prop)))
    ;; Note: Hard-coded group number assumes tilde
    ;; and GFM fenced code regexp groups agree.
    (let ((begin (match-beginning 3))
          (end (match-end 3)))
      (when (and begin end)
        ;; Fix language strings beginning with periods, like ".ruby".
        (when (eq (char-after begin) ?.)
          (setq begin (1+ begin)))
        (buffer-substring-no-properties begin end)))))

(defun haddock-gfm-parse-buffer-for-languages (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (cl-loop
       with prop = 'haddock-gfm-block-begin
       for pos-prop = (haddock-find-next-prop prop)
       while pos-prop
       for lang = (haddock-code-block-lang pos-prop)
       do (progn (when lang (haddock-gfm-add-used-language lang))
                 (goto-char (next-single-property-change (point) prop)))))))


;;; Footnotes =================================================================

(defun haddock-footnote-counter-inc ()
  "Increment `haddock-footnote-counter' and return the new value."
  (when (= haddock-footnote-counter 0) ; hasn't been updated in this buffer yet.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "^\\[\\^\\(" haddock-footnote-chars "*?\\)\\]:")
                                (point-max) t)
        (let ((fn (string-to-number (match-string 1))))
          (when (> fn haddock-footnote-counter)
            (setq haddock-footnote-counter fn))))))
  (cl-incf haddock-footnote-counter))

(defun haddock-insert-footnote ()
  "Insert footnote with a new number and move point to footnote definition."
  (interactive)
  (let ((fn (haddock-footnote-counter-inc)))
    (insert (format "[^%d]" fn))
    (haddock-footnote-text-find-new-location)
    (haddock-ensure-blank-line-before)
    (unless (haddock-cur-line-blank-p)
      (insert "\n"))
    (insert (format "[^%d]: " fn))
    (haddock-ensure-blank-line-after)))

(defun haddock-footnote-text-find-new-location ()
  "Position the point at the proper location for a new footnote text."
  (cond
   ((eq haddock-footnote-location 'end) (goto-char (point-max)))
   ((eq haddock-footnote-location 'immediately) (haddock-end-of-text-block))
   ((eq haddock-footnote-location 'subtree) (haddock-end-of-subtree))
   ((eq haddock-footnote-location 'header) (haddock-end-of-defun))))

(defun haddock-footnote-kill ()
  "Kill the footnote at point.
The footnote text is killed (and added to the kill ring), the
footnote marker is deleted.  Point has to be either at the
footnote marker or in the footnote text."
  (interactive)
  (let ((marker-pos nil)
        (skip-deleting-marker nil)
        (starting-footnote-text-positions
         (haddock-footnote-text-positions)))
    (when starting-footnote-text-positions
      ;; We're starting in footnote text, so mark our return position and jump
      ;; to the marker if possible.
      (let ((marker-pos (haddock-footnote-find-marker
                         (cl-first starting-footnote-text-positions))))
        (if marker-pos
            (goto-char (1- marker-pos))
          ;; If there isn't a marker, we still want to kill the text.
          (setq skip-deleting-marker t))))
    ;; Either we didn't start in the text, or we started in the text and jumped
    ;; to the marker. We want to assume we're at the marker now and error if
    ;; we're not.
    (unless skip-deleting-marker
      (let ((marker (haddock-footnote-delete-marker)))
        (unless marker
          (error "Not at a footnote"))
        ;; Even if we knew the text position before, it changed when we deleted
        ;; the label.
        (setq marker-pos (cl-second marker))
        (let ((new-text-pos (haddock-footnote-find-text (cl-first marker))))
          (unless new-text-pos
            (error "No text for footnote `%s'" (cl-first marker)))
          (goto-char new-text-pos))))
    (let ((pos (haddock-footnote-kill-text)))
      (goto-char (if starting-footnote-text-positions
                     pos
                   marker-pos)))))

(defun haddock-footnote-delete-marker ()
  "Delete a footnote marker at point.
Returns a list (ID START) containing the footnote ID and the
start position of the marker before deletion.  If no footnote
marker was deleted, this function returns NIL."
  (let ((marker (haddock-footnote-marker-positions)))
    (when marker
      (delete-region (cl-second marker) (cl-third marker))
      (butlast marker))))

(defun haddock-footnote-kill-text ()
  "Kill footnote text at point.
Returns the start position of the footnote text before deletion,
or NIL if point was not inside a footnote text.

The killed text is placed in the kill ring (without the footnote
number)."
  (let ((fn (haddock-footnote-text-positions)))
    (when fn
      (let ((text (delete-and-extract-region (cl-second fn) (cl-third fn))))
        (string-match (concat "\\[\\" (cl-first fn) "\\]:[[:space:]]*\\(\\(.*\n?\\)*\\)") text)
        (kill-new (match-string 1 text))
        (when (and (haddock-cur-line-blank-p)
                   (haddock-prev-line-blank-p)
                   (not (bobp)))
          (delete-region (1- (point)) (point)))
        (cl-second fn)))))

(defun haddock-footnote-goto-text ()
  "Jump to the text of the footnote at point."
  (interactive)
  (let ((fn (car (haddock-footnote-marker-positions))))
    (unless fn
      (user-error "Not at a footnote marker"))
    (let ((new-pos (haddock-footnote-find-text fn)))
      (unless new-pos
        (error "No definition found for footnote `%s'" fn))
      (goto-char new-pos))))

(defun haddock-footnote-return ()
  "Return from a footnote to its footnote number in the main text."
  (interactive)
  (let ((fn (save-excursion
              (car (haddock-footnote-text-positions)))))
    (unless fn
      (user-error "Not in a footnote"))
    (let ((new-pos (haddock-footnote-find-marker fn)))
      (unless new-pos
        (error "Footnote marker `%s' not found" fn))
      (goto-char new-pos))))

(defun haddock-footnote-find-marker (id)
  "Find the location of the footnote marker with ID.
The actual buffer position returned is the position directly
following the marker's closing bracket.  If no marker is found,
NIL is returned."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "\\[" id "\\]\\([^:]\\|\\'\\)") nil t)
      (skip-chars-backward "^]")
      (point))))

(defun haddock-footnote-find-text (id)
  "Find the location of the text of footnote ID.
The actual buffer position returned is the position of the first
character of the text, after the footnote's identifier.  If no
footnote text is found, NIL is returned."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^ \\{0,3\\}\\[" id "\\]:") nil t)
      (skip-chars-forward "[ \t]")
      (point))))

(defun haddock-footnote-marker-positions ()
  "Return the position and ID of the footnote marker point is on.
The return value is a list (ID START END).  If point is not on a
footnote, NIL is returned."
  ;; first make sure we're at a footnote marker
  (if (or (looking-back (concat "\\[\\^" haddock-footnote-chars "*\\]?") (line-beginning-position))
          (looking-at-p (concat "\\[?\\^" haddock-footnote-chars "*?\\]")))
      (save-excursion
        ;; move point between [ and ^:
        (if (looking-at-p "\\[")
            (forward-char 1)
          (skip-chars-backward "^["))
        (looking-at (concat "\\(\\^" haddock-footnote-chars "*?\\)\\]"))
        (list (match-string 1) (1- (match-beginning 1)) (1+ (match-end 1))))))

(defun haddock-footnote-text-positions ()
  "Return the start and end positions of the footnote text point is in.
The exact return value is a list of three elements: (ID START END).
The start position is the position of the opening bracket
of the footnote id.  The end position is directly after the
newline that ends the footnote.  If point is not in a footnote,
NIL is returned instead."
  (save-excursion
    (let (result)
      (move-beginning-of-line 1)
      ;; Try to find the label. If we haven't found the label and we're at a blank
      ;; or indented line, back up if possible.
      (while (and
              (not (and (looking-at haddock-regex-footnote-definition)
                        (setq result (list (match-string 1) (point)))))
              (and (not (bobp))
                   (or (haddock-cur-line-blank-p)
                       (>= (current-indentation) 4))))
        (forward-line -1))
      (when result
        ;; Advance if there is a next line that is either blank or indented.
        ;; (Need to check if we're on the last line, because
        ;; haddock-next-line-blank-p returns true for last line in buffer.)
        (while (and (/= (line-end-position) (point-max))
                    (or (haddock-next-line-blank-p)
                        (>= (haddock-next-line-indent) 4)))
          (forward-line))
        ;; Move back while the current line is blank.
        (while (haddock-cur-line-blank-p)
          (forward-line -1))
        ;; Advance to capture this line and a single trailing newline (if there
        ;; is one).
        (forward-line)
        (append result (list (point)))))))

(defun haddock-get-defined-footnotes ()
  "Return a list of all defined footnotes.
Result is an alist of pairs (MARKER . LINE), where MARKER is the
footnote marker, a string, and LINE is the line number containing
the footnote definition.

For example, suppose the following footnotes are defined at positions
448 and 475:

\[^1]: First footnote here.
\[^marker]: Second footnote.

Then the returned list is: ((\"^1\" . 478) (\"^marker\" . 475))"
  (save-excursion
    (goto-char (point-min))
    (let (footnotes)
      (while (haddock-search-until-condition
              (lambda () (and (not (haddock-code-block-at-point-p))
                              (not (haddock-inline-code-at-point-p))
                              (not (haddock-in-comment-p))))
              haddock-regex-footnote-definition nil t)
        (let ((marker (match-string-no-properties 1))
              (pos (match-beginning 0)))
          (unless (zerop (length marker))
            (cl-pushnew (cons marker pos) footnotes :test #'equal))))
      (reverse footnotes))))


;;; Element Removal ===========================================================

(defun haddock-kill-thing-at-point ()
  "Kill thing at point and add important text, without markup, to kill ring.
Possible things to kill include (roughly in order of precedence):
inline code, headers, horizonal rules, links (add link text to
kill ring), images (add alt text to kill ring), angle uri, email
addresses, bold, italics, reference definition (add URI to kill
ring), footnote markers and text (kill both marker and text, add
text to kill ring), and list items."
  (interactive "*")
  (let (val)
    (cond
     ;; Inline code
     ((haddock-inline-code-at-point)
      (kill-new (match-string 2))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; ATX header
     ((thing-at-point-looking-at haddock-regex-header-atx)
      (kill-new (match-string 2))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Setext header
     ((thing-at-point-looking-at haddock-regex-header-setext)
      (kill-new (match-string 1))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Horizonal rule
     ((thing-at-point-looking-at haddock-regex-hr)
      (kill-new (match-string 0))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Inline link or image (add link or alt text to kill ring)
     ((thing-at-point-looking-at haddock-regex-link-inline)
      (kill-new (match-string 3))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Reference link or image (add link or alt text to kill ring)
     ((thing-at-point-looking-at haddock-regex-link-reference)
      (kill-new (match-string 3))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Angle URI (add URL to kill ring)
     ((thing-at-point-looking-at haddock-regex-angle-uri)
      (kill-new (match-string 2))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Email address in angle brackets (add email address to kill ring)
     ((thing-at-point-looking-at haddock-regex-email)
      (kill-new (match-string 1))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Wiki link (add alias text to kill ring)
     ((and haddock-enable-wiki-links
           (thing-at-point-looking-at haddock-regex-wiki-link))
      (kill-new (haddock-wiki-link-alias))
      (delete-region (match-beginning 1) (match-end 1)))
     ;; Bold
     ((thing-at-point-looking-at haddock-regex-bold)
      (kill-new (match-string 4))
      (delete-region (match-beginning 2) (match-end 2)))
     ;; Italics
     ((thing-at-point-looking-at haddock-regex-italic)
      (kill-new (match-string 3))
      (delete-region (match-beginning 1) (match-end 1)))
     ;; Strikethrough
     ((thing-at-point-looking-at haddock-regex-strike-through)
      (kill-new (match-string 4))
      (delete-region (match-beginning 2) (match-end 2)))
     ;; Footnote marker (add footnote text to kill ring)
     ((thing-at-point-looking-at haddock-regex-footnote)
      (haddock-footnote-kill))
     ;; Footnote text (add footnote text to kill ring)
     ((setq val (haddock-footnote-text-positions))
      (haddock-footnote-kill))
     ;; Reference definition (add URL to kill ring)
     ((thing-at-point-looking-at haddock-regex-reference-definition)
      (kill-new (match-string 5))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; List item
     ((setq val (haddock-cur-list-item-bounds))
      (kill-new (delete-and-extract-region (cl-first val) (cl-second val))))
     (t
      (user-error "Nothing found at point to kill")))))

(defun haddock-kill-outline ()
  "Kill visible heading and add it to `kill-ring'."
  (interactive)
  (save-excursion
    (haddock-outline-previous)
    (kill-region (point) (progn (haddock-outline-next) (point)))))

(defun haddock-kill-block ()
  "Kill visible code block, list item, or blockquote and add it to `kill-ring'."
  (interactive)
  (save-excursion
    (haddock-backward-block)
    (kill-region (point) (progn (haddock-forward-block) (point)))))


;;; Indentation ===============================================================

(defun haddock-indent-find-next-position (cur-pos positions)
  "Return the position after the index of CUR-POS in POSITIONS.
Positions are calculated by `haddock-calc-indents'."
  (while (and positions
              (not (equal cur-pos (car positions))))
    (setq positions (cdr positions)))
  (or (cadr positions) 0))

(define-obsolete-function-alias 'haddock-exdent-find-next-position
  'haddock-outdent-find-next-position "v2.3")

(defun haddock-outdent-find-next-position (cur-pos positions)
  "Return the maximal element that precedes CUR-POS from POSITIONS.
Positions are calculated by `haddock-calc-indents'."
  (let ((result 0))
    (dolist (i positions)
      (when (< i cur-pos)
        (setq result (max result i))))
    result))

(defun haddock-indent-line ()
  "Indent the current line using some heuristics.
If the _previous_ command was either `haddock-enter-key' or
`haddock-cycle', then we should cycle to the next
reasonable indentation position.  Otherwise, we could have been
called directly by `haddock-enter-key', by an initial call of
`haddock-cycle', or indirectly by `auto-fill-mode'.  In
these cases, indent to the default position.
Positions are calculated by `haddock-calc-indents'."
  (interactive)
  (let ((positions (haddock-calc-indents))
        (point-pos (current-column))
        (_ (back-to-indentation))
        (cur-pos (current-column)))
    (if (not (equal this-command 'haddock-cycle))
        (indent-line-to (car positions))
      (setq positions (sort (delete-dups positions) '<))
      (let* ((next-pos (haddock-indent-find-next-position cur-pos positions))
             (new-point-pos (max (+ point-pos (- next-pos cur-pos)) 0)))
        (indent-line-to next-pos)
        (move-to-column new-point-pos)))))

(defun haddock-calc-indents ()
  "Return a list of indentation columns to cycle through.
The first element in the returned list should be considered the
default indentation level.  This function does not worry about
duplicate positions, which are handled up by calling functions."
  (let (pos prev-line-pos positions)

    ;; Indentation of previous line
    (setq prev-line-pos (haddock-prev-line-indent))
    (setq positions (cons prev-line-pos positions))

    ;; Indentation of previous non-list-marker text
    (when (setq pos (save-excursion
                      (forward-line -1)
                      (when (looking-at haddock-regex-list)
                        (- (match-end 3) (match-beginning 0)))))
      (setq positions (cons pos positions)))

    ;; Indentation required for a pre block in current context
    (setq pos (length (haddock-pre-indentation (point))))
    (setq positions (cons pos positions))

    ;; Indentation of the previous line + tab-width
    (if prev-line-pos
        (setq positions (cons (+ prev-line-pos tab-width) positions))
      (setq positions (cons tab-width positions)))

    ;; Indentation of the previous line - tab-width
    (if (and prev-line-pos (> prev-line-pos tab-width))
        (setq positions (cons (- prev-line-pos tab-width) positions)))

    ;; Indentation of all preceeding list markers (when in a list)
    (when (setq pos (haddock-calculate-list-levels))
      (setq positions (append pos positions)))

    ;; First column
    (setq positions (cons 0 positions))

    ;; Return reversed list
    (reverse positions)))

(defun haddock-enter-key ()
  "Handle RET depending on the context.
If the point is at a table, move to the next row.  Otherwise,
indent according to value of `haddock-indent-on-enter'.
When it is nil, simply call `newline'.  Otherwise, indent the next line
following RET using `haddock-indent-line'.  Furthermore, when it
is set to 'indent-and-new-item and the point is in a list item,
start a new item with the same indentation. If the point is in an
empty list item, remove it (so that pressing RET twice when in a
list simply adds a blank line)."
  (interactive)
  (cond
   ;; Table
   ((haddock-table-at-point-p)
    (call-interactively #'haddock-table-next-row))
   ;; Indent non-table text
   (haddock-indent-on-enter
    (let (bounds)
      (if (and (memq haddock-indent-on-enter '(indent-and-new-item))
               (setq bounds (haddock-cur-list-item-bounds)))
          (let ((beg (cl-first bounds))
                (end (cl-second bounds))
                (length (cl-fourth bounds)))
            ;; Point is in a list item
            (if (= (- end beg) length)
                ;; Delete blank list
                (progn
                  (delete-region beg end)
                  (newline)
                  (haddock-indent-line))
              (call-interactively #'haddock-insert-list-item)))
        ;; Point is not in a list
        (newline)
        (haddock-indent-line))))
   ;; Insert a raw newline
   (t (newline))))

(define-obsolete-function-alias 'haddock-exdent-or-delete
  'haddock-outdent-or-delete "v2.3")

(defun haddock-outdent-or-delete (arg)
  "Handle BACKSPACE by cycling through indentation points.
When BACKSPACE is pressed, if there is only whitespace
before the current point, then outdent the line one level.
Otherwise, do normal delete by repeating
`backward-delete-char-untabify' ARG times."
  (interactive "*p")
  (if (use-region-p)
      (backward-delete-char-untabify arg)
    (let ((cur-pos (current-column))
          (start-of-indention (save-excursion
                                (back-to-indentation)
                                (current-column)))
          (positions (haddock-calc-indents)))
      (if (and (> cur-pos 0) (= cur-pos start-of-indention))
          (indent-line-to (haddock-outdent-find-next-position cur-pos positions))
        (backward-delete-char-untabify arg)))))

(defun haddock-find-leftmost-column (beg end)
  "Find the leftmost column in the region from BEG to END."
  (let ((mincol 1000))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (back-to-indentation)
        (unless (looking-at-p "[ \t]*$")
          (setq mincol (min mincol (current-column))))
        (forward-line 1)
        ))
    mincol))

(defun haddock-indent-region (beg end arg)
  "Indent the region from BEG to END using some heuristics.
When ARG is non-nil, outdent the region instead.
See `haddock-indent-line' and `haddock-indent-line'."
  (interactive "*r\nP")
  (let* ((positions (sort (delete-dups (haddock-calc-indents)) '<))
         (leftmostcol (haddock-find-leftmost-column beg end))
         (next-pos (if arg
                       (haddock-outdent-find-next-position leftmostcol positions)
                     (haddock-indent-find-next-position leftmostcol positions))))
    (indent-rigidly beg end (- next-pos leftmostcol))
    (setq deactivate-mark nil)))

(define-obsolete-function-alias 'haddock-exdent-region
  'haddock-outdent-region "v2.3")

(defun haddock-outdent-region (beg end)
  "Call `haddock-indent-region' on region from BEG to END with prefix."
  (interactive "*r")
  (haddock-indent-region beg end t))


;;; Markup Completion =========================================================

(defconst haddock-complete-alist
  '((haddock-regex-header-atx . haddock-complete-atx)
    (haddock-regex-header-setext . haddock-complete-setext)
    (haddock-regex-hr . haddock-complete-hr))
  "Association list of form (regexp . function) for markup completion.")

(defun haddock-incomplete-atx-p ()
  "Return t if ATX header markup is incomplete and nil otherwise.
Assumes match data is available for `haddock-regex-header-atx'.
Checks that the number of trailing hash marks equals the number of leading
hash marks, that there is only a single space before and after the text,
and that there is no extraneous whitespace in the text."
  (or
   ;; Number of starting and ending hash marks differs
   (not (= (length (match-string 1)) (length (match-string 3))))
   ;; When the header text is not empty...
   (and (> (length (match-string 2)) 0)
        ;; ...if there are extra leading, trailing, or interior spaces
        (or (not (= (match-beginning 2) (1+ (match-end 1))))
            (not (= (match-beginning 3) (1+ (match-end 2))))
            (string-match-p "[ \t\n]\\{2\\}" (match-string 2))))
   ;; When the header text is empty...
   (and (= (length (match-string 2)) 0)
        ;; ...if there are too many or too few spaces
        (not (= (match-beginning 3) (+ (match-end 1) 2))))))

(defun haddock-complete-atx ()
  "Complete and normalize ATX headers.
Add or remove hash marks to the end of the header to match the
beginning.  Ensure that there is only a single space between hash
marks and header text.  Removes extraneous whitespace from header text.
Assumes match data is available for `haddock-regex-header-atx'.
Return nil if markup was complete and non-nil if markup was completed."
  (when (haddock-incomplete-atx-p)
    (let* ((new-marker (make-marker))
           (new-marker (set-marker new-marker (match-end 2))))
      ;; Hash marks and spacing at end
      (goto-char (match-end 2))
      (delete-region (match-end 2) (match-end 3))
      (insert " " (match-string 1))
      ;; Remove extraneous whitespace from title
      (replace-match (haddock-compress-whitespace-string (match-string 2))
                     t t nil 2)
      ;; Spacing at beginning
      (goto-char (match-end 1))
      (delete-region (match-end 1) (match-beginning 2))
      (insert " ")
      ;; Leave point at end of text
      (goto-char new-marker))))

(defun haddock-incomplete-setext-p ()
  "Return t if setext header markup is incomplete and nil otherwise.
Assumes match data is available for `haddock-regex-header-setext'.
Checks that length of underline matches text and that there is no
extraneous whitespace in the text."
  (or (not (= (length (match-string 1)) (length (match-string 2))))
      (string-match-p "[ \t\n]\\{2\\}" (match-string 1))))

(defun haddock-complete-setext ()
  "Complete and normalize setext headers.
Add or remove underline characters to match length of header
text.  Removes extraneous whitespace from header text.  Assumes
match data is available for `haddock-regex-header-setext'.
Return nil if markup was complete and non-nil if markup was completed."
  (when (haddock-incomplete-setext-p)
    (let* ((text (haddock-compress-whitespace-string (match-string 1)))
           (char (char-after (match-beginning 2)))
           (level (if (char-equal char ?-) 2 1)))
      (goto-char (match-beginning 0))
      (delete-region (match-beginning 0) (match-end 0))
      (haddock-insert-header level text t)
      t)))

(defun haddock-incomplete-hr-p ()
  "Return non-nil if hr is not in `haddock-hr-strings' and nil otherwise.
Assumes match data is available for `haddock-regex-hr'."
  (not (member (match-string 0) haddock-hr-strings)))

(defun haddock-complete-hr ()
  "Complete horizontal rules.
If horizontal rule string is a member of `haddock-hr-strings',
do nothing.  Otherwise, replace with the car of
`haddock-hr-strings'.
Assumes match data is available for `haddock-regex-hr'.
Return nil if markup was complete and non-nil if markup was completed."
  (when (haddock-incomplete-hr-p)
    (replace-match (car haddock-hr-strings))
    t))

(defun haddock-complete ()
  "Complete markup of object near point or in region when active.
Handle all objects in `haddock-complete-alist', in order.
See `haddock-complete-at-point' and `haddock-complete-region'."
  (interactive "*")
  (if (haddock-use-region-p)
      (haddock-complete-region (region-beginning) (region-end))
    (haddock-complete-at-point)))

(defun haddock-complete-at-point ()
  "Complete markup of object near point.
Handle all elements of `haddock-complete-alist' in order."
  (interactive "*")
  (let ((list haddock-complete-alist) found changed)
    (while list
      (let ((regexp (eval (caar list)))
            (function (cdar list)))
        (setq list (cdr list))
        (when (thing-at-point-looking-at regexp)
          (setq found t)
          (setq changed (funcall function))
          (setq list nil))))
    (if found
        (or changed (user-error "Markup at point is complete"))
      (user-error "Nothing to complete at point"))))

(defun haddock-complete-region (beg end)
  "Complete markup of objects in region from BEG to END.
Handle all objects in `haddock-complete-alist', in order.  Each
match is checked to ensure that a previous regexp does not also
match."
  (interactive "*r")
  (let ((end-marker (set-marker (make-marker) end))
        previous)
    (dolist (element haddock-complete-alist)
      (let ((regexp (eval (car element)))
            (function (cdr element)))
        (goto-char beg)
        (while (re-search-forward regexp end-marker 'limit)
          (when (match-string 0)
            ;; Make sure this is not a match for any of the preceding regexps.
            ;; This prevents mistaking an HR for a Setext subheading.
            (let (match)
              (save-match-data
                (dolist (prev-regexp previous)
                  (or match (setq match (looking-back prev-regexp nil)))))
              (unless match
                (save-excursion (funcall function))))))
        (cl-pushnew regexp previous :test #'equal)))
    previous))

(defun haddock-complete-buffer ()
  "Complete markup for all objects in the current buffer."
  (interactive "*")
  (haddock-complete-region (point-min) (point-max)))


;;; Markup Cycling ============================================================

(defun haddock-cycle-atx (arg &optional remove)
  "Cycle ATX header markup.
Promote header (decrease level) when ARG is 1 and demote
header (increase level) if arg is -1.  When REMOVE is non-nil,
remove the header when the level reaches zero and stop cycling
when it reaches six.  Otherwise, perform a proper cycling through
levels one through six.  Assumes match data is available for
`haddock-regex-header-atx'."
  (let* ((old-level (length (match-string 1)))
         (new-level (+ old-level arg))
         (text (match-string 2)))
    (when (not remove)
      (setq new-level (% new-level 6))
      (setq new-level (cond ((= new-level 0) 6)
                            ((< new-level 0) (+ new-level 6))
                            (t new-level))))
    (cond
     ((= new-level 0)
      (haddock-unwrap-thing-at-point nil 0 2))
     ((<= new-level 6)
      (goto-char (match-beginning 0))
      (delete-region (match-beginning 0) (match-end 0))
      (haddock-insert-header new-level text nil)))))

(defun haddock-cycle-setext (arg &optional remove)
  "Cycle setext header markup.
Promote header (increase level) when ARG is 1 and demote
header (decrease level or remove) if arg is -1.  When demoting a
level-two setext header, replace with a level-three atx header.
When REMOVE is non-nil, remove the header when the level reaches
zero.  Otherwise, cycle back to a level six atx header.  Assumes
match data is available for `haddock-regex-header-setext'."
  (let* ((char (char-after (match-beginning 2)))
         (old-level (if (char-equal char ?=) 1 2))
         (new-level (+ old-level arg)))
    (when (and (not remove) (= new-level 0))
      (setq new-level 6))
    (cond
     ((= new-level 0)
      (haddock-unwrap-thing-at-point nil 0 1))
     ((<= new-level 2)
      (haddock-insert-header new-level nil t))
     ((<= new-level 6)
      (haddock-insert-header new-level nil nil)))))

(defun haddock-cycle-hr (arg &optional remove)
  "Cycle string used for horizontal rule from `haddock-hr-strings'.
When ARG is 1, cycle forward (demote), and when ARG is -1, cycle
backwards (promote).  When REMOVE is non-nil, remove the hr instead
of cycling when the end of the list is reached.
Assumes match data is available for `haddock-regex-hr'."
  (let* ((strings (if (= arg -1)
                      (reverse haddock-hr-strings)
                    haddock-hr-strings))
         (tail (member (match-string 0) strings))
         (new (or (cadr tail)
                  (if remove
                      (if (= arg 1)
                          ""
                        (car tail))
                    (car strings)))))
    (replace-match new)))

(defun haddock-cycle-bold ()
  "Cycle bold markup between underscores and asterisks.
Assumes match data is available for `haddock-regex-bold'."
  (save-excursion
    (let* ((old-delim (match-string 3))
           (new-delim (if (string-equal old-delim "**") "__" "**")))
      (replace-match new-delim t t nil 3)
      (replace-match new-delim t t nil 5))))

(defun haddock-cycle-italic ()
  "Cycle italic markup between underscores and asterisks.
Assumes match data is available for `haddock-regex-italic'."
  (save-excursion
    (let* ((old-delim (match-string 2))
           (new-delim (if (string-equal old-delim "*") "_" "*")))
      (replace-match new-delim t t nil 2)
      (replace-match new-delim t t nil 4))))


;;; Keymap ====================================================================

(defun haddock--style-map-prompt ()
  "Return a formatted prompt for Haddock markup insertion."
  (when haddock-enable-prefix-prompts
    (concat
     "Haddock: "
     (propertize "bold" 'face 'haddock-bold-face) ", "
     (propertize "italic" 'face 'haddock-italic-face) ", "
     (propertize "code" 'face 'haddock-inline-code-face) ", "
     (propertize "C = GFM code" 'face 'haddock-code-face) ", "
     (propertize "pre" 'face 'haddock-pre-face) ", "
     (propertize "footnote" 'face 'haddock-footnote-text-face) ", "
     (propertize "q = blockquote" 'face 'haddock-blockquote-face) ", "
     (propertize "h & 1-6 = heading" 'face 'haddock-header-face) ", "
     (propertize "- = hr" 'face 'haddock-hr-face) ", "
     "C-h = more")))

(defun haddock--command-map-prompt ()
  "Return prompt for Haddock buffer-wide commands."
  (when haddock-enable-prefix-prompts
    (concat
     "Command: "
     (propertize "m" 'face 'haddock-bold-face) "arkdown, "
     (propertize "p" 'face 'haddock-bold-face) "review, "
     (propertize "o" 'face 'haddock-bold-face) "pen, "
     (propertize "e" 'face 'haddock-bold-face) "xport, "
     "export & pre" (propertize "v" 'face 'haddock-bold-face) "iew, "
     (propertize "c" 'face 'haddock-bold-face) "heck refs, "
     (propertize "u" 'face 'haddock-bold-face) "nused refs, "
     "C-h = more")))

(defvar haddock-mode-style-map
  (let ((map (make-keymap (haddock--style-map-prompt))))
    (define-key map (kbd "1") 'haddock-insert-header-atx-1)
    (define-key map (kbd "2") 'haddock-insert-header-atx-2)
    (define-key map (kbd "3") 'haddock-insert-header-atx-3)
    (define-key map (kbd "4") 'haddock-insert-header-atx-4)
    (define-key map (kbd "5") 'haddock-insert-header-atx-5)
    (define-key map (kbd "6") 'haddock-insert-header-atx-6)
    (define-key map (kbd "!") 'haddock-insert-header-setext-1)
    (define-key map (kbd "@") 'haddock-insert-header-setext-2)
    (define-key map (kbd "b") 'haddock-insert-bold)
    (define-key map (kbd "c") 'haddock-insert-code)
    (define-key map (kbd "C") 'haddock-insert-gfm-code-block)
    (define-key map (kbd "f") 'haddock-insert-footnote)
    (define-key map (kbd "h") 'haddock-insert-header-dwim)
    (define-key map (kbd "H") 'haddock-insert-header-setext-dwim)
    (define-key map (kbd "i") 'haddock-insert-italic)
    (define-key map (kbd "k") 'haddock-insert-kbd)
    (define-key map (kbd "l") 'haddock-insert-link)
    (define-key map (kbd "p") 'haddock-insert-pre)
    (define-key map (kbd "P") 'haddock-pre-region)
    (define-key map (kbd "q") 'haddock-insert-blockquote)
    (define-key map (kbd "s") 'haddock-insert-strike-through)
    (define-key map (kbd "t") 'haddock-insert-table)
    (define-key map (kbd "Q") 'haddock-blockquote-region)
    (define-key map (kbd "w") 'haddock-insert-wiki-link)
    (define-key map (kbd "-") 'haddock-insert-hr)
    (define-key map (kbd "[") 'haddock-insert-gfm-checkbox)
    ;; Deprecated keys that may be removed in a future version
    (define-key map (kbd "e") 'haddock-insert-italic)
    map)
  "Keymap for Haddock text styling commands.")

(defvar haddock-mode-command-map
  (let ((map (make-keymap (haddock--command-map-prompt))))
    (define-key map (kbd "m") 'haddock-other-window)
    (define-key map (kbd "p") 'haddock-preview)
    (define-key map (kbd "e") 'haddock-export)
    (define-key map (kbd "v") 'haddock-export-and-preview)
    (define-key map (kbd "o") 'haddock-open)
    (define-key map (kbd "l") 'haddock-live-preview-mode)
    (define-key map (kbd "w") 'haddock-kill-ring-save)
    (define-key map (kbd "c") 'haddock-check-refs)
    (define-key map (kbd "u") 'haddock-unused-refs)
    (define-key map (kbd "n") 'haddock-cleanup-list-numbers)
    (define-key map (kbd "]") 'haddock-complete-buffer)
    (define-key map (kbd "^") 'haddock-table-sort-lines)
    (define-key map (kbd "|") 'haddock-table-convert-region)
    (define-key map (kbd "t") 'haddock-table-transpose)
    map)
  "Keymap for Haddock buffer-wide commands.")

(defvar haddock-mode-map
  (let ((map (make-keymap)))
    ;; Markup insertion & removal
    (define-key map (kbd "C-c C-s") haddock-mode-style-map)
    (define-key map (kbd "C-c C-l") 'haddock-insert-link)
    (define-key map (kbd "C-c C-k") 'haddock-kill-thing-at-point)
    ;; Promotion, demotion, and cycling
    (define-key map (kbd "C-c C--") 'haddock-promote)
    (define-key map (kbd "C-c C-=") 'haddock-demote)
    (define-key map (kbd "C-c C-]") 'haddock-complete)
    ;; Following and doing things
    (define-key map (kbd "C-c C-o") 'haddock-follow-thing-at-point)
    (define-key map (kbd "C-c C-d") 'haddock-do)
    (define-key map (kbd "C-c '") 'haddock-edit-code-block)
    ;; Indentation
    (define-key map (kbd "C-m") 'haddock-enter-key)
    (define-key map (kbd "DEL") 'haddock-outdent-or-delete)
    (define-key map (kbd "C-c >") 'haddock-indent-region)
    (define-key map (kbd "C-c <") 'haddock-outdent-region)
    ;; Visibility cycling
    (define-key map (kbd "TAB") 'haddock-cycle)
    (define-key map (kbd "<S-iso-lefttab>") 'haddock-shifttab)
    (define-key map (kbd "<S-tab>")  'haddock-shifttab)
    (define-key map (kbd "<backtab>") 'haddock-shifttab)
    ;; Heading and list navigation
    (define-key map (kbd "C-c C-n") 'haddock-outline-next)
    (define-key map (kbd "C-c C-p") 'haddock-outline-previous)
    (define-key map (kbd "C-c C-f") 'haddock-outline-next-same-level)
    (define-key map (kbd "C-c C-b") 'haddock-outline-previous-same-level)
    (define-key map (kbd "C-c C-u") 'haddock-outline-up)
    ;; Buffer-wide commands
    (define-key map (kbd "C-c C-c") haddock-mode-command-map)
    ;; Subtree, list, and table editing
    (define-key map (kbd "C-c <up>") 'haddock-move-up)
    (define-key map (kbd "C-c <down>") 'haddock-move-down)
    (define-key map (kbd "C-c <left>") 'haddock-promote)
    (define-key map (kbd "C-c <right>") 'haddock-demote)
    (define-key map (kbd "C-c S-<up>") 'haddock-table-delete-row)
    (define-key map (kbd "C-c S-<down>") 'haddock-table-insert-row)
    (define-key map (kbd "C-c S-<left>") 'haddock-table-delete-column)
    (define-key map (kbd "C-c S-<right>") 'haddock-table-insert-column)
    (define-key map (kbd "C-c C-M-h") 'haddock-mark-subtree)
    (define-key map (kbd "C-x n s") 'haddock-narrow-to-subtree)
    (define-key map (kbd "M-RET") 'haddock-insert-list-item)
    (define-key map (kbd "C-c C-j") 'haddock-insert-list-item)
    ;; Paragraphs (Haddock context aware)
    (define-key map [remap backward-paragraph] 'haddock-backward-paragraph)
    (define-key map [remap forward-paragraph] 'haddock-forward-paragraph)
    (define-key map [remap mark-paragraph] 'haddock-mark-paragraph)
    ;; Blocks (one or more paragraphs)
    (define-key map (kbd "C-M-{") 'haddock-backward-block)
    (define-key map (kbd "C-M-}") 'haddock-forward-block)
    (define-key map (kbd "C-c M-h") 'haddock-mark-block)
    (define-key map (kbd "C-x n b") 'haddock-narrow-to-block)
    ;; Pages (top-level sections)
    (define-key map [remap backward-page] 'haddock-backward-page)
    (define-key map [remap forward-page] 'haddock-forward-page)
    (define-key map [remap mark-page] 'haddock-mark-page)
    (define-key map [remap narrow-to-page] 'haddock-narrow-to-page)
    ;; Link Movement
    (define-key map (kbd "M-n") 'haddock-next-link)
    (define-key map (kbd "M-p") 'haddock-previous-link)
    ;; Toggling functionality
    (define-key map (kbd "C-c C-x C-e") 'haddock-toggle-math)
    (define-key map (kbd "C-c C-x C-f") 'haddock-toggle-fontify-code-blocks-natively)
    (define-key map (kbd "C-c C-x C-i") 'haddock-toggle-inline-images)
    (define-key map (kbd "C-c C-x C-l") 'haddock-toggle-url-hiding)
    (define-key map (kbd "C-c C-x C-m") 'haddock-toggle-markup-hiding)
    ;; Alternative keys (in case of problems with the arrow keys)
    (define-key map (kbd "C-c C-x u") 'haddock-move-up)
    (define-key map (kbd "C-c C-x d") 'haddock-move-down)
    (define-key map (kbd "C-c C-x l") 'haddock-promote)
    (define-key map (kbd "C-c C-x r") 'haddock-demote)
    ;; Deprecated keys that may be removed in a future version
    (define-key map (kbd "C-c C-a L") 'haddock-insert-link) ;; C-c C-l
    (define-key map (kbd "C-c C-a l") 'haddock-insert-link) ;; C-c C-l
    (define-key map (kbd "C-c C-a r") 'haddock-insert-link) ;; C-c C-l
    (define-key map (kbd "C-c C-a u") 'haddock-insert-uri) ;; C-c C-l
    (define-key map (kbd "C-c C-a f") 'haddock-insert-footnote)
    (define-key map (kbd "C-c C-a w") 'haddock-insert-wiki-link)
    (define-key map (kbd "C-c C-t 1") 'haddock-insert-header-atx-1)
    (define-key map (kbd "C-c C-t 2") 'haddock-insert-header-atx-2)
    (define-key map (kbd "C-c C-t 3") 'haddock-insert-header-atx-3)
    (define-key map (kbd "C-c C-t 4") 'haddock-insert-header-atx-4)
    (define-key map (kbd "C-c C-t 5") 'haddock-insert-header-atx-5)
    (define-key map (kbd "C-c C-t 6") 'haddock-insert-header-atx-6)
    (define-key map (kbd "C-c C-t !") 'haddock-insert-header-setext-1)
    (define-key map (kbd "C-c C-t @") 'haddock-insert-header-setext-2)
    (define-key map (kbd "C-c C-t h") 'haddock-insert-header-dwim)
    (define-key map (kbd "C-c C-t H") 'haddock-insert-header-setext-dwim)
    (define-key map (kbd "C-c C-t s") 'haddock-insert-header-setext-2)
    (define-key map (kbd "C-c C-t t") 'haddock-insert-header-setext-1)
    (define-key map (kbd "C-c C-i") 'haddock-insert-image)
    (define-key map (kbd "C-c C-x m") 'haddock-insert-list-item) ;; C-c C-j
    (define-key map (kbd "C-c C-x C-x") 'haddock-toggle-gfm-checkbox) ;; C-c C-d
    (define-key map (kbd "C-c -") 'haddock-insert-hr)
    map)
  "Keymap for Haddock major mode.")

(defvar haddock-mode-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'haddock-follow-link-at-point)
    map)
  "Keymap for following links with mouse.")

(defvar gfm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map haddock-mode-map)
    (define-key map (kbd "C-c C-s d") 'haddock-insert-strike-through)
    (define-key map "`" 'haddock-electric-backquote)
    map)
  "Keymap for `gfm-mode'.
See also `haddock-mode-map'.")


;;; Menu ======================================================================

(easy-menu-define haddock-mode-menu haddock-mode-map
  "Menu for Haddock mode"
  '("Haddock"
    "---"
    ("Movement"
     ["Jump" haddock-do]
     ["Follow Link" haddock-follow-thing-at-point]
     ["Next Link" haddock-next-link]
     ["Previous Link" haddock-previous-link]
     "---"
     ["Next Heading or List Item" haddock-outline-next]
     ["Previous Heading or List Item" haddock-outline-previous]
     ["Next at Same Level" haddock-outline-next-same-level]
     ["Previous at Same Level" haddock-outline-previous-same-level]
     ["Up to Parent" haddock-outline-up]
     "---"
     ["Forward Paragraph" haddock-forward-paragraph]
     ["Backward Paragraph" haddock-backward-paragraph]
     ["Forward Block" haddock-forward-block]
     ["Backward Block" haddock-backward-block])
    ("Show & Hide"
     ["Cycle Heading Visibility" haddock-cycle
      :enable (haddock-on-heading-p)]
     ["Cycle Heading Visibility (Global)" haddock-shifttab]
     "---"
     ["Narrow to Region" narrow-to-region]
     ["Narrow to Block" haddock-narrow-to-block]
     ["Narrow to Section" narrow-to-defun]
     ["Narrow to Subtree" haddock-narrow-to-subtree]
     ["Widen" widen (buffer-narrowed-p)]
     "---"
     ["Toggle Markup Hiding" haddock-toggle-markup-hiding
      :keys "C-c C-x C-m"
      :style radio
      :selected haddock-hide-markup])
    "---"
    ("Headings & Structure"
     ["Automatic Heading" haddock-insert-header-dwim
      :keys "C-c C-s h"]
     ["Automatic Heading (Setext)" haddock-insert-header-setext-dwim
      :keys "C-c C-s H"]
     ("Specific Heading (atx)"
      ["First Level atx" haddock-insert-header-atx-1
       :keys "C-c C-s 1"]
      ["Second Level atx" haddock-insert-header-atx-2
       :keys "C-c C-s 2"]
      ["Third Level atx" haddock-insert-header-atx-3
       :keys "C-c C-s 3"]
      ["Fourth Level atx" haddock-insert-header-atx-4
       :keys "C-c C-s 4"]
      ["Fifth Level atx" haddock-insert-header-atx-5
       :keys "C-c C-s 5"]
      ["Sixth Level atx" haddock-insert-header-atx-6
       :keys "C-c C-s 6"])
     ("Specific Heading (Setext)"
      ["First Level Setext" haddock-insert-header-setext-1
       :keys "C-c C-s !"]
      ["Second Level Setext" haddock-insert-header-setext-2
       :keys "C-c C-s @"])
     ["Horizontal Rule" haddock-insert-hr
      :keys "C-c C-s -"]
     "---"
     ["Move Subtree Up" haddock-move-up
      :keys "C-c <up>"]
     ["Move Subtree Down" haddock-move-down
      :keys "C-c <down>"]
     ["Promote Subtree" haddock-promote
      :keys "C-c <left>"]
     ["Demote Subtree" haddock-demote
      :keys "C-c <right>"])
    ("Region & Mark"
     ["Indent Region" haddock-indent-region]
     ["Outdent Region" haddock-outdent-region]
     "--"
     ["Mark Paragraph" mark-paragraph]
     ["Mark Block" haddock-mark-block]
     ["Mark Section" mark-defun]
     ["Mark Subtree" haddock-mark-subtree])
    ("Tables"
     ["Move Row Up" haddock-move-up
      :enable (haddock-table-at-point-p)
      :keys "C-c <up>"]
     ["Move Row Down" haddock-move-down
      :enable (haddock-table-at-point-p)
      :keys "C-c <down>"]
     ["Move Column Left" haddock-demote
      :enable (haddock-table-at-point-p)
      :keys "C-c <left>"]
     ["Move Column Right" haddock-promote
      :enable (haddock-table-at-point-p)
      :keys "C-c <right>"]
     ["Delete Row" haddock-table-delete-row
      :enable (haddock-table-at-point-p)]
     ["Insert Row" haddock-table-insert-row
      :enable (haddock-table-at-point-p)]
     ["Delete Column" haddock-table-delete-column
      :enable (haddock-table-at-point-p)]
     ["Insert Column" haddock-table-insert-column
      :enable (haddock-table-at-point-p)]
     ["Insert Table" haddock-insert-table]
     "--"
     ["Convert Region to Table" haddock-table-convert-region]
     ["Sort Table Lines" haddock-table-sort-lines
      :enable (haddock-table-at-point-p)]
     ["Transpose Table" haddock-table-transpose
      :enable (haddock-table-at-point-p)])
    ("Lists"
     ["Insert List Item" haddock-insert-list-item]
     ["Move Subtree Up" haddock-move-up
      :keys "C-c <up>"]
     ["Move Subtree Down" haddock-move-down
      :keys "C-c <down>"]
     ["Indent Subtree" haddock-demote
      :keys "C-c <right>"]
     ["Outdent Subtree" haddock-promote
      :keys "C-c <left>"]
     ["Renumber List" haddock-cleanup-list-numbers]
     ["Insert Task List Item" haddock-insert-gfm-checkbox
      :keys "C-c C-x ["]
     ["Toggle Task List Item" haddock-toggle-gfm-checkbox
      :enable (haddock-gfm-task-list-item-at-point)
      :keys "C-c C-d"])
    ("Links & Images"
     ["Insert Link" haddock-insert-link]
     ["Insert Image" haddock-insert-image]
     ["Insert Footnote" haddock-insert-footnote
      :keys "C-c C-s f"]
     ["Insert Wiki Link" haddock-insert-wiki-link
      :keys "C-c C-s w"]
     "---"
     ["Check References" haddock-check-refs]
     ["Find Unused References" haddock-unused-refs]
     ["Toggle URL Hiding" haddock-toggle-url-hiding
      :style radio
      :selected haddock-hide-urls]
     ["Toggle Inline Images" haddock-toggle-inline-images
      :keys "C-c C-x C-i"
      :style radio
      :selected haddock-inline-image-overlays]
     ["Toggle Wiki Links" haddock-toggle-wiki-links
      :style radio
      :selected haddock-enable-wiki-links])
    ("Styles"
     ["Bold" haddock-insert-bold]
     ["Italic" haddock-insert-italic]
     ["Code" haddock-insert-code]
     ["Strikethrough" haddock-insert-strike-through]
     ["Keyboard" haddock-insert-kbd]
     "---"
     ["Blockquote" haddock-insert-blockquote]
     ["Preformatted" haddock-insert-pre]
     ["GFM Code Block" haddock-insert-gfm-code-block]
     ["Edit Code Block" haddock-edit-code-block
      :enable (haddock-code-block-at-point-p)]
     "---"
     ["Blockquote Region" haddock-blockquote-region]
     ["Preformatted Region" haddock-pre-region]
     "---"
     ["Fontify Code Blocks Natively"
      haddock-toggle-fontify-code-blocks-natively
      :style radio
      :selected haddock-fontify-code-blocks-natively]
     ["LaTeX Math Support" haddock-toggle-math
      :style radio
      :selected haddock-enable-math])
    "---"
    ("Preview & Export"
     ["Compile" haddock-other-window]
     ["Preview" haddock-preview]
     ["Export" haddock-export]
     ["Export & View" haddock-export-and-preview]
     ["Open" haddock-open]
     ["Live Export" haddock-live-preview-mode
      :style radio
      :selected haddock-live-preview-mode]
     ["Kill ring save" haddock-kill-ring-save])
    ("Markup Completion and Cycling"
     ["Complete Markup" haddock-complete]
     ["Promote Element" haddock-promote
      :keys "C-c C--"]
     ["Demote Element" haddock-demote
      :keys "C-c C-="])
    "---"
    ["Kill Element" haddock-kill-thing-at-point]
    "---"
    ("Documentation"
     ["Version" haddock-show-version]
     ["Homepage" haddock-mode-info]
     ["Describe Mode" (describe-function 'haddock-mode)]
     ["Guide" (browse-url "https://leanpub.com/haddock-mode")])))


;;; imenu =====================================================================

(defun haddock-imenu-create-nested-index ()
  "Create and return a nested imenu index alist for the current buffer.
See `imenu-create-index-function' and `imenu--index-alist' for details."
  (let* ((root '(nil . nil))
         cur-alist
         (cur-level 0)
         (empty-heading "-")
         (self-heading ".")
         hashes pos level heading)
    (save-excursion
      ;; Headings
      (goto-char (point-min))
      (while (re-search-forward haddock-regex-header (point-max) t)
        (unless (haddock-code-block-at-point-p)
          (cond
           ((match-string-no-properties 2) ;; level 1 setext
            (setq heading (match-string-no-properties 1))
            (setq pos (match-beginning 1)
                  level 1))
           ((match-string-no-properties 3) ;; level 2 setext
            (setq heading (match-string-no-properties 1))
            (setq pos (match-beginning 1)
                  level 2))
           ((setq hashes (haddock-trim-whitespace
                          (match-string-no-properties 4)))
            (setq heading (match-string-no-properties 5)
                  pos (match-beginning 4)
                  level (length hashes))))
          (let ((alist (list (cons heading pos))))
            (cond
             ((= cur-level level)       ; new sibling
              (setcdr cur-alist alist)
              (setq cur-alist alist))
             ((< cur-level level)       ; first child
              (dotimes (_ (- level cur-level 1))
                (setq alist (list (cons empty-heading alist))))
              (if cur-alist
                  (let* ((parent (car cur-alist))
                         (self-pos (cdr parent)))
                    (setcdr parent (cons (cons self-heading self-pos) alist)))
                (setcdr root alist))    ; primogenitor
              (setq cur-alist alist)
              (setq cur-level level))
             (t                         ; new sibling of an ancestor
              (let ((sibling-alist (last (cdr root))))
                (dotimes (_ (1- level))
                  (setq sibling-alist (last (cdar sibling-alist))))
                (setcdr sibling-alist alist)
                (setq cur-alist alist))
              (setq cur-level level))))))
      ;; Footnotes
      (let ((fn (haddock-get-defined-footnotes)))
        (if (or (zerop (length fn))
                (null haddock-add-footnotes-to-imenu))
            (cdr root)
          (nconc (cdr root) (list (cons "Footnotes" fn))))))))

(defun haddock-imenu-create-flat-index ()
  "Create and return a flat imenu index alist for the current buffer.
See `imenu-create-index-function' and `imenu--index-alist' for details."
  (let* ((empty-heading "-") index heading pos)
    (save-excursion
      ;; Headings
      (goto-char (point-min))
      (while (re-search-forward haddock-regex-header (point-max) t)
        (when (and (not (haddock-code-block-at-point-p (point-at-bol)))
                   (not (haddock-text-property-at-point 'haddock-yaml-metadata-begin)))
          (cond
           ((setq heading (match-string-no-properties 1))
            (setq pos (match-beginning 1)))
           ((setq heading (match-string-no-properties 5))
            (setq pos (match-beginning 4))))
          (or (> (length heading) 0)
              (setq heading empty-heading))
          (setq index (append index (list (cons heading pos))))))
      ;; Footnotes
      (when haddock-add-footnotes-to-imenu
        (nconc index (haddock-get-defined-footnotes)))
      index)))


;;; References ================================================================

(defun haddock-reference-goto-definition ()
  "Jump to the definition of the reference at point or create it."
  (interactive)
  (when (thing-at-point-looking-at haddock-regex-link-reference)
    (let* ((text (match-string-no-properties 3))
           (reference (match-string-no-properties 6))
           (target (downcase (if (string= reference "") text reference)))
           (loc (cadr (save-match-data (haddock-reference-definition target)))))
      (if loc
          (goto-char loc)
        (goto-char (match-beginning 0))
        (haddock-insert-reference-definition target)))))

(defun haddock-reference-find-links (reference)
  "Return a list of all links for REFERENCE.
REFERENCE should not include the surrounding square brackets.
Elements of the list have the form (text start line), where
text is the link text, start is the location at the beginning of
the link, and line is the line number on which the link appears."
  (let* ((ref-quote (regexp-quote reference))
         (regexp (format "!?\\(?:\\[\\(%s\\)\\][ ]?\\[\\]\\|\\[\\([^]]+?\\)\\][ ]?\\[%s\\]\\)"
                         ref-quote ref-quote))
         links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let* ((text (or (match-string-no-properties 1)
                         (match-string-no-properties 2)))
               (start (match-beginning 0))
               (line (haddock-line-number-at-pos)))
          (cl-pushnew (list text start line) links :test #'equal))))
    links))

(defmacro haddock-for-all-refs (f)
  `(let ((result))
     (save-excursion
       (goto-char (point-min))
       (while
           (re-search-forward haddock-regex-link-reference nil t)
         (let* ((text (match-string-no-properties 3))
                (reference (match-string-no-properties 6))
                (target (downcase (if (string= reference "") text reference))))
          (,f text target result))))
     (reverse result)))

(defmacro haddock-collect-always (_ target result)
  `(cl-pushnew ,target ,result :test #'equal))

(defmacro haddock-collect-undefined (text target result)
  `(unless (haddock-reference-definition target)
     (let ((entry (assoc ,target ,result)))
       (if (not entry)
           (cl-pushnew
            (cons ,target (list (cons ,text (haddock-line-number-at-pos))))
            ,result :test #'equal)
         (setcdr entry
                 (append (cdr entry) (list (cons ,text (haddock-line-number-at-pos)))))))))

(defun haddock-get-all-refs ()
  "Return a list of all Haddock references."
  (haddock-for-all-refs haddock-collect-always))

(defun haddock-get-undefined-refs ()
  "Return a list of undefined Haddock references.
Result is an alist of pairs (reference . occurrences), where
occurrences is itself another alist of pairs (label . line-number).
For example, an alist corresponding to [Nice editor][Emacs] at line 12,
\[GNU Emacs][Emacs] at line 45 and [manual][elisp] at line 127 is
\((\"emacs\" (\"Nice editor\" . 12) (\"GNU Emacs\" . 45)) (\"elisp\" (\"manual\" . 127)))."
  (haddock-for-all-refs haddock-collect-undefined))

(defun haddock-get-unused-refs ()
  (cl-sort
   (cl-set-difference
    (haddock-get-defined-references) (haddock-get-all-refs)
    :test (lambda (e1 e2) (equal (car e1) e2)))
   #'< :key #'cdr))

(defmacro defun-haddock-buffer (name docstring)
  "Define a function to name and return a buffer.

By convention, NAME must be a name of a string constant with
%buffer% placeholder used to name the buffer, and will also be
used as a name of the function defined.

DOCSTRING will be used as the first part of the docstring."
  `(defun ,name (&optional buffer-name)
     ,(concat docstring "\n\nBUFFER-NAME is the name of the main buffer being visited.")
     (or buffer-name (setq buffer-name (buffer-name)))
     (let ((refbuf (get-buffer-create (haddock-replace-regexp-in-string
                                       "%buffer%" buffer-name
                                       ,name))))
       (with-current-buffer refbuf
         (when view-mode
           (View-exit-and-edit))
         (use-local-map button-buffer-map)
         (erase-buffer))
       refbuf)))

(defconst haddock-reference-check-buffer
  "*Undefined references for %buffer%*"
  "Pattern for name of buffer for listing undefined references.
The string %buffer% will be replaced by the corresponding
`haddock-mode' buffer name.")

(defun-haddock-buffer
  haddock-reference-check-buffer
  "Name and return buffer for reference checking.")

(defconst haddock-unused-references-buffer
  "*Unused references for %buffer%*"
  "Pattern for name of buffer for listing unused references.
The string %buffer% will be replaced by the corresponding
`haddock-mode' buffer name.")

(defun-haddock-buffer
  haddock-unused-references-buffer
  "Name and return buffer for unused reference checking.")

(defconst haddock-reference-links-buffer
  "*Reference links for %buffer%*"
  "Pattern for name of buffer for listing references.
The string %buffer% will be replaced by the corresponding buffer name.")

(defun-haddock-buffer
  haddock-reference-links-buffer
  "Name, setup, and return a buffer for listing links.")

;; Add an empty Haddock reference definition to buffer
;; specified in the 'target-buffer property.  The reference name is
;; the button's label.
(define-button-type 'haddock-undefined-reference-button
  'help-echo "mouse-1, RET: create definition for undefined reference"
  'follow-link t
  'face 'bold
  'action (lambda (b)
            (let ((buffer (button-get b 'target-buffer))
                  (line (button-get b 'target-line))
                  (label (button-label b)))
              (switch-to-buffer-other-window buffer)
              (goto-char (point-min))
              (forward-line line)
              (haddock-insert-reference-definition label)
              (haddock-check-refs t))))

;; Jump to line in buffer specified by 'target-buffer property.
;; Line number is button's 'target-line property.
(define-button-type 'haddock-goto-line-button
  'help-echo "mouse-1, RET: go to line"
  'follow-link t
  'face 'italic
  'action (lambda (b)
            (switch-to-buffer-other-window (button-get b 'target-buffer))
            ;; use call-interactively to silence compiler
            (let ((current-prefix-arg (button-get b 'target-line)))
              (call-interactively 'goto-line))))

;; Kill a line in buffer specified by 'target-buffer property.
;; Line number is button's 'target-line property.
(define-button-type 'haddock-kill-line-button
  'help-echo "mouse-1, RET: kill line"
  'follow-link t
  'face 'italic
  'action (lambda (b)
            (switch-to-buffer-other-window (button-get b 'target-buffer))
            ;; use call-interactively to silence compiler
            (let ((current-prefix-arg (button-get b 'target-line)))
              (call-interactively 'goto-line))
            (kill-line 1)
            (haddock-unused-refs t)))

;; Jumps to a particular link at location given by 'target-char
;; property in buffer given by 'target-buffer property.
(define-button-type 'haddock-location-button
  'help-echo "mouse-1, RET: jump to location of link"
  'follow-link t
  'face 'bold
  'action (lambda (b)
            (let ((target (button-get b 'target-buffer))
                  (loc (button-get b 'target-char)))
              (kill-buffer-and-window)
              (switch-to-buffer target)
              (goto-char loc))))

(defun haddock-insert-undefined-reference-button (reference oldbuf)
  "Insert a button for creating REFERENCE in buffer OLDBUF.
REFERENCE should be a list of the form (reference . occurrences),
as returned by `haddock-get-undefined-refs'."
  (let ((label (car reference)))
    ;; Create a reference button
    (insert-button label
                   :type 'haddock-undefined-reference-button
                   'target-buffer oldbuf
                   'target-line (cdr (car (cdr reference))))
    (insert " (")
    (dolist (occurrence (cdr reference))
      (let ((line (cdr occurrence)))
        ;; Create a line number button
        (insert-button (number-to-string line)
                       :type 'haddock-goto-line-button
                       'target-buffer oldbuf
                       'target-line line)
        (insert " ")))
    (delete-char -1)
    (insert ")")
    (newline)))

(defun haddock-insert-unused-reference-button (reference oldbuf)
  "Insert a button for creating REFERENCE in buffer OLDBUF.
REFERENCE must be a pair of (ref . line-number)."
  (let ((label (car reference))
        (line (cdr reference)))
    ;; Create a reference button
    (insert-button label
                   :type 'haddock-goto-line-button
                   'face 'bold
                   'target-buffer oldbuf
                   'target-line line)
    (insert (format " (%d) [" line))
    (insert-button "X"
                   :type 'haddock-kill-line-button
                   'face 'bold
                   'target-buffer oldbuf
                   'target-line line)
    (insert "]")
    (newline)))

(defun haddock-insert-link-button (link oldbuf)
  "Insert a button for jumping to LINK in buffer OLDBUF.
LINK should be a list of the form (text char line) containing
the link text, location, and line number."
  (let ((label (cl-first link))
        (char (cl-second link))
        (line (cl-third link)))
    ;; Create a reference button
    (insert-button label
                   :type 'haddock-location-button
                   'target-buffer oldbuf
                   'target-char char)
    (insert (format " (line %d)\n" line))))

(defun haddock-reference-goto-link (&optional reference)
  "Jump to the location of the first use of REFERENCE."
  (interactive)
  (unless reference
    (if (thing-at-point-looking-at haddock-regex-reference-definition)
        (setq reference (match-string-no-properties 2))
      (user-error "No reference definition at point")))
  (let ((links (haddock-reference-find-links reference)))
    (cond ((= (length links) 1)
           (goto-char (cadr (car links))))
          ((> (length links) 1)
           (let ((oldbuf (current-buffer))
                 (linkbuf (haddock-reference-links-buffer)))
             (with-current-buffer linkbuf
               (insert "Links using reference " reference ":\n\n")
               (dolist (link (reverse links))
                 (haddock-insert-link-button link oldbuf)))
             (view-buffer-other-window linkbuf)
             (goto-char (point-min))
             (forward-line 2)))
          (t
           (error "No links for reference %s" reference)))))

(defmacro defun-haddock-ref-checker
    (name docstring checker-function buffer-function none-message buffer-header insert-reference)
  "Define a function NAME acting on result of CHECKER-FUNCTION.

DOCSTRING is used as a docstring for the defined function.

BUFFER-FUNCTION should name and return an auxiliary buffer to put
results in.

NONE-MESSAGE is used when CHECKER-FUNCTION returns no results.

BUFFER-HEADER is put into the auxiliary buffer first, followed by
calling INSERT-REFERENCE for each element in the list returned by
CHECKER-FUNCTION."
  `(defun ,name (&optional silent)
     ,(concat
       docstring
       "\n\nIf SILENT is non-nil, do not message anything when no
such references found.")
     (interactive "P")
     (when (not (memq major-mode '(haddock-mode gfm-mode)))
       (user-error "Not available in current mode"))
     (let ((oldbuf (current-buffer))
           (refs (,checker-function))
           (refbuf (,buffer-function)))
       (if (null refs)
           (progn
             (when (not silent)
               (message ,none-message))
             (kill-buffer refbuf))
         (with-current-buffer refbuf
           (insert ,buffer-header)
           (dolist (ref refs)
             (,insert-reference ref oldbuf))
           (view-buffer-other-window refbuf)
           (goto-char (point-min))
           (forward-line 2))))))

(defun-haddock-ref-checker
  haddock-check-refs
  "Show all undefined Haddock references in current `haddock-mode' buffer.

Links which have empty reference definitions are considered to be
defined."
  haddock-get-undefined-refs
  haddock-reference-check-buffer
  "No undefined references found"
  "The following references are undefined:\n\n"
  haddock-insert-undefined-reference-button)


(defun-haddock-ref-checker
  haddock-unused-refs
  "Show all unused Haddock references in current `haddock-mode' buffer."
  haddock-get-unused-refs
  haddock-unused-references-buffer
  "No unused references found"
  "The following references are unused:\n\n"
  haddock-insert-unused-reference-button)



;;; Lists =====================================================================

(defun haddock-insert-list-item (&optional arg)
  "Insert a new list item.
If the point is inside unordered list, insert a bullet mark.  If
the point is inside ordered list, insert the next number followed
by a period.  Use the previous list item to determine the amount
of whitespace to place before and after list markers.

With a \\[universal-argument] prefix (i.e., when ARG is (4)),
decrease the indentation by one level.

With two \\[universal-argument] prefixes (i.e., when ARG is (16)),
increase the indentation by one level."
  (interactive "p")
  (let (bounds cur-indent marker indent new-indent new-loc)
    (save-match-data
      ;; Look for a list item on current or previous non-blank line
      (save-excursion
        (while (and (not (setq bounds (haddock-cur-list-item-bounds)))
                    (not (bobp))
                    (haddock-cur-line-blank-p))
          (forward-line -1)))
      (when bounds
        (cond ((save-excursion
                 (skip-chars-backward " \t")
                 (looking-at-p haddock-regex-list))
               (beginning-of-line)
               (insert "\n")
               (forward-line -1))
              ((not (haddock-cur-line-blank-p))
               (newline)))
        (setq new-loc (point)))
      ;; Look ahead for a list item on next non-blank line
      (unless bounds
        (save-excursion
          (while (and (null bounds)
                      (not (eobp))
                      (haddock-cur-line-blank-p))
            (forward-line)
            (setq bounds (haddock-cur-list-item-bounds))))
        (when bounds
          (setq new-loc (point))
          (unless (haddock-cur-line-blank-p)
            (newline))))
      (if (not bounds)
          ;; When not in a list, start a new unordered one
          (progn
            (unless (haddock-cur-line-blank-p)
              (insert "\n"))
            (insert haddock-unordered-list-item-prefix))
        ;; Compute indentation and marker for new list item
        (setq cur-indent (nth 2 bounds))
        (setq marker (nth 4 bounds))
        ;; If current item is a GFM checkbox, insert new unchecked checkbox.
        (when (nth 5 bounds)
          (setq marker
                (concat marker
                        (replace-regexp-in-string "[Xx]" " " (nth 5 bounds)))))
        (cond
         ;; Dedent: decrement indentation, find previous marker.
         ((= arg 4)
          (setq indent (max (- cur-indent 4) 0))
          (let ((prev-bounds
                 (save-excursion
                   (goto-char (nth 0 bounds))
                   (when (haddock-up-list)
                     (haddock-cur-list-item-bounds)))))
            (when prev-bounds
              (setq marker (nth 4 prev-bounds)))))
         ;; Indent: increment indentation by 4, use same marker.
         ((= arg 16) (setq indent (+ cur-indent 4)))
         ;; Same level: keep current indentation and marker.
         (t (setq indent cur-indent)))
        (setq new-indent (make-string indent 32))
        (goto-char new-loc)
        (cond
         ;; Ordered list
         ((string-match-p "[0-9]" marker)
          (if (= arg 16) ;; starting a new column indented one more level
              (insert (concat new-indent "1. "))
            ;; Don't use previous match-data
            (set-match-data nil)
            ;; travel up to the last item and pick the correct number.  If
            ;; the argument was nil, "new-indent = cur-indent" is the same,
            ;; so we don't need special treatment. Neat.
            (save-excursion
              (while (and (not (looking-at (concat new-indent "\\([0-9]+\\)\\(\\.[ \t]*\\)")))
                          (>= (forward-line -1) 0))))
            (let* ((old-prefix (match-string 1))
                   (old-spacing (match-string 2))
                   (new-prefix (if old-prefix
                                   (int-to-string (1+ (string-to-number old-prefix)))
                                 "1"))
                   (space-adjust (- (length old-prefix) (length new-prefix)))
                   (new-spacing (if (and (match-string 2)
                                         (not (string-match-p "\t" old-spacing))
                                         (< space-adjust 0)
                                         (> space-adjust (- 1 (length (match-string 2)))))
                                    (substring (match-string 2) 0 space-adjust)
                                  (or old-spacing ". "))))
              (insert (concat new-indent new-prefix new-spacing)))))
         ;; Unordered list, GFM task list, or ordered list with hash mark
         ((string-match-p "[\\*\\+-]\\|#\\." marker)
          (insert new-indent marker))))
      ;; Propertize the newly inserted list item now
      (haddock-syntax-propertize-list-items (point-at-bol) (point-at-eol)))))

(defun haddock-move-list-item-up ()
  "Move the current list item up in the list when possible.
In nested lists, move child items with the parent item."
  (interactive)
  (let (cur prev old)
    (when (setq cur (haddock-cur-list-item-bounds))
      (setq old (point))
      (goto-char (nth 0 cur))
      (if (haddock-prev-list-item (nth 3 cur))
          (progn
            (setq prev (haddock-cur-list-item-bounds))
            (condition-case nil
                (progn
                  (transpose-regions (nth 0 prev) (nth 1 prev)
                                     (nth 0 cur) (nth 1 cur) t)
                  (goto-char (+ (nth 0 prev) (- old (nth 0 cur)))))
              ;; Catch error in case regions overlap.
              (error (goto-char old))))
        (goto-char old)))))

(defun haddock-move-list-item-down ()
  "Move the current list item down in the list when possible.
In nested lists, move child items with the parent item."
  (interactive)
  (let (cur next old)
    (when (setq cur (haddock-cur-list-item-bounds))
      (setq old (point))
      (if (haddock-next-list-item (nth 3 cur))
          (progn
            (setq next (haddock-cur-list-item-bounds))
            (condition-case nil
                (progn
                  (transpose-regions (nth 0 cur) (nth 1 cur)
                                     (nth 0 next) (nth 1 next) nil)
                  (goto-char (+ old (- (nth 1 next) (nth 1 cur)))))
              ;; Catch error in case regions overlap.
              (error (goto-char old))))
        (goto-char old)))))

(defun haddock-demote-list-item (&optional bounds)
  "Indent (or demote) the current list item.
Optionally, BOUNDS of the current list item may be provided if available.
In nested lists, demote child items as well."
  (interactive)
  (when (or bounds (setq bounds (haddock-cur-list-item-bounds)))
    (save-excursion
      (let* ((item-start (set-marker (make-marker) (nth 0 bounds)))
             (item-end (set-marker (make-marker) (nth 1 bounds)))
             (list-start (progn (haddock-beginning-of-list)
                                (set-marker (make-marker) (point))))
             (list-end (progn (haddock-end-of-list)
                              (set-marker (make-marker) (point)))))
        (goto-char item-start)
        (while (< (point) item-end)
          (unless (haddock-cur-line-blank-p)
            (insert (make-string haddock-list-indent-width ? )))
          (forward-line))
        (haddock-syntax-propertize-list-items list-start list-end)))))

(defun haddock-promote-list-item (&optional bounds)
  "Unindent (or promote) the current list item.
Optionally, BOUNDS of the current list item may be provided if available.
In nested lists, demote child items as well."
  (interactive)
  (when (or bounds (setq bounds (haddock-cur-list-item-bounds)))
    (save-excursion
      (save-match-data
        (let ((item-start (set-marker (make-marker) (nth 0 bounds)))
              (item-end (set-marker (make-marker) (nth 1 bounds)))
              (list-start (progn (haddock-beginning-of-list)
                                 (set-marker (make-marker) (point))))
              (list-end (progn (haddock-end-of-list)
                               (set-marker (make-marker) (point))))
              num regexp)
          (goto-char item-start)
          (when (looking-at (format "^[ ]\\{1,%d\\}"
                                    haddock-list-indent-width))
            (setq num (- (match-end 0) (match-beginning 0)))
            (setq regexp (format "^[ ]\\{1,%d\\}" num))
            (while (and (< (point) item-end)
                        (re-search-forward regexp item-end t))
              (replace-match "" nil nil)
              (forward-line))
            (haddock-syntax-propertize-list-items list-start list-end)))))))

(defun haddock-cleanup-list-numbers-level (&optional pfx)
  "Update the numbering for level PFX (as a string of spaces).

Assume that the previously found match was for a numbered item in
a list."
  (let ((cpfx pfx)
        (idx 0)
        (continue t)
        (step t)
        (sep nil))
    (while (and continue (not (eobp)))
      (setq step t)
      (cond
       ((looking-at "^\\([\s-]*\\)[0-9]+\\. ")
        (setq cpfx (match-string-no-properties 1))
        (cond
         ((string= cpfx pfx)
          (save-excursion
            (replace-match
             (concat pfx (number-to-string (setq idx (1+ idx))) ". ")))
          (setq sep nil))
         ;; indented a level
         ((string< pfx cpfx)
          (setq sep (haddock-cleanup-list-numbers-level cpfx))
          (setq step nil))
         ;; exit the loop
         (t
          (setq step nil)
          (setq continue nil))))

       ((looking-at "^\\([\s-]*\\)[^ \t\n\r].*$")
        (setq cpfx (match-string-no-properties 1))
        (cond
         ;; reset if separated before
         ((string= cpfx pfx) (when sep (setq idx 0)))
         ((string< cpfx pfx)
          (setq step nil)
          (setq continue nil))))
       (t (setq sep t)))

      (when step
        (beginning-of-line)
        (setq continue (= (forward-line) 0))))
    sep))

(defun haddock-cleanup-list-numbers ()
  "Update the numbering of ordered lists."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (haddock-cleanup-list-numbers-level "")))


;;; Movement ==================================================================

(defun haddock-beginning-of-defun (&optional arg)
  "`beginning-of-defun-function' for Haddock.
This is used to find the beginning of the defun and should behave
like ‘beginning-of-defun’, returning non-nil if it found the
beginning of a defun.  It moves the point backward, right before a
heading which defines a defun.  When ARG is non-nil, repeat that
many times.  When ARG is negative, move forward to the ARG-th
following section."
  (or arg (setq arg 1))
  (when (< arg 0) (end-of-line))
  ;; Adjust position for setext headings.
  (when (and (thing-at-point-looking-at haddock-regex-header-setext)
             (not (= (point) (match-beginning 0)))
             (not (haddock-code-block-at-point-p)))
    (goto-char (match-end 0)))
  (let (found)
    ;; Move backward with positive argument.
    (while (and (not (bobp)) (> arg 0))
      (setq found nil)
      (while (and (not found)
                  (not (bobp))
                  (re-search-backward haddock-regex-header nil 'move))
        (when (not (haddock-code-block-at-pos (match-beginning 0))))
        (setq found (match-beginning 0)))
      (setq arg (1- arg)))
    ;; Move forward with negative argument.
    (while (and (not (eobp)) (< arg 0))
      (setq found nil)
      (while (and (not found)
                  (not (eobp))
                  (re-search-forward haddock-regex-header nil 'move))
        (when (not (haddock-code-block-at-pos (match-beginning 0))))
        (setq found (match-beginning 0)))
      (setq arg (1+ arg)))
    (when found
      (beginning-of-line)
      t)))

(defun haddock-end-of-defun ()
  "`end-of-defun-function’ for Haddock.
This is used to find the end of the defun at point.
It is called with no argument, right after calling ‘beginning-of-defun-raw’,
so it can assume that point is at the beginning of the defun body.
It should move point to the first position after the defun."
  (or (eobp) (forward-char 1))
  (let (found)
    (while (and (not found)
                (not (eobp))
                (re-search-forward haddock-regex-header nil 'move))
      (when (not (haddock-code-block-at-pos (match-beginning 0)))
        (setq found (match-beginning 0))))
    (when found
      (goto-char found)
      (skip-syntax-backward "-"))))

(make-obsolete 'haddock-beginning-of-block 'haddock-beginning-of-text-block "v2.2")

(defun haddock-beginning-of-text-block ()
  "Move backward to previous beginning of a plain text block.
This function simply looks for blank lines without considering
the surrounding context in light of Haddock syntax.  For that, see
`haddock-backward-block'."
  (interactive)
  (let ((start (point)))
    (if (re-search-backward haddock-regex-block-separator nil t)
        (goto-char (match-end 0))
      (goto-char (point-min)))
    (when (and (= start (point)) (not (bobp)))
      (forward-line -1)
      (if (re-search-backward haddock-regex-block-separator nil t)
          (goto-char (match-end 0))
        (goto-char (point-min))))))

(make-obsolete 'haddock-end-of-block 'haddock-end-of-text-block "v2.2")

(defun haddock-end-of-text-block ()
  "Move forward to next beginning of a plain text block.
This function simply looks for blank lines without considering
the surrounding context in light of Haddock syntax.  For that, see
`haddock-forward-block'."
  (interactive)
  (beginning-of-line)
  (skip-chars-forward " \t\n")
  (when (= (point) (point-min))
    (forward-char))
  (if (re-search-forward haddock-regex-block-separator nil t)
      (goto-char (match-end 0))
    (goto-char (point-max)))
  (skip-chars-backward " \t\n")
  (forward-line))

(defun haddock-backward-paragraph (&optional arg)
  "Move the point to the start of the current paragraph.
With argument ARG, do it ARG times; a negative argument ARG = -N
means move forward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (haddock-forward-paragraph (- arg))
    (dotimes (_ arg)
      ;; Skip over whitespace in between paragraphs when moving backward.
      (skip-chars-backward " \t\n")
      (beginning-of-line)
      ;; Skip over code block endings.
      (when (haddock-range-properties-exist
            (point-at-bol) (point-at-eol)
            '(haddock-gfm-block-end
              haddock-tilde-fence-end))
        (forward-line -1))
      ;; Skip over blank lines inside blockquotes.
      (while (and (not (eobp))
                  (looking-at haddock-regex-blockquote)
                  (= (length (match-string 3)) 0))
        (forward-line -1))
      ;; Proceed forward based on the type of block of paragraph.
      (let (bounds skip)
        (cond
         ;; Blockquotes
         ((looking-at haddock-regex-blockquote)
          (while (and (not (bobp))
                      (looking-at haddock-regex-blockquote)
                      (> (length (match-string 3)) 0)) ;; not blank
            (forward-line -1))
          (forward-line))
         ;; List items
         ((setq bounds (haddock-cur-list-item-bounds))
          (goto-char (nth 0 bounds)))
         ;; Other
         (t
          (while (and (not (bobp))
                      (not skip)
                      (not (haddock-cur-line-blank-p))
                      (not (looking-at haddock-regex-blockquote))
                      (not (haddock-range-properties-exist
                            (point-at-bol) (point-at-eol)
                            '(haddock-gfm-block-end
                              haddock-tilde-fence-end))))
            (setq skip (haddock-range-properties-exist
                            (point-at-bol) (point-at-eol)
                            '(haddock-gfm-block-begin
                              haddock-tilde-fence-begin)))
            (forward-line -1))
          (unless (bobp)
            (forward-line 1))))))))

(defun haddock-forward-paragraph (&optional arg)
  "Move forward to the next end of a paragraph.
With argument ARG, do it ARG times; a negative argument ARG = -N
means move backward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (haddock-backward-paragraph (- arg))
    (dotimes (_ arg)
      ;; Skip whitespace in between paragraphs.
      (when (haddock-cur-line-blank-p)
        (skip-syntax-forward "-")
        (beginning-of-line))
      ;; Proceed forward based on the type of block.
      (let (bounds skip)
        (cond
         ;; Blockquotes
         ((looking-at haddock-regex-blockquote)
          ;; Skip over blank lines inside blockquotes.
          (while (and (not (eobp))
                      (looking-at haddock-regex-blockquote)
                      (= (length (match-string 3)) 0))
            (forward-line))
          ;; Move to end of quoted text block
          (while (and (not (eobp))
                      (looking-at haddock-regex-blockquote)
                      (> (length (match-string 3)) 0)) ;; not blank
            (forward-line)))
         ;; List items
         ((and (haddock-cur-list-item-bounds)
               (setq bounds (haddock-next-list-item-bounds)))
          (goto-char (nth 0 bounds)))
         ;; Other
         (t
          (forward-line)
          (while (and (not (eobp))
                      (not skip)
                      (not (haddock-cur-line-blank-p))
                      (not (looking-at haddock-regex-blockquote))
                      (not (haddock-range-properties-exist
                            (point-at-bol) (point-at-eol)
                            '(haddock-gfm-block-begin
                              haddock-tilde-fence-begin))))
            (setq skip (haddock-range-properties-exist
                        (point-at-bol) (point-at-eol)
                        '(haddock-gfm-block-end
                          haddock-tilde-fence-end)))
            (forward-line))))))))

(defun haddock-backward-block (&optional arg)
  "Move the point to the start of the current Haddock block.
Moves across complete code blocks, list items, and blockquotes,
but otherwise stops at blank lines, headers, and horizontal
rules.  With argument ARG, do it ARG times; a negative argument
ARG = -N means move forward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (haddock-forward-block (- arg))
    (dotimes (_ arg)
      ;; Skip over whitespace in between blocks when moving backward,
      ;; unless at a block boundary with no whitespace.
      (skip-syntax-backward "-")
      (beginning-of-line)
      ;; Proceed forward based on the type of block.
      (cond
       ;; Code blocks
       ((and (haddock-code-block-at-pos (point)) ;; this line
             (haddock-code-block-at-pos (point-at-bol 0))) ;; previous line
        (forward-line -1)
        (while (and (haddock-code-block-at-point-p) (not (bobp)))
          (forward-line -1))
        (forward-line))
       ;; Headings
       ((haddock-heading-at-point)
        (goto-char (match-beginning 0)))
       ;; Horizontal rules
       ((looking-at haddock-regex-hr))
       ;; Blockquotes
       ((looking-at haddock-regex-blockquote)
        (forward-line -1)
        (while (and (looking-at haddock-regex-blockquote)
                    (not (bobp)))
          (forward-line -1))
        (forward-line))
       ;; List items
       ((haddock-cur-list-item-bounds)
        (haddock-beginning-of-list))
       ;; Other
       (t
        ;; Move forward in case it is a one line regular paragraph.
        (unless (haddock-next-line-blank-p)
          (forward-line))
        (unless (haddock-prev-line-blank-p)
          (haddock-backward-paragraph)))))))

(defun haddock-forward-block (&optional arg)
  "Move forward to the next end of a Haddock block.
Moves across complete code blocks, list items, and blockquotes,
but otherwise stops at blank lines, headers, and horizontal
rules.  With argument ARG, do it ARG times; a negative argument
ARG = -N means move backward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (haddock-backward-block (- arg))
    (dotimes (_ arg)
      ;; Skip over whitespace in between blocks when moving forward.
      (if (haddock-cur-line-blank-p)
          (skip-syntax-forward "-")
        (beginning-of-line))
      ;; Proceed forward based on the type of block.
      (cond
       ;; Code blocks
       ((haddock-code-block-at-point-p)
        (forward-line)
        (while (and (haddock-code-block-at-point-p) (not (eobp)))
          (forward-line)))
       ;; Headings
       ((looking-at haddock-regex-header)
        (goto-char (or (match-end 4) (match-end 2) (match-end 3)))
        (forward-line))
       ;; Horizontal rules
       ((looking-at haddock-regex-hr)
        (forward-line))
       ;; Blockquotes
       ((looking-at haddock-regex-blockquote)
        (forward-line)
        (while (and (looking-at haddock-regex-blockquote) (not (eobp)))
          (forward-line)))
       ;; List items
       ((haddock-cur-list-item-bounds)
        (haddock-end-of-list)
        (forward-line))
       ;; Other
       (t (haddock-forward-paragraph))))
    (skip-syntax-backward "-")
    (unless (eobp)
      (forward-char 1))))

(defun haddock-backward-page (&optional count)
  "Move backward to boundary of the current toplevel section.
With COUNT, repeat, or go forward if negative."
  (interactive "p")
  (or count (setq count 1))
  (if (< count 0)
      (haddock-forward-page (- count))
    (skip-syntax-backward "-")
    (or (haddock-back-to-heading-over-code-block t t)
        (goto-char (point-min)))
    (when (looking-at haddock-regex-header)
      (let ((level (haddock-outline-level)))
        (when (> level 1) (haddock-up-heading level))
        (when (> count 1)
          (condition-case nil
              (haddock-backward-same-level (1- count))
            (error (goto-char (point-min)))))))))

(defun haddock-forward-page (&optional count)
  "Move forward to boundary of the current toplevel section.
With COUNT, repeat, or go backward if negative."
  (interactive "p")
  (or count (setq count 1))
  (if (< count 0)
      (haddock-backward-page (- count))
    (if (haddock-back-to-heading-over-code-block t t)
        (let ((level (haddock-outline-level)))
          (when (> level 1) (haddock-up-heading level))
          (condition-case nil
              (haddock-forward-same-level count)
            (error (goto-char (point-max)))))
      (haddock-next-visible-heading 1))))

(defun haddock-next-link ()
  "Jump to next inline, reference, or wiki link.
If successful, return point.  Otherwise, return nil.
See `haddock-wiki-link-p' and `haddock-previous-wiki-link'."
  (interactive)
  (let ((opoint (point)))
    (when (or (haddock-link-p) (haddock-wiki-link-p))
      ;; At a link already, move past it.
      (goto-char (+ (match-end 0) 1)))
    ;; Search for the next wiki link and move to the beginning.
    (while (and (re-search-forward (haddock-make-regex-link-generic) nil t)
                (haddock-code-block-at-point-p)
                (< (point) (point-max))))
    (if (and (not (eq (point) opoint))
             (or (haddock-link-p) (haddock-wiki-link-p)))
        ;; Group 1 will move past non-escape character in wiki link regexp.
        ;; Go to beginning of group zero for all other link types.
        (goto-char (or (match-beginning 1) (match-beginning 0)))
      (goto-char opoint)
      nil)))

(defun haddock-previous-link ()
  "Jump to previous wiki link.
If successful, return point.  Otherwise, return nil.
See `haddock-wiki-link-p' and `haddock-next-wiki-link'."
  (interactive)
  (let ((opoint (point)))
    (while (and (re-search-backward (haddock-make-regex-link-generic) nil t)
                (haddock-code-block-at-point-p)
                (> (point) (point-min))))
    (if (and (not (eq (point) opoint))
             (or (haddock-link-p) (haddock-wiki-link-p)))
        (goto-char (or (match-beginning 1) (match-beginning 0)))
      (goto-char opoint)
      nil)))


;;; Outline ===================================================================

(defun haddock-move-heading-common (move-fn &optional arg adjust)
  "Wrapper for `outline-mode' functions to skip false positives.
MOVE-FN is a function and ARG is its argument. For example,
headings inside preformatted code blocks may match
`outline-regexp' but should not be considered as headings.
When ADJUST is non-nil, adjust the point for interactive calls
to avoid leaving the point at invisible markup.  This adjustment
generally should only be done for interactive calls, since other
functions may expect the point to be at the beginning of the
regular expression."
  (let ((prev -1) (start (point)))
    (if arg (funcall move-fn arg) (funcall move-fn))
    (while (and (/= prev (point)) (haddock-code-block-at-point-p))
      (setq prev (point))
      (if arg (funcall move-fn arg) (funcall move-fn)))
    ;; Adjust point for setext headings and invisible text.
    (save-match-data
      (when (and adjust (thing-at-point-looking-at haddock-regex-header))
        (if haddock-hide-markup
            ;; Move to beginning of heading text if markup is hidden.
            (goto-char (or (match-beginning 1) (match-beginning 5)))
          ;; Move to beginning of markup otherwise.
          (goto-char (or (match-beginning 1) (match-beginning 4))))))
    (if (= (point) start) nil (point))))

(defun haddock-next-visible-heading (arg)
  "Move to the next visible heading line of any level.
With argument, repeats or can move backward if negative. ARG is
passed to `outline-next-visible-heading'."
  (interactive "p")
  (haddock-move-heading-common #'outline-next-visible-heading arg 'adjust))

(defun haddock-previous-visible-heading (arg)
  "Move to the previous visible heading line of any level.
With argument, repeats or can move backward if negative. ARG is
passed to `outline-previous-visible-heading'."
  (interactive "p")
  (haddock-move-heading-common #'outline-previous-visible-heading arg 'adjust))

(defun haddock-next-heading ()
  "Move to the next heading line of any level."
  (haddock-move-heading-common #'outline-next-heading))

(defun haddock-previous-heading ()
  "Move to the previous heading line of any level."
  (haddock-move-heading-common #'outline-previous-heading))

(defun haddock-back-to-heading-over-code-block (&optional invisible-ok no-error)
  "Move back to the beginning of the previous heading.
Returns t if the point is at a heading, the location if a heading
was found, and nil otherwise.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil.  Throw an error if there is no previous heading unless
NO-ERROR is non-nil.
Leaves match data intact for `haddock-regex-header'."
  (beginning-of-line)
  (or (and (haddock-heading-at-point)
           (not (haddock-code-block-at-point-p)))
      (let (found)
        (save-excursion
          (while (and (not found)
                      (re-search-backward haddock-regex-header nil t))
            (when (and (or invisible-ok (not (outline-invisible-p)))
                       (not (haddock-code-block-at-point-p)))
              (setq found (point))))
          (if (not found)
              (unless no-error (user-error "Before first heading"))
            (setq found (point))))
        (when found (goto-char found)))))

(defun haddock-forward-same-level (arg)
  "Move forward to the ARG'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (haddock-back-to-heading-over-code-block)
  (haddock-move-heading-common #'outline-forward-same-level arg 'adjust))

(defun haddock-backward-same-level (arg)
  "Move backward to the ARG'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (haddock-back-to-heading-over-code-block)
  (while (> arg 0)
    (let ((point-to-move-to
           (save-excursion
             (haddock-move-heading-common #'outline-get-last-sibling nil 'adjust))))
      (if point-to-move-to
          (progn
            (goto-char point-to-move-to)
            (setq arg (1- arg)))
        (user-error "No previous same-level heading")))))

(defun haddock-up-heading (arg)
  "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels."
  (interactive "p")
  (and (called-interactively-p 'any)
       (not (eq last-command 'haddock-up-heading)) (push-mark))
  (haddock-move-heading-common #'outline-up-heading arg 'adjust))

(defun haddock-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (haddock-move-heading-common #'outline-back-to-heading invisible-ok))

(defalias 'haddock-end-of-heading 'outline-end-of-heading)

(defun haddock-on-heading-p ()
  "Return non-nil if point is on a heading line."
  (get-text-property (point-at-bol) 'haddock-heading))

(defun haddock-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil.
Derived from `org-end-of-subtree'."
  (haddock-back-to-heading invisible-OK)
  (let ((first t)
        (level (haddock-outline-level)))
    (while (and (not (eobp))
                (or first (> (haddock-outline-level) level)))
      (setq first nil)
      (haddock-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

(defun haddock-outline-fix-visibility ()
  "Hide any false positive headings that should not be shown.
For example, headings inside preformatted code blocks may match
`outline-regexp' but should not be shown as headings when cycling.
Also, the ending --- line in metadata blocks appears to be a
setext header, but should not be folded."
  (save-excursion
    (goto-char (point-min))
    ;; Unhide any false positives in metadata blocks
    (when (haddock-text-property-at-point 'haddock-yaml-metadata-begin)
      (let ((body (progn (forward-line)
                         (haddock-text-property-at-point
                          'haddock-yaml-metadata-section))))
        (when body
          (let ((end (progn (goto-char (cl-second body))
                            (haddock-text-property-at-point
                             'haddock-yaml-metadata-end))))
            (outline-flag-region (point-min) (1+ (cl-second end)) nil)))))
    ;; Hide any false positives in code blocks
    (unless (outline-on-heading-p)
      (outline-next-visible-heading 1))
    (while (< (point) (point-max))
      (when (haddock-code-block-at-point-p)
        (outline-flag-region (1- (point-at-bol)) (point-at-eol) t))
      (outline-next-visible-heading 1))))

(defvar haddock-cycle-global-status 1)
(defvar haddock-cycle-subtree-status nil)

(defun haddock-next-preface ()
  (let (finish)
    (while (and (not finish) (re-search-forward (concat "\n\\(?:" outline-regexp "\\)")
                                                nil 'move))
      (unless (haddock-code-block-at-point-p)
        (goto-char (match-beginning 0))
        (setq finish t))))
  (when (and (bolp) (or outline-blank-line (eobp)) (not (bobp)))
    (forward-char -1)))

(defun haddock-show-entry ()
  (save-excursion
    (outline-back-to-heading t)
    (outline-flag-region (1- (point))
                         (progn
                           (haddock-next-preface)
                           (if (= 1 (- (point-max) (point)))
                               (point-max)
                             (point)))
                         nil)))

;; This function was originally derived from `org-cycle' from org.el.
(defun haddock-cycle (&optional arg)
  "Visibility cycling for Haddock mode.
If ARG is t, perform global visibility cycling.  If the point is
at an atx-style header, cycle visibility of the corresponding
subtree.  Otherwise, indent the current line or insert a tab,
as appropriate, by calling `indent-for-tab-command'."
  (interactive "P")
  (cond

   ;; Global cycling
   ((eq arg t)
    (cond
     ;; Move from overview to contents
     ((and (eq last-command this-command)
           (eq haddock-cycle-global-status 2))
      (haddock-hide-sublevels 1)
      (message "CONTENTS")
      (setq haddock-cycle-global-status 3)
      (haddock-outline-fix-visibility))
     ;; Move from contents to all
     ((and (eq last-command this-command)
           (eq haddock-cycle-global-status 3))
      (haddock-show-all)
      (message "SHOW ALL")
      (setq haddock-cycle-global-status 1))
     ;; Defaults to overview
     (t
      (haddock-hide-body)
      (message "OVERVIEW")
      (setq haddock-cycle-global-status 2)
      (haddock-outline-fix-visibility))))

   ;; At a heading: rotate between three different views
   ((save-excursion (beginning-of-line 1) (haddock-on-heading-p))
    (haddock-back-to-heading)
    (let ((goal-column 0) eoh eol eos)
      ;; Determine boundaries
      (save-excursion
        (haddock-back-to-heading)
        (save-excursion
          (beginning-of-line 2)
          (while (and (not (eobp)) ;; this is like `next-line'
                      (get-char-property (1- (point)) 'invisible))
            (beginning-of-line 2)) (setq eol (point)))
        (haddock-end-of-heading)   (setq eoh (point))
        (haddock-end-of-subtree t)
        (skip-chars-forward " \t\n")
        (beginning-of-line 1) ; in case this is an item
        (setq eos (1- (point))))
      ;; Find out what to do next and set `this-command'
      (cond
       ;; Nothing is hidden behind this heading
       ((= eos eoh)
        (message "EMPTY ENTRY")
        (setq haddock-cycle-subtree-status nil))
       ;; Entire subtree is hidden in one line: open it
       ((>= eol eos)
        (haddock-show-entry)
        (haddock-show-children)
        (message "CHILDREN")
        (setq haddock-cycle-subtree-status 'children))
       ;; We just showed the children, now show everything.
       ((and (eq last-command this-command)
             (eq haddock-cycle-subtree-status 'children))
        (haddock-show-subtree)
        (message "SUBTREE")
        (setq haddock-cycle-subtree-status 'subtree))
       ;; Default action: hide the subtree.
       (t
        (haddock-hide-subtree)
        (message "FOLDED")
        (setq haddock-cycle-subtree-status 'folded)))))

   ;; In a table, move forward by one cell
   ((haddock-table-at-point-p)
    (call-interactively #'haddock-table-forward-cell))

   ;; Otherwise, indent as appropriate
   (t
    (indent-for-tab-command))))

(defun haddock-shifttab ()
  "Handle S-TAB keybinding based on context.
When in a table, move backward one cell.
Otherwise, cycle global heading visibility by calling
`haddock-cycle' with argument t."
  (interactive)
  (cond ((haddock-table-at-point-p)
         (call-interactively #'haddock-table-backward-cell))
        (t (haddock-cycle t))))

(defun haddock-outline-level ()
  "Return the depth to which a statement is nested in the outline."
  (cond
   ((and (match-beginning 0)
         (haddock-code-block-at-pos (match-beginning 0)))
    7) ;; Only 6 header levels are defined.
   ((match-end 2) 1)
   ((match-end 3) 2)
   ((match-end 4)
    (length (haddock-trim-whitespace (match-string-no-properties 4))))))

(defun haddock-promote-subtree (&optional arg)
  "Promote the current subtree of ATX headings.
Note that Haddock does not support heading levels higher than
six and therefore level-six headings will not be promoted
further. If ARG is non-nil promote the heading, otherwise
demote."
  (interactive "*P")
  (save-excursion
    (when (and (or (thing-at-point-looking-at haddock-regex-header-atx)
                   (re-search-backward haddock-regex-header-atx nil t))
               (not (haddock-code-block-at-point-p)))
      (let ((level (length (match-string 1)))
            (promote-or-demote (if arg 1 -1))
            (remove 't))
        (haddock-cycle-atx promote-or-demote remove)
        (catch 'end-of-subtree
          (while (and (haddock-next-heading)
                      (looking-at haddock-regex-header-atx))
            ;; Exit if this not a higher level heading; promote otherwise.
            (if (and (looking-at haddock-regex-header-atx)
                     (<= (length (match-string-no-properties 1)) level))
                (throw 'end-of-subtree nil)
              (haddock-cycle-atx promote-or-demote remove))))))))

(defun haddock-demote-subtree ()
  "Demote the current subtree of ATX headings."
  (interactive)
  (haddock-promote-subtree t))

(defun haddock-move-subtree-up ()
  "Move the current subtree of ATX headings up."
  (interactive)
  (outline-move-subtree-up 1))

(defun haddock-move-subtree-down ()
  "Move the current subtree of ATX headings down."
  (interactive)
  (outline-move-subtree-down 1))

(defun haddock-outline-next ()
  "Move to next list item, when in a list, or next visible heading."
  (interactive)
  (let ((bounds (haddock-next-list-item-bounds)))
    (if bounds
        (goto-char (nth 0 bounds))
      (haddock-next-visible-heading 1))))

(defun haddock-outline-previous ()
  "Move to previous list item, when in a list, or previous visible heading."
  (interactive)
  (let ((bounds (haddock-prev-list-item-bounds)))
    (if bounds
        (goto-char (nth 0 bounds))
      (haddock-previous-visible-heading 1))))

(defun haddock-outline-next-same-level ()
  "Move to next list item or heading of same level."
  (interactive)
  (let ((bounds (haddock-cur-list-item-bounds)))
    (if bounds
        (haddock-next-list-item (nth 3 bounds))
      (haddock-forward-same-level 1))))

(defun haddock-outline-previous-same-level ()
  "Move to previous list item or heading of same level."
  (interactive)
  (let ((bounds (haddock-cur-list-item-bounds)))
    (if bounds
        (haddock-prev-list-item (nth 3 bounds))
      (haddock-backward-same-level 1))))

(defun haddock-outline-up ()
  "Move to previous list item, when in a list, or next heading."
  (interactive)
  (unless (haddock-up-list)
    (haddock-up-heading 1)))


;;; Marking and Narrowing =====================================================

(defun haddock-mark-paragraph ()
  "Put mark at end of this block, point at beginning.
The block marked is the one that contains point or follows point.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next block after the
ones already marked."
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (haddock-forward-paragraph)
         (point)))
    (let ((beginning-of-defun-function 'haddock-backward-paragraph)
          (end-of-defun-function 'haddock-forward-paragraph))
      (mark-defun))))

(defun haddock-mark-block ()
  "Put mark at end of this block, point at beginning.
The block marked is the one that contains point or follows point.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next block after the
ones already marked."
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (haddock-forward-block)
         (point)))
    (let ((beginning-of-defun-function 'haddock-backward-block)
          (end-of-defun-function 'haddock-forward-block))
      (mark-defun))))

(defun haddock-narrow-to-block ()
  "Make text outside current block invisible.
The current block is the one that contains point or follows point."
  (interactive)
  (let ((beginning-of-defun-function 'haddock-backward-block)
        (end-of-defun-function 'haddock-forward-block))
    (narrow-to-defun)))

(defun haddock-mark-text-block ()
  "Put mark at end of this plain text block, point at beginning.
The block marked is the one that contains point or follows point.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next block after the
ones already marked."
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (haddock-end-of-text-block)
         (point)))
    (let ((beginning-of-defun-function 'haddock-beginning-of-text-block)
          (end-of-defun-function 'haddock-end-of-text-block))
      (mark-defun))))

(defun haddock-mark-page ()
  "Put mark at end of this top level section, point at beginning.
The top level section marked is the one that contains point or
follows point.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next page after the
ones already marked."
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (haddock-forward-page)
         (point)))
    (let ((beginning-of-defun-function 'haddock-backward-page)
          (end-of-defun-function 'haddock-forward-page))
      (mark-defun))))

(defun haddock-narrow-to-page ()
  "Make text outside current top level section invisible.
The current section is the one that contains point or follows point."
  (interactive)
  (let ((beginning-of-defun-function 'haddock-backward-page)
        (end-of-defun-function 'haddock-forward-page))
    (narrow-to-defun)))

(defun haddock-mark-subtree ()
  "Mark the current subtree.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (let ((beg))
    (if (haddock-heading-at-point)
        (beginning-of-line)
      (haddock-previous-visible-heading 1))
    (setq beg (point))
    (haddock-end-of-subtree)
    (push-mark (point) nil t)
    (goto-char beg)))

(defun haddock-narrow-to-subtree ()
  "Narrow buffer to the current subtree."
  (interactive)
  (save-excursion
    (save-match-data
      (narrow-to-region
       (progn (haddock-back-to-heading-over-code-block t) (point))
       (progn (haddock-end-of-subtree)
          (if (and (haddock-heading-at-point) (not (eobp)))
          (backward-char 1))
          (point))))))


;;; Generic Structure Editing, Completion, and Cycling Commands ===============

(defun haddock-move-up ()
  "Move thing at point up.
When in a list item, call `haddock-move-list-item-up'.
When in a table, call `haddock-table-move-row-up'.
Otherwise, move the current heading subtree up with
`haddock-move-subtree-up'."
  (interactive)
  (cond
   ((haddock-list-item-at-point-p)
    (call-interactively #'haddock-move-list-item-up))
   ((haddock-table-at-point-p)
    (call-interactively #'haddock-table-move-row-up))
   (t
    (call-interactively #'haddock-move-subtree-up))))

(defun haddock-move-down ()
  "Move thing at point down.
When in a list item, call `haddock-move-list-item-down'.
Otherwise, move the current heading subtree up with
`haddock-move-subtree-down'."
  (interactive)
  (cond
   ((haddock-list-item-at-point-p)
    (call-interactively #'haddock-move-list-item-down))
   ((haddock-table-at-point-p)
    (call-interactively #'haddock-table-move-row-down))
   (t
    (call-interactively #'haddock-move-subtree-down))))

(defun haddock-promote ()
  "Promote or move element at point to the left.
Depending on the context, this function will promote a heading or
list item at the point, move a table column to the left, or cycle
markup."
  (interactive)
  (let (bounds)
    (cond
     ;; Promote atx heading subtree
     ((thing-at-point-looking-at haddock-regex-header-atx)
      (haddock-promote-subtree))
     ;; Promote setext heading
     ((thing-at-point-looking-at haddock-regex-header-setext)
      (haddock-cycle-setext -1))
     ;; Promote horizonal rule
     ((thing-at-point-looking-at haddock-regex-hr)
      (haddock-cycle-hr -1))
     ;; Promote list item
     ((setq bounds (haddock-cur-list-item-bounds))
      (haddock-promote-list-item bounds))
     ;; Move table column to the left
     ((haddock-table-at-point-p)
      (call-interactively #'haddock-table-move-column-left))
     ;; Promote bold
     ((thing-at-point-looking-at haddock-regex-bold)
      (haddock-cycle-bold))
     ;; Promote italic
     ((thing-at-point-looking-at haddock-regex-italic)
      (haddock-cycle-italic))
     (t
      (user-error "Nothing to promote at point")))))

(defun haddock-demote ()
  "Demote or move element at point to the right.
Depending on the context, this function will demote a heading or
list item at the point, move a table column to the right, or cycle
or remove markup."
  (interactive)
  (let (bounds)
    (cond
     ;; Demote atx heading subtree
     ((thing-at-point-looking-at haddock-regex-header-atx)
      (haddock-demote-subtree))
     ;; Demote setext heading
     ((thing-at-point-looking-at haddock-regex-header-setext)
      (haddock-cycle-setext 1))
     ;; Demote horizonal rule
     ((thing-at-point-looking-at haddock-regex-hr)
      (haddock-cycle-hr 1))
     ;; Demote list item
     ((setq bounds (haddock-cur-list-item-bounds))
      (haddock-demote-list-item bounds))
     ;; Move table column to the right
     ((haddock-table-at-point-p)
      (call-interactively #'haddock-table-move-column-right))
     ;; Demote bold
     ((thing-at-point-looking-at haddock-regex-bold)
      (haddock-cycle-bold))
     ;; Demote italic
     ((thing-at-point-looking-at haddock-regex-italic)
      (haddock-cycle-italic))
     (t
      (user-error "Nothing to demote at point")))))


;;; Commands ==================================================================

(defun haddock (&optional output-buffer-name)
  "Run `haddock-command' on buffer, sending output to OUTPUT-BUFFER-NAME.
The output buffer name defaults to `haddock-output-buffer-name'.
Return the name of the output buffer used."
  (interactive)
  (save-window-excursion
    (let ((begin-region)
          (end-region))
      (if (haddock-use-region-p)
          (setq begin-region (region-beginning)
                end-region (region-end))
        (setq begin-region (point-min)
              end-region (point-max)))

      (unless output-buffer-name
        (setq output-buffer-name haddock-output-buffer-name))
      (let ((exit-code
             (cond
              ;; Handle case when `haddock-command' does not read from stdin
              ((and (stringp haddock-command) haddock-command-needs-filename)
               (if (not buffer-file-name)
                   (user-error "Must be visiting a file")
                 ;; Don’t use ‘shell-command’ because it’s not guaranteed to
                 ;; return the exit code of the process.
                 (shell-command-on-region
                  ;; Pass an empty region so that stdin is empty.
                  (point) (point)
                  (concat haddock-command " "
                          (shell-quote-argument buffer-file-name))
                  output-buffer-name)))
              ;; Pass region to `haddock-command' via stdin
              (t
               (let ((buf (get-buffer-create output-buffer-name)))
                 (with-current-buffer buf
                   (setq buffer-read-only nil)
                   (erase-buffer))
                 (if (stringp haddock-command)
                     (call-process-region begin-region end-region
                                          shell-file-name nil buf nil
                                          shell-command-switch haddock-command)
                   (funcall haddock-command begin-region end-region buf)
                   ;; If the ‘haddock-command’ function didn’t signal an
                   ;; error, assume it succeeded by binding ‘exit-code’ to 0.
                   0))))))
        ;; The exit code can be a signal description string, so don’t use ‘=’
        ;; or ‘zerop’.
        (unless (eq exit-code 0)
          (user-error "%s failed with exit code %s"
                      haddock-command exit-code))))
    output-buffer-name))

(defun haddock-standalone (&optional output-buffer-name)
  "Special function to provide standalone HTML output.
Insert the output in the buffer named OUTPUT-BUFFER-NAME."
  (interactive)
  (setq output-buffer-name (haddock output-buffer-name))
  (with-current-buffer output-buffer-name
    (set-buffer output-buffer-name)
    (unless (haddock-output-standalone-p)
      (haddock-add-xhtml-header-and-footer output-buffer-name))
    (goto-char (point-min))
    (html-mode))
  output-buffer-name)

(defun haddock-other-window (&optional output-buffer-name)
  "Run `haddock-command' on current buffer and display in other window.
When OUTPUT-BUFFER-NAME is given, insert the output in the buffer with
that name."
  (interactive)
  (haddock-display-buffer-other-window
   (haddock-standalone output-buffer-name)))

(defun haddock-output-standalone-p ()
  "Determine whether `haddock-command' output is standalone XHTML.
Standalone XHTML output is identified by an occurrence of
`haddock-xhtml-standalone-regexp' in the first five lines of output."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (re-search-forward
       haddock-xhtml-standalone-regexp
       (save-excursion (goto-char (point-min)) (forward-line 4) (point))
       t))))

(defun haddock-stylesheet-link-string (stylesheet-path)
  (concat "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\""
          stylesheet-path
          "\"  />"))

(defun haddock-add-xhtml-header-and-footer (title)
  "Wrap XHTML header and footer with given TITLE around current buffer."
  (goto-char (point-min))
  (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
          "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
          "\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\n"
          "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\n"
          "<head>\n<title>")
  (insert title)
  (insert "</title>\n")
  (unless (= (length haddock-content-type) 0)
    (insert
     (format
      "<meta http-equiv=\"Content-Type\" content=\"%s;charset=%s\"/>\n"
      haddock-content-type
      (or (and haddock-coding-system
               (fboundp 'coding-system-get)
               (coding-system-get haddock-coding-system
                                  'mime-charset))
          (and (fboundp 'coding-system-get)
               (coding-system-get buffer-file-coding-system
                                  'mime-charset))
          "utf-8"))))
  (if (> (length haddock-css-paths) 0)
      (insert (mapconcat #'haddock-stylesheet-link-string
                         haddock-css-paths "\n")))
  (when (> (length haddock-xhtml-header-content) 0)
    (insert haddock-xhtml-header-content))
  (insert "\n</head>\n\n"
          "<body>\n\n")
  (when (> (length haddock-xhtml-body-preamble) 0)
    (insert haddock-xhtml-body-preamble "\n"))
  (goto-char (point-max))
  (when (> (length haddock-xhtml-body-epilogue) 0)
    (insert "\n" haddock-xhtml-body-epilogue))
  (insert "\n"
          "</body>\n"
          "</html>\n"))

(defun haddock-preview (&optional output-buffer-name)
  "Run `haddock-command' on the current buffer and view output in browser.
When OUTPUT-BUFFER-NAME is given, insert the output in the buffer with
that name."
  (interactive)
  (browse-url-of-buffer
   (haddock-standalone (or output-buffer-name haddock-output-buffer-name))))

(defun haddock-export-file-name (&optional extension)
  "Attempt to generate a filename for Haddock output.
The file extension will be EXTENSION if given, or .html by default.
If the current buffer is visiting a file, we construct a new
output filename based on that filename.  Otherwise, return nil."
  (when (buffer-file-name)
    (unless extension
      (setq extension ".html"))
    (let ((candidate
           (concat
            (cond
             ((buffer-file-name)
              (file-name-sans-extension (buffer-file-name)))
             (t (buffer-name)))
            extension)))
      (cond
       ((equal candidate (buffer-file-name))
        (concat candidate extension))
       (t
        candidate)))))

(defun haddock-export (&optional output-file)
  "Run Haddock on the current buffer, save to file, and return the filename.
If OUTPUT-FILE is given, use that as the filename.  Otherwise, use the filename
generated by `haddock-export-file-name', which will be constructed using the
current filename, but with the extension removed and replaced with .html."
  (interactive)
  (unless output-file
    (setq output-file (haddock-export-file-name ".html")))
  (when output-file
    (let* ((init-buf (current-buffer))
           (init-point (point))
           (init-buf-string (buffer-string))
           (output-buffer (find-file-noselect output-file))
           (output-buffer-name (buffer-name output-buffer)))
      (run-hooks 'haddock-before-export-hook)
      (haddock-standalone output-buffer-name)
      (with-current-buffer output-buffer
        (run-hooks 'haddock-after-export-hook)
        (save-buffer)
        (when haddock-export-kill-buffer (kill-buffer)))
      ;; if modified, restore initial buffer
      (when (buffer-modified-p init-buf)
        (erase-buffer)
        (insert init-buf-string)
        (save-buffer)
        (goto-char init-point))
      output-file)))

(defun haddock-export-and-preview ()
  "Export to XHTML using `haddock-export' and browse the resulting file."
  (interactive)
  (browse-url-of-file (haddock-export)))

(defvar haddock-live-preview-buffer nil
  "Buffer used to preview haddock output in `haddock-live-preview-export'.")
(make-variable-buffer-local 'haddock-live-preview-buffer)

(defvar haddock-live-preview-source-buffer nil
  "Source buffer from which current buffer was generated.
This is the inverse of `haddock-live-preview-buffer'.")
(make-variable-buffer-local 'haddock-live-preview-source-buffer)

(defvar haddock-live-preview-currently-exporting nil)

(defun haddock-live-preview-get-filename ()
  "Standardize the filename exported by `haddock-live-preview-export'."
  (haddock-export-file-name ".html"))

(defun haddock-live-preview-window-eww (file)
  "Preview FILE with eww.
To be used with `haddock-live-preview-window-function'."
  (if (require 'eww nil t)
      (progn
        (eww-open-file file)
        (get-buffer "*eww*"))
    (error "EWW is not present or not loaded on this version of Emacs")))

(defun haddock-visual-lines-between-points (beg end)
  (save-excursion
    (goto-char beg)
    (cl-loop with count = 0
             while (progn (end-of-visual-line)
                          (and (< (point) end) (line-move-visual 1 t)))
             do (cl-incf count)
             finally return count)))

(defun haddock-live-preview-window-serialize (buf)
  "Get window point and scroll data for all windows displaying BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (mapcar
       (lambda (win)
         (with-selected-window win
           (let* ((start (window-start))
                  (pt (window-point))
                  (pt-or-sym (cond ((= pt (point-min)) 'min)
                                   ((= pt (point-max)) 'max)
                                   (t pt)))
                  (diff (haddock-visual-lines-between-points
                         start pt)))
             (list win pt-or-sym diff))))
       (get-buffer-window-list buf)))))

(defun haddock-get-point-back-lines (pt num-lines)
  (save-excursion
    (goto-char pt)
    (line-move-visual (- num-lines) t)
    ;; in testing, can occasionally overshoot the number of lines to traverse
    (let ((actual-num-lines (haddock-visual-lines-between-points (point) pt)))
      (when (> actual-num-lines num-lines)
        (line-move-visual (- actual-num-lines num-lines) t)))
    (point)))

(defun haddock-live-preview-window-deserialize (window-posns)
  "Apply window point and scroll data from WINDOW-POSNS.
WINDOW-POSNS is provided by `haddock-live-preview-window-serialize'."
  (cl-destructuring-bind (win pt-or-sym diff) window-posns
    (when (window-live-p win)
      (with-current-buffer haddock-live-preview-buffer
        (set-window-buffer win (current-buffer))
        (cl-destructuring-bind (actual-pt actual-diff)
            (cl-case pt-or-sym
              (min (list (point-min) 0))
              (max (list (point-max) diff))
              (t   (list pt-or-sym diff)))
          (set-window-start
           win (haddock-get-point-back-lines actual-pt actual-diff))
          (set-window-point win actual-pt))))))

(defun haddock-live-preview-export ()
  "Export to XHTML using `haddock-export'.
Browse the resulting file within Emacs using
`haddock-live-preview-window-function' Return the buffer
displaying the rendered output."
  (interactive)
  (let ((filename (haddock-live-preview-get-filename)))
    (when filename
      (let* ((haddock-live-preview-currently-exporting t)
             (cur-buf (current-buffer))
             (export-file (haddock-export filename))
             ;; get positions in all windows currently displaying output buffer
             (window-data
              (haddock-live-preview-window-serialize
               haddock-live-preview-buffer)))
        (save-window-excursion
          (let ((output-buffer
                 (funcall haddock-live-preview-window-function export-file)))
            (with-current-buffer output-buffer
              (setq haddock-live-preview-source-buffer cur-buf)
              (add-hook 'kill-buffer-hook
                        #'haddock-live-preview-remove-on-kill t t))
            (with-current-buffer cur-buf
              (setq haddock-live-preview-buffer output-buffer))))
        (with-current-buffer cur-buf
          ;; reset all windows displaying output buffer to where they were,
          ;; now with the new output
          (mapc #'haddock-live-preview-window-deserialize window-data)
          ;; delete html editing buffer
          (let ((buf (get-file-buffer export-file))) (when buf (kill-buffer buf)))
          (when (and export-file (file-exists-p export-file)
                     (eq haddock-live-preview-delete-export
                         'delete-on-export))
            (delete-file export-file))
          haddock-live-preview-buffer)))))

(defun haddock-live-preview-remove ()
  (when (buffer-live-p haddock-live-preview-buffer)
    (kill-buffer haddock-live-preview-buffer))
  (setq haddock-live-preview-buffer nil)
  ;; if set to 'delete-on-export, the output has already been deleted
  (when (eq haddock-live-preview-delete-export 'delete-on-destroy)
    (let ((outfile-name (haddock-live-preview-get-filename)))
      (when (and outfile-name (file-exists-p outfile-name))
        (delete-file outfile-name)))))

(defun haddock-get-other-window ()
  "Find another window to display preview or output content."
  (cond
   ((memq haddock-split-window-direction '(vertical below))
    (or (window-in-direction 'below) (split-window-vertically)))
   ((memq haddock-split-window-direction '(horizontal right))
    (or (window-in-direction 'right) (split-window-horizontally)))
   (t (split-window-sensibly (get-buffer-window)))))

(defun haddock-display-buffer-other-window (buf)
  "Display preview or output buffer BUF in another window."
  (let ((cur-buf (current-buffer))
        (window (haddock-get-other-window)))
    (set-window-buffer window buf)
    (set-buffer cur-buf)))

(defun haddock-live-preview-if-haddock ()
  (when (and (derived-mode-p 'haddock-mode)
             haddock-live-preview-mode)
    (unless haddock-live-preview-currently-exporting
      (if (buffer-live-p haddock-live-preview-buffer)
          (haddock-live-preview-export)
        (haddock-display-buffer-other-window
         (haddock-live-preview-export))))))

(defun haddock-live-preview-remove-on-kill ()
  (cond ((and (derived-mode-p 'haddock-mode)
              haddock-live-preview-mode)
         (haddock-live-preview-remove))
        (haddock-live-preview-source-buffer
         (with-current-buffer haddock-live-preview-source-buffer
           (setq haddock-live-preview-buffer nil))
         (setq haddock-live-preview-source-buffer nil))))

(defun haddock-live-preview-switch-to-output ()
  "Switch to output buffer."
  (interactive)
  "Turn on `haddock-live-preview-mode' if not already on, and switch to its
output buffer in another window."
  (if haddock-live-preview-mode
      (haddock-display-buffer-other-window (haddock-live-preview-export)))
    (haddock-live-preview-mode))

(defun haddock-live-preview-re-export ()
  "Re export source buffer."
  (interactive)
  "If the current buffer is a buffer displaying the exported version of a
`haddock-live-preview-mode' buffer, call `haddock-live-preview-export' and
update this buffer's contents."
  (when haddock-live-preview-source-buffer
    (with-current-buffer haddock-live-preview-source-buffer
      (haddock-live-preview-export))))

(defun haddock-open ()
  "Open file for the current buffer with `haddock-open-command'."
  (interactive)
  (unless haddock-open-command
    (user-error "Variable `haddock-open-command' must be set"))
  (if (stringp haddock-open-command)
      (if (not buffer-file-name)
          (user-error "Must be visiting a file")
        (save-buffer)
        (let ((exit-code (call-process haddock-open-command nil nil nil
                                       buffer-file-name)))
          ;; The exit code can be a signal description string, so don’t use ‘=’
          ;; or ‘zerop’.
          (unless (eq exit-code 0)
            (user-error "%s failed with exit code %s"
                        haddock-open-command exit-code))))
    (funcall haddock-open-command))
  nil)

(defun haddock-kill-ring-save ()
  "Run Haddock on file and store output in the kill ring."
  (interactive)
  (save-window-excursion
    (haddock)
    (with-current-buffer haddock-output-buffer-name
      (kill-ring-save (point-min) (point-max)))))


;;; Links =====================================================================

(defun haddock-link-p ()
  "Return non-nil when `point' is at a non-wiki link.
See `haddock-wiki-link-p' for more information."
  (let ((case-fold-search nil))
    (and (not (haddock-wiki-link-p))
         (not (haddock-code-block-at-point-p))
         (or (thing-at-point-looking-at haddock-regex-link-inline)
             (thing-at-point-looking-at haddock-regex-link-reference)
             (thing-at-point-looking-at haddock-regex-uri)
             (thing-at-point-looking-at haddock-regex-angle-uri)))))

(make-obsolete 'haddock-link-link 'haddock-link-url "v2.3")

(defun haddock-link-at-pos (pos)
  "Return properties of link or image at position POS.
Value is a list of elements describing the link:
 0. beginning position
 1. end position
 2. link text
 3. URL
 4. reference label
 5. title text
 6. bang (nil or \"!\")"
  (save-excursion
    (goto-char pos)
    (let (begin end text url reference title bang)
      (cond
       ;; Inline or reference image or link at point.
       ((or (thing-at-point-looking-at haddock-regex-link-inline)
            (thing-at-point-looking-at haddock-regex-link-reference))
        (setq bang (match-string-no-properties 1)
              begin (match-beginning 0)
              end (match-end 0)
              text (match-string-no-properties 3))
        (if (char-equal (char-after (match-beginning 5)) ?\[)
            ;; Reference link
            (setq reference (match-string-no-properties 6))
          ;; Inline link
          (setq url (match-string-no-properties 6))
          (when (match-end 7)
            (setq title (substring (match-string-no-properties 7) 1 -1)))))
       ;; Angle bracket URI at point.
       ((thing-at-point-looking-at haddock-regex-angle-uri)
        (setq begin (match-beginning 0)
              end (match-end 0)
              url (match-string-no-properties 2)))
       ;; Plain URI at point.
       ((thing-at-point-looking-at haddock-regex-uri)
        (setq begin (match-beginning 0)
              end (match-end 0)
              url (match-string-no-properties 1))))
      (list begin end text url reference title bang))))

(defun haddock-link-url ()
  "Return the URL part of the regular (non-wiki) link at point.
Works with both inline and reference style links, and with images.
If point is not at a link or the link reference is not defined
returns nil."
  (let* ((values (haddock-link-at-pos (point)))
         (text (nth 2 values))
         (url (nth 3 values))
         (ref (nth 4 values)))
    (or url (and ref (car (haddock-reference-definition
                           (downcase (if (string= ref "") text ref))))))))

(defun haddock-follow-link-at-point ()
  "Open the current non-wiki link.
If the link is a complete URL, open in browser with `browse-url'.
Otherwise, open with `find-file' after stripping anchor and/or query string.
Translate filenames using `haddock-filename-translate-function'."
  (interactive)
  (if (haddock-link-p)
      (let* ((url (haddock-link-url))
             (struct (url-generic-parse-url url))
             (full (url-fullness struct))
             (file url))
        ;; Parse URL, determine fullness, strip query string
        (if (fboundp 'url-path-and-query)
            (setq file (car (url-path-and-query struct)))
          (when (and (setq file (url-filename struct))
                     (string-match "\\?" file))
            (setq file (substring file 0 (match-beginning 0)))))
        ;; Open full URLs in browser, files in Emacs
        (if full
            (browse-url url)
          (when (and file (> (length file) 0))
            (find-file (funcall haddock-translate-filename-function file)))))
    (user-error "Point is not at a Haddock link or URL")))

(defun haddock-fontify-inline-links (last)
  "Add text properties to next inline link from point to LAST."
  (when (haddock-match-generic-links last nil)
    (let* ((link-start (match-beginning 3))
           (link-end (match-end 3))
           (url-start (match-beginning 6))
           (url-end (match-end 6))
           (url (match-string-no-properties 6))
           (title-start (match-beginning 7))
           (title-end (match-end 7))
           (title (match-string-no-properties 7))
           ;; Markup part
           (mp (list 'face 'haddock-markup-face
                     'invisible 'haddock-markup
                     'rear-nonsticky t
                     'font-lock-multiline t))
           ;; Link part (without face)
           (lp (list 'keymap haddock-mode-mouse-map
                     'mouse-face 'haddock-highlight-face
                     'font-lock-multiline t
                     'help-echo (if title (concat title "\n" url) url)))
           ;; URL part
           (up (list 'keymap haddock-mode-mouse-map
                     'face 'haddock-url-face
                     'invisible 'haddock-markup
                     'mouse-face 'haddock-highlight-face
                     'font-lock-multiline t))
           ;; URL composition character
           (url-char (haddock--first-displayable haddock-url-compose-char))
           ;; Title part
           (tp (list 'face 'haddock-link-title-face
                     'invisible 'haddock-markup
                     'font-lock-multiline t)))
      (dolist (g '(1 2 4 5 8))
        (when (match-end g)
          (add-text-properties (match-beginning g) (match-end g) mp)))
      ;; Preserve existing faces applied to link part (e.g., inline code)
      (when link-start
        (add-text-properties link-start link-end lp)
        (add-face-text-property link-start link-end
                                'haddock-link-face 'append))
      (when url-start (add-text-properties url-start url-end up))
      (when title-start (add-text-properties url-end title-end tp))
      (when (and haddock-hide-urls url-start)
        (compose-region url-start (or title-end url-end) url-char))
      t)))

(defun haddock-fontify-reference-links (last)
  "Add text properties to next reference link from point to LAST."
  (when (haddock-match-generic-links last t)
    (let* ((link-start (match-beginning 3))
           (link-end (match-end 3))
           (ref-start (match-beginning 6))
           (ref-end (match-end 6))
           ;; Markup part
           (mp (list 'face 'haddock-markup-face
                     'invisible 'haddock-markup
                     'rear-nonsticky t
                     'font-lock-multiline t))
           ;; Link part
           (lp (list 'keymap haddock-mode-mouse-map
                     'face 'haddock-link-face
                     'mouse-face 'haddock-highlight-face
                     'font-lock-multiline t
                     'help-echo (lambda (_ __ pos)
                                  (save-match-data
                                    (save-excursion
                                      (goto-char pos)
                                      (or (haddock-link-url)
                                          "Undefined reference"))))))
           ;; URL composition character
           (url-char (haddock--first-displayable haddock-url-compose-char))
           ;; Reference part
           (rp (list 'face 'haddock-reference-face
                     'invisible 'haddock-markup
                     'font-lock-multiline t)))
      (dolist (g '(1 2 4 5 8))
        (when (match-end g)
          (add-text-properties (match-beginning g) (match-end g) mp)))
      (when link-start (add-text-properties link-start link-end lp))
      (when ref-start (add-text-properties ref-start ref-end rp)
            (when (and haddock-hide-urls (> (- ref-end ref-start) 2))
              (compose-region ref-start ref-end url-char)))
      t)))

(defun haddock-fontify-angle-uris (last)
  "Add text properties to angle URIs from point to LAST."
  (when (haddock-match-angle-uris last)
    (let* ((url-start (match-beginning 2))
           (url-end (match-end 2))
           ;; Markup part
           (mp (list 'face 'haddock-markup-face
                     'invisible 'haddock-markup
                     'rear-nonsticky t
                     'font-lock-multiline t))
           ;; URI part
           (up (list 'keymap haddock-mode-mouse-map
                     'face 'haddock-plain-url-face
                     'mouse-face 'haddock-highlight-face
                     'font-lock-multiline t)))
      (dolist (g '(1 3))
        (add-text-properties (match-beginning g) (match-end g) mp))
      (add-text-properties url-start url-end up)
      t)))

(defun haddock-fontify-plain-uris (last)
  "Add text properties to plain URLs from point to LAST."
  (when (haddock-match-plain-uris last)
    (let* ((start (match-beginning 0))
           (end (match-end 0))
           (props (list 'keymap haddock-mode-mouse-map
                        'face 'haddock-plain-url-face
                        'mouse-face 'haddock-highlight-face
                        'rear-nonsticky t
                        'font-lock-multiline t)))
      (add-text-properties start end props)
      t)))

(defun haddock-toggle-url-hiding (&optional arg)
  "Toggle the display or hiding of URLs.
With a prefix argument ARG, enable URL hiding if ARG is positive,
and disable it otherwise."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq haddock-hide-urls
        (if (eq arg 'toggle)
            (not haddock-hide-urls)
          (> (prefix-numeric-value arg) 0)))
  (if haddock-hide-urls
      (message "haddock-mode URL hiding enabled")
    (message "haddock-mode URL hiding disabled"))
  (haddock-reload-extensions))


;;; Wiki Links ================================================================

(defun haddock-wiki-link-p ()
  "Return non-nil if wiki links are enabled and `point' is at a true wiki link.
A true wiki link name matches `haddock-regex-wiki-link' but does
not match the current file name after conversion.  This modifies
the data returned by `match-data'.  Note that the potential wiki
link name must be available via `match-string'."
  (when haddock-enable-wiki-links
    (let ((case-fold-search nil))
      (and (thing-at-point-looking-at haddock-regex-wiki-link)
           (not (haddock-code-block-at-point-p))
           (or (not buffer-file-name)
               (not (string-equal (buffer-file-name)
                                  (haddock-convert-wiki-link-to-filename
                                   (haddock-wiki-link-link)))))))))

(defun haddock-wiki-link-link ()
  "Return the link part of the wiki link using current match data.
The location of the link component depends on the value of
`haddock-wiki-link-alias-first'."
  (if haddock-wiki-link-alias-first
      (or (match-string-no-properties 5) (match-string-no-properties 3))
    (match-string-no-properties 3)))

(defun haddock-wiki-link-alias ()
  "Return the alias or text part of the wiki link using current match data.
The location of the alias component depends on the value of
`haddock-wiki-link-alias-first'."
  (if haddock-wiki-link-alias-first
      (match-string-no-properties 3)
    (or (match-string-no-properties 5) (match-string-no-properties 3))))

(defun haddock-convert-wiki-link-to-filename (name)
  "Generate a filename from the wiki link NAME.
Spaces in NAME are replaced with `haddock-link-space-sub-char'.
When in `gfm-mode', follow GitHub's conventions where [[Test Test]]
and [[test test]] both map to Test-test.ext.  Look in the current
directory first, then in subdirectories if
`haddock-wiki-link-search-subdirectories' is non-nil, and then
in parent directories if
`haddock-wiki-link-search-parent-directories' is non-nil."
  (let* ((basename (haddock-replace-regexp-in-string
                    "[[:space:]\n]" haddock-link-space-sub-char name))
         (basename (if (memq major-mode '(gfm-mode gfm-view-mode))
                       (concat (upcase (substring basename 0 1))
                               (downcase (substring basename 1 nil)))
                     basename))
         directory extension default candidates dir)
    (when buffer-file-name
      (setq directory (file-name-directory buffer-file-name)
            extension (file-name-extension buffer-file-name)))
    (setq default (concat basename
                          (when extension (concat "." extension))))
    (cond
     ;; Look in current directory first.
     ((or (null buffer-file-name)
          (file-exists-p default))
      default)
     ;; Possibly search in subdirectories, next.
     ((and haddock-wiki-link-search-subdirectories
           (setq candidates
                 (haddock-directory-files-recursively
                  directory (concat "^" default "$"))))
      (car candidates))
     ;; Possibly search in parent directories as a last resort.
     ((and haddock-wiki-link-search-parent-directories
           (setq dir (locate-dominating-file directory default)))
      (concat dir default))
     ;; If nothing is found, return default in current directory.
     (t default))))

(defun haddock-follow-wiki-link (name &optional other)
  "Follow the wiki link NAME.
Convert the name to a file name and call `find-file'.  Ensure that
the new buffer remains in `haddock-mode'.  Open the link in another
window when OTHER is non-nil."
  (let ((filename (haddock-convert-wiki-link-to-filename name))
        (wp (when buffer-file-name
              (file-name-directory buffer-file-name))))
    (if (not wp)
        (user-error "Must be visiting a file")
      (when other (other-window 1))
      (let ((default-directory wp))
        (find-file filename)))
    (when (not (eq major-mode 'haddock-mode))
      (haddock-mode))))

(defun haddock-follow-wiki-link-at-point (&optional arg)
  "Find Wiki Link at point.
With prefix argument ARG, open the file in other window.
See `haddock-wiki-link-p' and `haddock-follow-wiki-link'."
  (interactive "P")
  (if (haddock-wiki-link-p)
      (haddock-follow-wiki-link (haddock-wiki-link-link) arg)
    (user-error "Point is not at a Wiki Link")))

(defun haddock-highlight-wiki-link (from to face)
  "Highlight the wiki link in the region between FROM and TO using FACE."
  (put-text-property from to 'font-lock-face face))

(defun haddock-unfontify-region-wiki-links (from to)
  "Remove wiki link faces from the region specified by FROM and TO."
  (interactive "*r")
  (let ((modified (buffer-modified-p)))
    (remove-text-properties from to '(font-lock-face haddock-link-face))
    (remove-text-properties from to '(font-lock-face haddock-missing-link-face))
    ;; remove-text-properties marks the buffer modified in emacs 24.3,
    ;; undo that if it wasn't originally marked modified
    (set-buffer-modified-p modified)))

(defun haddock-fontify-region-wiki-links (from to)
  "Search region given by FROM and TO for wiki links and fontify them.
If a wiki link is found check to see if the backing file exists
and highlight accordingly."
  (goto-char from)
  (save-match-data
    (while (re-search-forward haddock-regex-wiki-link to t)
      (when (not (haddock-code-block-at-point-p))
        (let ((highlight-beginning (match-beginning 1))
              (highlight-end (match-end 1))
              (file-name
               (haddock-convert-wiki-link-to-filename
                (haddock-wiki-link-link))))
          (if (condition-case nil (file-exists-p file-name) (error nil))
              (haddock-highlight-wiki-link
               highlight-beginning highlight-end 'haddock-link-face)
            (haddock-highlight-wiki-link
             highlight-beginning highlight-end 'haddock-missing-link-face)))))))

(defun haddock-extend-changed-region (from to)
  "Extend region given by FROM and TO so that we can fontify all links.
The region is extended to the first newline before and the first
newline after."
  ;; start looking for the first new line before 'from
  (goto-char from)
  (re-search-backward "\n" nil t)
  (let ((new-from (point-min))
        (new-to (point-max)))
    (if (not (= (point) from))
        (setq new-from (point)))
    ;; do the same thing for the first new line after 'to
    (goto-char to)
    (re-search-forward "\n" nil t)
    (if (not (= (point) to))
        (setq new-to (point)))
    (cl-values new-from new-to)))

(defun haddock-check-change-for-wiki-link (from to)
  "Check region between FROM and TO for wiki links and re-fontify as needed."
  (interactive "*r")
  (let* ((modified (buffer-modified-p))
         (buffer-undo-list t)
         (inhibit-read-only t)
         (inhibit-point-motion-hooks t)
         deactivate-mark
         buffer-file-truename)
    (unwind-protect
        (save-excursion
          (save-match-data
            (save-restriction
              ;; Extend the region to fontify so that it starts
              ;; and ends at safe places.
              (cl-multiple-value-bind (new-from new-to)
                  (haddock-extend-changed-region from to)
                (goto-char new-from)
                ;; Only refontify when the range contains text with a
                ;; wiki link face or if the wiki link regexp matches.
                (when (or (haddock-range-property-any
                           new-from new-to 'font-lock-face
                           '(haddock-link-face haddock-missing-link-face))
                          (re-search-forward
                           haddock-regex-wiki-link new-to t))
                  ;; Unfontify existing fontification (start from scratch)
                  (haddock-unfontify-region-wiki-links new-from new-to)
                  ;; Now do the fontification.
                  (haddock-fontify-region-wiki-links new-from new-to))))))
      (and (not modified)
           (buffer-modified-p)
           (set-buffer-modified-p nil)))))

(defun haddock-check-change-for-wiki-link-after-change (from to _)
    "Check region between FROM and TO for wiki links and re-fontify as needed.
Designed to be used with the `after-change-functions' hook."
  (haddock-check-change-for-wiki-link from to))

(defun haddock-fontify-buffer-wiki-links ()
  "Refontify all wiki links in the buffer."
  (interactive)
  (haddock-check-change-for-wiki-link (point-min) (point-max)))

(defun haddock-toggle-wiki-links (&optional arg)
  "Toggle support for wiki links.
With a prefix argument ARG, enable wiki link support if ARG is positive,
and disable it otherwise."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq haddock-enable-wiki-links
        (if (eq arg 'toggle)
            (not haddock-enable-wiki-links)
          (> (prefix-numeric-value arg) 0)))
  (if haddock-enable-wiki-links
      (message "haddock-mode wiki link support enabled")
    (message "haddock-mode wiki link support disabled"))
  (haddock-reload-extensions))

(defun haddock-setup-wiki-link-hooks ()
  "Add or remove hooks for fontifying wiki links.
These are only enabled when `haddock-wiki-link-fontify-missing' is non-nil."
  ;; Anytime text changes make sure it gets fontified correctly
  (if (and haddock-enable-wiki-links
           haddock-wiki-link-fontify-missing)
      (add-hook 'after-change-functions
                'haddock-check-change-for-wiki-link-after-change t t)
    (remove-hook 'after-change-functions
                 'haddock-check-change-for-wiki-link-after-change t))
  ;; If we left the buffer there is a really good chance we were
  ;; creating one of the wiki link documents. Make sure we get
  ;; refontified when we come back.
  (if (and haddock-enable-wiki-links
           haddock-wiki-link-fontify-missing)
      (progn
        (add-hook 'window-configuration-change-hook
                  'haddock-fontify-buffer-wiki-links t t)
        (haddock-fontify-buffer-wiki-links))
    (remove-hook 'window-configuration-change-hook
                 'haddock-fontify-buffer-wiki-links t)
  (haddock-unfontify-region-wiki-links (point-min) (point-max))))


;;; Following & Doing =========================================================

(defun haddock-follow-thing-at-point (arg)
  "Follow thing at point if possible, such as a reference link or wiki link.
Opens inline and reference links in a browser.  Opens wiki links
to other files in the current window, or the another window if
ARG is non-nil.
See `haddock-follow-link-at-point' and
`haddock-follow-wiki-link-at-point'."
  (interactive "P")
  (cond ((haddock-link-p)
         (haddock-follow-link-at-point))
        ((haddock-wiki-link-p)
         (haddock-follow-wiki-link-at-point arg))
        (t
         (user-error "Nothing to follow at point"))))

(make-obsolete 'haddock-jump 'haddock-do "v2.3")

(defun haddock-do ()
  "Do something sensible based on context at point.
Jumps between reference links and definitions; between footnote
markers and footnote text."
  (interactive)
  (cond
   ;; Footnote definition
   ((haddock-footnote-text-positions)
    (haddock-footnote-return))
   ;; Footnote marker
   ((haddock-footnote-marker-positions)
    (haddock-footnote-goto-text))
   ;; Reference link
   ((thing-at-point-looking-at haddock-regex-link-reference)
    (haddock-reference-goto-definition))
   ;; Reference definition
   ((thing-at-point-looking-at haddock-regex-reference-definition)
    (haddock-reference-goto-link (match-string-no-properties 2)))
   ;; GFM task list item
   ((haddock-gfm-task-list-item-at-point)
    (haddock-toggle-gfm-checkbox))
   ;; Align table
   ((haddock-table-at-point-p)
    (call-interactively #'haddock-table-align))
   ;; Otherwise
   (t
    (haddock-insert-gfm-checkbox))))


;;; Miscellaneous =============================================================

(defun haddock-compress-whitespace-string (str)
  "Compress whitespace in STR and return result.
Leading and trailing whitespace is removed.  Sequences of multiple
spaces, tabs, and newlines are replaced with single spaces."
  (haddock-replace-regexp-in-string "\\(^[ \t\n]+\\|[ \t\n]+$\\)" ""
                            (haddock-replace-regexp-in-string "[ \t\n]+" " " str)))

(defun haddock--substitute-command-keys (string)
  "Like `substitute-command-keys' but, but prefers control characters.
First pass STRING to `substitute-command-keys' and then
substitute `C-i` for `TAB` and `C-m` for `RET`."
  (replace-regexp-in-string
   "\\<TAB\\>" "C-i"
   (replace-regexp-in-string
    "\\<RET\\>" "C-m" (substitute-command-keys string) t) t))

(defun haddock-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
This is an exact copy of `line-number-at-pos' for use in emacs21."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun haddock-inside-link-p ()
  "Return t if point is within a link."
  (save-match-data
    (thing-at-point-looking-at (haddock-make-regex-link-generic))))

(defun haddock-line-is-reference-definition-p ()
  "Return whether the current line is a (non-footnote) reference defition."
  (save-excursion
    (move-beginning-of-line 1)
    (and (looking-at-p haddock-regex-reference-definition)
         (not (looking-at-p "[ \t]*\\[^")))))

(defun haddock-adaptive-fill-function ()
  "Return prefix for filling paragraph or nil if not determined."
  (cond
   ;; List item inside blockquote
   ((looking-at "^[ \t]*>[ \t]*\\(\\(?:[0-9]+\\|#\\)\\.\\|[*+:-]\\)[ \t]+")
    (haddock-replace-regexp-in-string
     "[0-9\\.*+-]" " " (match-string-no-properties 0)))
   ;; Blockquote
   ((looking-at haddock-regex-blockquote)
    (buffer-substring-no-properties (match-beginning 0) (match-end 2)))
   ;; List items
   ((looking-at haddock-regex-list)
    (match-string-no-properties 0))
   ;; Footnote definition
   ((looking-at-p haddock-regex-footnote-definition)
    "    ") ; four spaces
   ;; No match
   (t nil)))

(defun haddock-fill-paragraph (&optional justify)
  "Fill paragraph at or after point.
This function is like \\[fill-paragraph], but it skips Haddock
code blocks.  If the point is in a code block, or just before one,
do not fill.  Otherwise, call `fill-paragraph' as usual. If
JUSTIFY is non-nil, justify text as well.  Since this function
handles filling itself, it always returns t so that
`fill-paragraph' doesn't run."
  (interactive "P")
  (unless (or (haddock-code-block-at-point-p)
              (save-excursion
                (back-to-indentation)
                (skip-syntax-forward "-")
                (haddock-code-block-at-point-p)))
    (fill-paragraph justify))
  t)

(make-obsolete 'haddock-fill-forward-paragraph-function
               'haddock-fill-forward-paragraph "v2.3")

(defun haddock-fill-forward-paragraph (&optional arg)
  "Function used by `fill-paragraph' to move over ARG paragraphs.
This is a `fill-forward-paragraph-function' for `haddock-mode'.
It is called with a single argument specifying the number of
paragraphs to move.  Just like `forward-paragraph', it should
return the number of paragraphs left to move."
  (or arg (setq arg 1))
  (if (> arg 0)
      ;; With positive ARG, move across ARG non-code-block paragraphs,
      ;; one at a time.  When passing a code block, don't decrement ARG.
      (while (and (not (eobp))
                  (> arg 0)
                  (= (forward-paragraph 1) 0)
                  (or (haddock-code-block-at-pos (point-at-bol 0))
                      (setq arg (1- arg)))))
    ;; Move backward by one paragraph with negative ARG (always -1).
    (let ((start (point)))
      (setq arg (forward-paragraph arg))
      (while (and (not (eobp))
                  (progn (move-to-left-margin) (not (eobp)))
                  (looking-at-p paragraph-separate))
        (forward-line 1))
      (cond
       ;; Move point past whitespace following list marker.
       ((looking-at haddock-regex-list)
        (goto-char (match-end 0)))
       ;; Move point past whitespace following pipe at beginning of line
       ;; to handle Pandoc line blocks.
       ((looking-at "^|\\s-*")
        (goto-char (match-end 0)))
       ;; Return point if the paragraph passed was a code block.
       ((haddock-code-block-at-pos (point-at-bol 2))
        (goto-char start)))))
  arg)

(defun haddock--inhibit-electric-quote ()
  "Function added to `electric-quote-inhibit-functions'.
Return non-nil if the quote has been inserted inside a code block
or span."
  (let ((pos (1- (point))))
    (or (haddock-inline-code-at-pos pos)
        (haddock-code-block-at-pos pos))))


;;; Extension Framework =======================================================

(defun haddock-reload-extensions ()
  "Check settings, update font-lock keywords and hooks, and re-fontify buffer."
  (interactive)
  (when (member major-mode
                '(haddock-mode haddock-view-mode gfm-mode gfm-view-mode))
    ;; Refontify buffer
    (if (eval-when-compile (fboundp 'font-lock-flush))
        ;; Use font-lock-flush in Emacs >= 25.1
        (font-lock-flush)
      ;; Backwards compatibility for Emacs 24.3-24.5
      (when (and font-lock-mode (fboundp 'font-lock-refresh-defaults))
        (font-lock-refresh-defaults)))
    ;; Add or remove hooks related to extensions
    (haddock-setup-wiki-link-hooks)))

(defun haddock-handle-local-variables ()
  "Run in `hack-local-variables-hook' to update font lock rules.
Checks to see if there is actually a ‘haddock-mode’ file local variable
before regenerating font-lock rules for extensions."
  (when (and (boundp 'file-local-variables-alist)
             (or (assoc 'haddock-enable-wiki-links file-local-variables-alist)
                 (assoc 'haddock-enable-math file-local-variables-alist)))
    (when (assoc 'haddock-enable-math file-local-variables-alist)
      (haddock-toggle-math haddock-enable-math))
    (haddock-reload-extensions)))


;;; Math Support ==============================================================

(make-obsolete 'haddock-enable-math 'haddock-toggle-math "v2.1")

(defconst haddock-mode-font-lock-keywords-math
  (list
   ;; Equation reference (eq:foo)
   '("\\((eq:\\)\\([[:alnum:]:_]+\\)\\()\\)" . ((1 haddock-markup-face)
                                                (2 haddock-reference-face)
                                                (3 haddock-markup-face)))
   ;; Equation reference \eqref{foo}
   '("\\(\\\\eqref{\\)\\([[:alnum:]:_]+\\)\\(}\\)" . ((1 haddock-markup-face)
                                                      (2 haddock-reference-face)
                                                      (3 haddock-markup-face))))
  "Font lock keywords to add and remove when toggling math support.")

(defun haddock-toggle-math (&optional arg)
  "Toggle support for inline and display LaTeX math expressions.
With a prefix argument ARG, enable math mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq haddock-enable-math
        (if (eq arg 'toggle)
            (not haddock-enable-math)
          (> (prefix-numeric-value arg) 0)))
  (if haddock-enable-math
      (progn
        (font-lock-add-keywords
         'haddock-mode haddock-mode-font-lock-keywords-math)
        (message "haddock-mode math support enabled"))
    (font-lock-remove-keywords
     'haddock-mode haddock-mode-font-lock-keywords-math)
    (message "haddock-mode math support disabled"))
  (haddock-reload-extensions))


;;; GFM Checkboxes ============================================================

(define-button-type 'haddock-gfm-checkbox-button
  'follow-link t
  'face 'haddock-gfm-checkbox-face
  'mouse-face 'haddock-highlight-face
  'action #'haddock-toggle-gfm-checkbox-button)

(defun haddock-gfm-task-list-item-at-point (&optional bounds)
  "Return non-nil if there is a GFM task list item at the point.
Optionally, the list item BOUNDS may be given if available, as
returned by `haddock-cur-list-item-bounds'.  When a task list item
is found, the return value is the same value returned by
`haddock-cur-list-item-bounds'."
  (unless bounds
    (setq bounds (haddock-cur-list-item-bounds)))
  (> (length (nth 5 bounds)) 0))

(defun haddock-insert-gfm-checkbox ()
  "Add GFM checkbox at point.
Returns t if added.
Returns nil if non-applicable."
  (interactive)
    (let ((bounds (haddock-cur-list-item-bounds)))
      (if bounds
          (unless (cl-sixth bounds)
            (let ((pos (+ (cl-first bounds) (cl-fourth bounds)))
                  (markup "[ ] "))
              (if (< pos (point))
                  (save-excursion
                    (goto-char pos)
                    (insert markup))
                (goto-char pos)
                (insert markup))
              (syntax-propertize (+ (cl-second bounds) 4))
              t))
        (unless (save-excursion
                  (back-to-indentation)
                  (or (haddock-list-item-at-point-p)
                      (haddock-heading-at-point)
                      (haddock-in-comment-p)
                      (haddock-code-block-at-point-p)))
          (let ((pos (save-excursion
                       (back-to-indentation)
                       (point)))
                (markup (concat (or (save-excursion
                                      (beginning-of-line 0)
                                      (cl-fifth (haddock-cur-list-item-bounds)))
                                    haddock-unordered-list-item-prefix)
                                "[ ] ")))
            (if (< pos (point))
                (save-excursion
                  (goto-char pos)
                  (insert markup))
              (goto-char pos)
              (insert markup))
            (syntax-propertize (point-at-eol))
            t)))))

(defun haddock-toggle-gfm-checkbox ()
  "Toggle GFM checkbox at point.
Returns the resulting status as a string, either \"[x]\" or \"[ ]\".
Returns nil if there is no task list item at the point."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((bounds (haddock-cur-list-item-bounds)))
        (when bounds
          ;; Move to beginning of task list item
          (goto-char (cl-first bounds))
          ;; Advance to column of first non-whitespace after marker
          (forward-char (cl-fourth bounds))
          (cond ((looking-at "\\[ \\]")
                 (replace-match
                  (if haddock-gfm-uppercase-checkbox "[X]" "[x]")
                  nil t)
                 (match-string-no-properties 0))
                ((looking-at "\\[[xX]\\]")
                 (replace-match "[ ]" nil t)
                 (match-string-no-properties 0))))))))

(defun haddock-toggle-gfm-checkbox-button (button)
  "Toggle GFM checkbox BUTTON on click."
  (save-match-data
    (save-excursion
      (goto-char (button-start button))
      (haddock-toggle-gfm-checkbox))))

(defun haddock-make-gfm-checkboxes-buttons (start end)
  "Make GFM checkboxes buttons in region between START and END."
  (save-excursion
    (goto-char start)
    (let ((case-fold-search t))
      (save-excursion
        (while (re-search-forward haddock-regex-gfm-checkbox end t)
          (make-button (match-beginning 1) (match-end 1)
                       :type 'haddock-gfm-checkbox-button))))))

;; Called when any modification is made to buffer text.
(defun haddock-gfm-checkbox-after-change-function (beg end _)
  "Add to `after-change-functions' to setup GFM checkboxes as buttons.
BEG and END are the limits of scanned region."
  (save-excursion
    (save-match-data
      ;; Rescan between start of line from `beg' and start of line after `end'.
      (haddock-make-gfm-checkboxes-buttons
       (progn (goto-char beg) (beginning-of-line) (point))
       (progn (goto-char end) (forward-line 1) (point))))))

(defun haddock-remove-gfm-checkbox-overlays ()
  "Remove all GFM checkbox overlays in buffer."
  (save-excursion
    (save-restriction
      (widen)
      (remove-overlays nil nil 'face 'haddock-gfm-checkbox-face))))


;;; Display inline image ======================================================

(defvar haddock-inline-image-overlays nil)
(make-variable-buffer-local 'haddock-inline-image-overlays)

(defun haddock-remove-inline-images ()
  "Remove inline image overlays from image links in the buffer.
This can be toggled with `haddock-toggle-inline-images'
or \\[haddock-toggle-inline-images]."
  (interactive)
  (mapc #'delete-overlay haddock-inline-image-overlays)
  (setq haddock-inline-image-overlays nil))

(defcustom haddock-display-remote-images nil
  "If non-nil, download and display remote images.
See also `haddock-inline-image-overlays'.

Only image URLs specified with a protocol listed in
`haddock-remote-image-protocols' are displayed."
  :group 'haddock
  :type 'boolean)

(defcustom haddock-remote-image-protocols '("https")
  "List of protocols to use to download remote images.
See also `haddock-display-remote-images'."
  :group 'haddock
  :type '(repeat string))

(defvar haddock--remote-image-cache
  (make-hash-table :test 'equal)
  "A map from URLs to image paths.")

(defun haddock--get-remote-image (url)
  "Retrieve the image path for a given URL."
  (or (gethash url haddock--remote-image-cache)
      (let ((dl-path (make-temp-file "haddock-mode--image")))
        (require 'url)
        (url-copy-file url dl-path t)
        (puthash url dl-path haddock--remote-image-cache))))

(defun haddock-display-inline-images ()
  "Add inline image overlays to image links in the buffer.
This can be toggled with `haddock-toggle-inline-images'
or \\[haddock-toggle-inline-images]."
  (interactive)
  (unless (display-images-p)
    (error "Cannot show images"))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward haddock-regex-link-inline nil t)
        (let ((start (match-beginning 0))
              (imagep (match-beginning 1))
              (end (match-end 0))
              (file (match-string-no-properties 6)))
          (when (and imagep
                     (not (zerop (length file))))
            (unless (file-exists-p file)
              (when (and haddock-display-remote-images
                         (member (downcase (url-type (url-generic-parse-url file)))
                                 haddock-remote-image-protocols))
                (setq file (haddock--get-remote-image file))))
            (when (file-exists-p file)
              (let* ((abspath (if (file-name-absolute-p file)
                                  file
                                (concat default-directory file)))
                     (image
                      (if (and haddock-max-image-size
                               (image-type-available-p 'imagemagick))
                          (create-image
                           abspath 'imagemagick nil
                           :max-width (car haddock-max-image-size)
                           :max-height (cdr haddock-max-image-size))
                        (create-image abspath))))
                (when image
                  (let ((ov (make-overlay start end)))
                    (overlay-put ov 'display image)
                    (overlay-put ov 'face 'default)
                    (push ov haddock-inline-image-overlays)))))))))))

(defun haddock-toggle-inline-images ()
  "Toggle inline image overlays in the buffer."
  (interactive)
  (if haddock-inline-image-overlays
      (haddock-remove-inline-images)
    (haddock-display-inline-images)))


;;; GFM Code Block Fontification ==============================================

(defcustom haddock-fontify-code-blocks-natively nil
  "When non-nil, fontify code in code blocks using the native major mode.
This only works for fenced code blocks where the language is
specified where we can automatically determine the appropriate
mode to use.  The language to mode mapping may be customized by
setting the variable `haddock-code-lang-modes'."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp
  :package-version '(haddock-mode . "2.3"))

(defcustom haddock-fontify-code-block-default-mode nil
  "Default mode to use to fontify code blocks.
This mode is used when automatic detection fails, such as for GFM
code blocks with no language specified."
  :group 'haddock
  :type '(choice function (const :tag "None" nil))
  :package-version '(haddock-mode . "2.4"))

(defun haddock-toggle-fontify-code-blocks-natively (&optional arg)
  "Toggle the native fontification of code blocks.
With a prefix argument ARG, enable if ARG is positive,
and disable otherwise."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq haddock-fontify-code-blocks-natively
        (if (eq arg 'toggle)
            (not haddock-fontify-code-blocks-natively)
          (> (prefix-numeric-value arg) 0)))
  (if haddock-fontify-code-blocks-natively
      (message "haddock-mode native code block fontification enabled")
    (message "haddock-mode native code block fontification disabled"))
  (haddock-reload-extensions))

;; This is based on `org-src-lang-modes' from org-src.el
(defcustom haddock-code-lang-modes
  '(("ocaml" . tuareg-mode) ("elisp" . emacs-lisp-mode) ("ditaa" . artist-mode)
    ("asymptote" . asy-mode) ("dot" . fundamental-mode) ("sqlite" . sql-mode)
    ("calc" . fundamental-mode) ("C" . c-mode) ("cpp" . c++-mode)
    ("C++" . c++-mode) ("screen" . shell-script-mode) ("shell" . sh-mode)
    ("bash" . sh-mode))
  "Alist mapping languages to their major mode.
The key is the language name, the value is the major mode.  For
many languages this is simple, but for language where this is not
the case, this variable provides a way to simplify things on the
user side.  For example, there is no ocaml-mode in Emacs, but the
mode to use is `tuareg-mode'."
  :group 'haddock
  :type '(repeat
          (cons
           (string "Language name")
           (symbol "Major mode")))
  :package-version '(haddock-mode . "2.3"))

(defun haddock-get-lang-mode (lang)
  "Return major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol."
  (cl-find-if
   'fboundp
   (list (cdr (assoc lang haddock-code-lang-modes))
         (cdr (assoc (downcase lang) haddock-code-lang-modes))
         (intern (concat lang "-mode"))
         (intern (concat (downcase lang) "-mode")))))

(defun haddock-fontify-code-blocks-generic (matcher last)
  "Add text properties to next code block from point to LAST.
Use matching function MATCHER."
  (when (funcall matcher last)
    (save-excursion
      (save-match-data
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               ;; Find positions outside opening and closing backquotes.
               (bol-prev (progn (goto-char start)
                                (if (bolp) (point-at-bol 0) (point-at-bol))))
               (eol-next (progn (goto-char end)
                                (if (bolp) (point-at-bol 2) (point-at-bol 3))))
               lang)
          (if (and haddock-fontify-code-blocks-natively
                   (or (setq lang (haddock-code-block-lang))
                       haddock-fontify-code-block-default-mode))
              (haddock-fontify-code-block-natively lang start end)
            (add-text-properties start end '(face haddock-pre-face)))
          ;; Set background for block as well as opening and closing lines.
          (font-lock-append-text-property
           bol-prev eol-next 'face 'haddock-code-face)
          ;; Set invisible property for lines before and after, including newline.
          (add-text-properties bol-prev start '(invisible haddock-markup))
          (add-text-properties end eol-next '(invisible haddock-markup)))))
    t))

(defun haddock-fontify-gfm-code-blocks (last)
  "Add text properties to next GFM code block from point to LAST."
  (haddock-fontify-code-blocks-generic 'haddock-match-gfm-code-blocks last))

(defun haddock-fontify-fenced-code-blocks (last)
  "Add text properties to next tilde fenced code block from point to LAST."
  (haddock-fontify-code-blocks-generic 'haddock-match-fenced-code-blocks last))

;; Based on `org-src-font-lock-fontify-block' from org-src.el.
(defun haddock-fontify-code-block-natively (lang start end)
  "Fontify given GFM or fenced code block.
This function is called by Emacs for automatic fontification when
`haddock-fontify-code-blocks-natively' is non-nil.  LANG is the
language used in the block. START and END specify the block
position."
  (let ((lang-mode (if lang (haddock-get-lang-mode lang)
                     haddock-fontify-code-block-default-mode)))
    (when (fboundp lang-mode)
      (let ((string (buffer-substring-no-properties start end))
            (modified (buffer-modified-p))
            (haddock-buffer (current-buffer)) pos next)
        (remove-text-properties start end '(face nil))
        (with-current-buffer
            (get-buffer-create
             (concat " haddock-code-fontification:" (symbol-name lang-mode)))
          ;; Make sure that modification hooks are not inhibited in
          ;; the org-src-fontification buffer in case we're called
          ;; from `jit-lock-function' (Bug#25132).
          (let ((inhibit-modification-hooks nil))
            (delete-region (point-min) (point-max))
            (insert string " ")) ;; so there's a final property change
          (unless (eq major-mode lang-mode) (funcall lang-mode))
          (haddock-font-lock-ensure)
          (setq pos (point-min))
          (while (setq next (next-single-property-change pos 'face))
            (let ((val (get-text-property pos 'face)))
              (when val
                (put-text-property
                 (+ start (1- pos)) (1- (+ start next)) 'face
                 val haddock-buffer)))
            (setq pos next)))
        (add-text-properties
         start end
         '(font-lock-fontified t fontified t font-lock-multiline t))
        (set-buffer-modified-p modified)))))

(require 'edit-indirect nil t)
(defvar edit-indirect-guess-mode-function)
(defvar edit-indirect-after-commit-functions)

(defun haddock--edit-indirect-after-commit-function (_beg end)
  "Ensure trailing newlines at the END of code blocks."
  (goto-char end)
  (unless (eq (char-before) ?\n)
    (insert "\n")))

(defun haddock-edit-code-block ()
  "Edit Haddock code block in an indirect buffer."
  (interactive)
  (save-excursion
    (if (fboundp 'edit-indirect-region)
        (let* ((bounds (haddock-get-enclosing-fenced-block-construct))
               (begin (and bounds (goto-char (nth 0 bounds)) (point-at-bol 2)))
               (end (and bounds (goto-char (nth 1 bounds)) (point-at-bol 1))))
          (if (and begin end)
              (let* ((lang (haddock-code-block-lang))
                     (mode (or (and lang (haddock-get-lang-mode lang))
                               haddock-edit-code-block-default-mode))
                     (edit-indirect-guess-mode-function
                      (lambda (_parent-buffer _beg _end)
                        (funcall mode))))
                (edit-indirect-region begin end 'display-buffer))
            (user-error "Not inside a GFM or tilde fenced code block")))
      (when (y-or-n-p "Package edit-indirect needed to edit code blocks. Install it now? ")
        (progn (package-refresh-contents)
               (package-install 'edit-indirect)
               (haddock-edit-code-block))))))


;;; Table Editing =============================================================

;; These functions were originally adapted from `org-table.el'.

;; General helper functions

(defmacro haddock--with-gensyms (symbols &rest body)
  (declare (debug (sexp body)) (indent 1))
  `(let ,(mapcar (lambda (s)
                   `(,s (make-symbol (concat "--" (symbol-name ',s)))))
                 symbols)
     ,@body))

(defun haddock--split-string (string &optional separators)
  "Splits STRING into substrings at SEPARATORS.
SEPARATORS is a regular expression. If nil it defaults to
`split-string-default-separators'. This version returns no empty
strings if there are matches at the beginning and end of string."
  (let ((start 0) notfirst list)
    (while (and (string-match
                 (or separators split-string-default-separators)
                 string
                 (if (and notfirst
                          (= start (match-beginning 0))
                          (< start (length string)))
                     (1+ start) start))
                (< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
          (and (eq (match-beginning 0) (match-end 0))
               (eq (match-beginning 0) start))
          (push (substring string start (match-beginning 0)) list))
      (setq start (match-end 0)))
    (or (eq start (length string))
        (push (substring string start) list))
    (nreverse list)))

(defun haddock--string-width (s)
  "Return width of string S.
This version ignores characters with invisibility property
`haddock-markup'."
  (let (b)
    (when (or (eq t buffer-invisibility-spec)
              (member 'haddock-markup buffer-invisibility-spec))
      (while (setq b (text-property-any
                      0 (length s)
                      'invisible 'haddock-markup s))
        (setq s (concat
                 (substring s 0 b)
                 (substring s (or (next-single-property-change
                                   b 'invisible s)
                                  (length s))))))))
  (string-width s))

(defun haddock--remove-invisible-markup (s)
  "Remove Haddock markup from string S.
This version removes characters with invisibility property
`haddock-markup'."
  (let (b)
    (while (setq b (text-property-any
                    0 (length s)
                    'invisible 'haddock-markup s))
      (setq s (concat
               (substring s 0 b)
               (substring s (or (next-single-property-change
                                 b 'invisible s)
                                (length s)))))))
  s)

;; Functions for maintaining tables

(defvar haddock-table-at-point-p-function nil
  "Function to decide if point is inside a table.

The indirection serves to differentiate between standard haddock
tables and gfm tables which are less strict about the markup.")

(defconst haddock-table-line-regexp "^[ \t]*|"
  "Regexp matching any line inside a table.")

(defconst haddock-table-hline-regexp "^[ \t]*|[-:]"
  "Regexp matching hline inside a table.")

(defconst haddock-table-dline-regexp "^[ \t]*|[^-:]"
  "Regexp matching dline inside a table.")

(defun haddock-table-at-point-p ()
  "Return non-nil when point is inside a table."
  (if (functionp haddock-table-at-point-p-function)
      (funcall haddock-table-at-point-p-function)
    (haddock--table-at-point-p)))

(defun haddock--table-at-point-p ()
  "Return non-nil when point is inside a table."
  (save-excursion
    (beginning-of-line)
    (and (looking-at-p haddock-table-line-regexp)
         (not (haddock-code-block-at-point-p)))))

(defconst gfm-table-line-regexp "^.?*|"
  "Regexp matching any line inside a table.")

(defconst gfm-table-hline-regexp "^-+\\(|-\\)+"
  "Regexp matching hline inside a table.")

;; GFM simplified tables syntax is as follows:
;; - A header line for the column names, this is any text
;;   separated by `|'.
;; - Followed by a string -|-|- ..., the number of dashes is optional
;;   but must be higher than 1. The number of separators should match
;;   the number of columns.
;; - Followed by the rows of data, which has the same format as the
;;   header line.
;; Example:
;;
;; foo | bar
;; ------|---------
;; bar | baz
;; bar | baz
(defun gfm--table-at-point-p ()
  "Return non-nil when point is inside a gfm-compatible table."
  (or (haddock--table-at-point-p)
      (save-excursion
        (beginning-of-line)
        (when (looking-at-p gfm-table-line-regexp)
          ;; we might be at the first line of the table, check if the
          ;; line below is the hline
          (or (save-excursion
                (forward-line 1)
                (looking-at-p gfm-table-hline-regexp))
              ;; go up to find the header
              (catch 'done
                (while (looking-at-p gfm-table-line-regexp)
                  (cond
                   ((looking-at-p gfm-table-hline-regexp)
                    (throw 'done t))
                   ((bobp)
                    (throw 'done nil)))
                  (forward-line -1))
                nil))))))

(defun haddock-table-hline-at-point-p ()
  "Return non-nil when point is on a hline in a table.
This function assumes point is on a table."
  (save-excursion
    (beginning-of-line)
    (looking-at-p haddock-table-hline-regexp)))

(defun haddock-table-begin ()
  "Find the beginning of the table and return its position.
This function assumes point is on a table."
  (save-excursion
    (while (and (not (bobp))
                (haddock-table-at-point-p))
      (forward-line -1))
    (unless (or (eobp)
                (haddock-table-at-point-p))
      (forward-line 1))
    (point)))

(defun haddock-table-end ()
  "Find the end of the table and return its position.
This function assumes point is on a table."
  (save-excursion
    (while (and (not (eobp))
                (haddock-table-at-point-p))
      (forward-line 1))
    (point)))

(defun haddock-table-get-dline ()
  "Return index of the table data line at point.
This function assumes point is on a table."
  (let ((pos (point)) (end (haddock-table-end)) (cnt 0))
    (save-excursion
      (goto-char (haddock-table-begin))
      (while (and (re-search-forward
                   haddock-table-dline-regexp end t)
                  (setq cnt (1+ cnt))
                  (< (point-at-eol) pos))))
    cnt))

(defun haddock-table-get-column ()
  "Return table column at point.
This function assumes point is on a table."
  (let ((pos (point)) (cnt 0))
    (save-excursion
      (beginning-of-line)
      (while (search-forward "|" pos t) (setq cnt (1+ cnt))))
    cnt))

(defun haddock-table-get-cell (&optional n)
  "Return the content of the cell in column N of current row.
N defaults to column at point. This function assumes point is on
a table."
  (and n (haddock-table-goto-column n))
  (skip-chars-backward "^|\n") (backward-char 1)
  (if (looking-at "|[^|\r\n]*")
      (let* ((pos (match-beginning 0))
             (val (buffer-substring (1+ pos) (match-end 0))))
        (goto-char (min (point-at-eol) (+ 2 pos)))
        ;; Trim whitespaces
        (setq val (replace-regexp-in-string "\\`[ \t]+" "" val)
              val (replace-regexp-in-string "[ \t]+\\'" "" val)))
    (forward-char 1) ""))

(defun haddock-table-goto-dline (n)
  "Go to the Nth data line in the table at point.
Return t when the line exists, nil otherwise. This function
assumes point is on a table."
  (goto-char (haddock-table-begin))
  (let ((end (haddock-table-end)) (cnt 0))
    (while (and (re-search-forward
                 haddock-table-dline-regexp end t)
                (< (setq cnt (1+ cnt)) n)))
    (= cnt n)))

(defun haddock-table-goto-column (n &optional on-delim)
  "Go to the Nth column in the table line at point.
With optional argument ON-DELIM, stop with point before the left
delimiter of the cell. If there are less than N cells, just go
beyond the last delimiter. This function assumes point is on a
table."
  (beginning-of-line 1)
  (when (> n 0)
    (while (and (> (setq n (1- n)) -1)
                (search-forward "|" (point-at-eol) t)))
    (if on-delim
        (backward-char 1)
      (when (looking-at " ") (forward-char 1)))))

(defmacro haddock-table-save-cell (&rest body)
  "Save cell at point, execute BODY and restore cell.
This function assumes point is on a table."
  (declare (debug (body)))
  (haddock--with-gensyms (line column)
    `(let ((,line (copy-marker (line-beginning-position)))
           (,column (haddock-table-get-column)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,line)
         (haddock-table-goto-column ,column)
         (set-marker ,line nil)))))

(defun haddock-table-blank-line (s)
  "Convert a table line S into a line with blank cells."
  (if (string-match "^[ \t]*|-" s)
      (setq s (mapconcat
               (lambda (x) (if (member x '(?| ?+)) "|" " "))
               s ""))
    (while (string-match "|\\([ \t]*?[^ \t\r\n|][^\r\n|]*\\)|" s)
      (setq s (replace-match
               (concat "|" (make-string (length (match-string 1 s)) ?\ ) "|")
               t t s)))
    s))

(defun haddock-table-colfmt (fmtspec)
  "Process column alignment specifier FMTSPEC for tables."
  (when (stringp fmtspec)
    (mapcar (lambda (x)
              (cond ((string-match-p "^:.*:$" x) 'c)
                    ((string-match-p "^:"     x) 'l)
                    ((string-match-p ":$"     x) 'r)
                    (t 'd)))
            (haddock--split-string fmtspec "\\s-*|\\s-*"))))

(defun haddock-table-align ()
  "Align table at point.
This function assumes point is on a table."
  (interactive)
  (let ((begin (haddock-table-begin))
        (end (copy-marker (haddock-table-end))))
    (haddock-table-save-cell
     (goto-char begin)
     (let* (fmtspec
            ;; Store table indent
            (indent (progn (looking-at "[ \t]*") (match-string 0)))
            ;; Split table in lines and save column format specifier
            (lines (mapcar (lambda (l)
                             (if (string-match-p "\\`[ \t]*|[-:]" l)
                                 (progn (setq fmtspec (or fmtspec l)) nil) l))
                           (haddock--split-string (buffer-substring begin end) "\n")))
            ;; Split lines in cells
            (cells (mapcar (lambda (l) (haddock--split-string l "\\s-*|\\s-*"))
                           (remq nil lines)))
            ;; Calculate maximum number of cells in a line
            (maxcells (if cells
                          (apply #'max (mapcar #'length cells))
                        (user-error "Empty table")))
            ;; Empty cells to fill short lines
            (emptycells (make-list maxcells "")) maxwidths)
       ;; Calculate maximum width for each column
       (dotimes (i maxcells)
         (let ((column (mapcar (lambda (x) (or (nth i x) "")) cells)))
           (push (apply #'max 1 (mapcar #'haddock--string-width column))
                 maxwidths)))
       (setq maxwidths (nreverse maxwidths))
       ;; Process column format specifier
       (setq fmtspec (haddock-table-colfmt fmtspec))
       ;; Compute formats needed for output of table lines
       (let ((hfmt (concat indent "|"))
             (rfmt (concat indent "|"))
             hfmt1 rfmt1 fmt)
         (dolist (width maxwidths (setq hfmt (concat (substring hfmt 0 -1) "|")))
           (setq fmt (pop fmtspec))
           (cond ((equal fmt 'l) (setq hfmt1 ":%s-|" rfmt1 " %%-%ds |"))
                 ((equal fmt 'r) (setq hfmt1 "-%s:|" rfmt1  " %%%ds |"))
                 ((equal fmt 'c) (setq hfmt1 ":%s:|" rfmt1 " %%-%ds |"))
                 (t              (setq hfmt1 "-%s-|" rfmt1 " %%-%ds |")))
           (setq rfmt (concat rfmt (format rfmt1 width)))
           (setq hfmt (concat hfmt (format hfmt1 (make-string width ?-)))))
         ;; Replace modified lines only
         (dolist (line lines)
           (let ((line (if line
                           (apply #'format rfmt (append (pop cells) emptycells))
                         hfmt))
                 (previous (buffer-substring (point) (line-end-position))))
             (if (equal previous line)
                 (forward-line)
               (insert line "\n")
               (delete-region (point) (line-beginning-position 2))))))
       (set-marker end nil)))))

(defun haddock-table-insert-row (&optional arg)
  "Insert a new row above the row at point into the table.
With optional argument ARG, insert below the current row."
  (interactive "P")
  (unless (haddock-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((line (buffer-substring
                (line-beginning-position) (line-end-position)))
         (new (haddock-table-blank-line line)))
    (beginning-of-line (if arg 2 1))
    (unless (bolp) (insert "\n"))
    (insert-before-markers new "\n")
    (beginning-of-line 0)
    (re-search-forward "| ?" (line-end-position) t)))

(defun haddock-table-delete-row ()
  "Delete row or horizontal line at point from the table."
  (interactive)
  (unless (haddock-table-at-point-p)
    (user-error "Not at a table"))
  (let ((col (current-column)))
    (kill-region (point-at-bol)
                 (min (1+ (point-at-eol)) (point-max)))
    (unless (haddock-table-at-point-p) (beginning-of-line 0))
    (move-to-column col)))

(defun haddock-table-move-row (&optional up)
  "Move table line at point down.
With optional argument UP, move it up."
  (interactive "P")
  (unless (haddock-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((col (current-column)) (pos (point))
         (tonew (if up 0 2)) txt)
    (beginning-of-line tonew)
    (unless (haddock-table-at-point-p)
      (goto-char pos) (user-error "Cannot move row further"))
    (goto-char pos) (beginning-of-line 1) (setq pos (point))
    (setq txt (buffer-substring (point) (1+ (point-at-eol))))
    (delete-region (point) (1+ (point-at-eol)))
    (beginning-of-line tonew)
    (insert txt) (beginning-of-line 0)
    (move-to-column col)))

(defun haddock-table-move-row-up ()
  "Move table row at point up."
  (interactive)
  (haddock-table-move-row 'up))

(defun haddock-table-move-row-down ()
  "Move table row at point down."
  (interactive)
  (haddock-table-move-row nil))

(defun haddock-table-insert-column ()
  "Insert a new table column."
  (interactive)
  (unless (haddock-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((col (max 1 (haddock-table-get-column)))
         (begin (haddock-table-begin))
         (end (copy-marker (haddock-table-end))))
    (haddock-table-save-cell
     (goto-char begin)
     (while (< (point) end)
       (haddock-table-goto-column col t)
       (if (haddock-table-hline-at-point-p)
           (insert "|---")
         (insert "|   "))
       (forward-line)))
    (set-marker end nil)
    (haddock-table-align)))

(defun haddock-table-delete-column ()
  "Delete column at point from table."
  (interactive)
  (unless (haddock-table-at-point-p)
    (user-error "Not at a table"))
  (let ((col (haddock-table-get-column))
        (begin (haddock-table-begin))
        (end (copy-marker (haddock-table-end))))
    (haddock-table-save-cell
     (goto-char begin)
     (while (< (point) end)
       (haddock-table-goto-column col t)
       (and (looking-at "|[^|\n]+|")
            (replace-match "|"))
       (forward-line)))
    (set-marker end nil)
    (haddock-table-goto-column (max 1 (1- col)))
    (haddock-table-align)))

(defun haddock-table-move-column (&optional left)
  "Move table column at point to the right.
With optional argument LEFT, move it to the left."
  (interactive "P")
  (unless (haddock-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((col (haddock-table-get-column))
         (col1 (if left (1- col) col))
         (colpos (if left (1- col) (1+ col)))
         (begin (haddock-table-begin))
         (end (copy-marker (haddock-table-end))))
    (when (and left (= col 1))
      (user-error "Cannot move column further left"))
    (when (and (not left) (looking-at "[^|\n]*|[^|\n]*$"))
      (user-error "Cannot move column further right"))
    (haddock-table-save-cell
     (goto-char begin)
     (while (< (point) end)
       (haddock-table-goto-column col1 t)
       (when (looking-at "|\\([^|\n]+\\)|\\([^|\n]+\\)|")
         (replace-match "|\\2|\\1|"))
       (forward-line)))
    (set-marker end nil)
    (haddock-table-goto-column colpos)
    (haddock-table-align)))

(defun haddock-table-move-column-left ()
  "Move table column at point to the left."
  (interactive)
  (haddock-table-move-column 'left))

(defun haddock-table-move-column-right ()
  "Move table column at point to the right."
  (interactive)
  (haddock-table-move-column nil))

(defun haddock-table-next-row ()
  "Go to the next row (same column) in the table.
Create new table lines if required."
  (interactive)
  (unless (haddock-table-at-point-p)
    (user-error "Not at a table"))
  (if (or (looking-at "[ \t]*$")
          (save-excursion (skip-chars-backward " \t") (bolp)))
      (newline)
    (haddock-table-align)
    (let ((col (haddock-table-get-column)))
      (beginning-of-line 2)
      (if (or (not (haddock-table-at-point-p))
              (haddock-table-hline-at-point-p))
          (progn
            (beginning-of-line 0)
            (haddock-table-insert-row 'below)))
      (haddock-table-goto-column col)
      (skip-chars-backward "^|\n\r")
      (when (looking-at " ") (forward-char 1)))))

(defun haddock-table-forward-cell ()
  "Go to the next cell in the table.
Create new table lines if required."
  (interactive)
  (unless (haddock-table-at-point-p)
    (user-error "Not at a table"))
  (haddock-table-align)
  (let ((end (haddock-table-end)))
    (when (haddock-table-hline-at-point-p) (end-of-line 1))
    (condition-case nil
        (progn
          (re-search-forward "|" end)
          (if (looking-at "[ \t]*$")
              (re-search-forward "|" end))
          (if (and (looking-at "[-:]")
                   (re-search-forward "^[ \t]*|\\([^-:]\\)" end t))
              (goto-char (match-beginning 1)))
          (if (looking-at "[-:]")
              (progn
                (beginning-of-line 0)
                (haddock-table-insert-row 'below))
            (when (looking-at " ") (forward-char 1))))
      (error (haddock-table-insert-row 'below)))))

(defun haddock-table-backward-cell ()
  "Go to the previous cell in the table."
  (interactive)
  (unless (haddock-table-at-point-p)
    (user-error "Not at a table"))
  (haddock-table-align)
  (when (haddock-table-hline-at-point-p) (end-of-line 1))
  (condition-case nil
      (progn
        (re-search-backward "|" (haddock-table-begin))
        (re-search-backward "|" (haddock-table-begin)))
    (error (user-error "Cannot move to previous table cell")))
  (while (looking-at "|\\([-:]\\|[ \t]*$\\)")
    (re-search-backward "|" (haddock-table-begin)))
  (when (looking-at "| ?") (goto-char (match-end 0))))

(defun haddock-table-transpose ()
  "Transpose table at point.
Horizontal separator lines will be eliminated."
  (interactive)
  (unless (haddock-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((table (buffer-substring-no-properties
                 (haddock-table-begin) (haddock-table-end)))
         ;; Convert table to a Lisp structure
         (table (delq nil
                      (mapcar
                       (lambda (x)
                         (unless (string-match-p
                                  haddock-table-hline-regexp x)
                           (haddock--split-string x "\\s-*|\\s-*")))
                       (haddock--split-string table "[ \t]*\n[ \t]*"))))
         (dline_old (haddock-table-get-dline))
         (col_old (haddock-table-get-column))
         (contents (mapcar (lambda (_)
                             (let ((tp table))
                               (mapcar
                                (lambda (_)
                                  (prog1
                                      (pop (car tp))
                                    (setq tp (cdr tp))))
                                table)))
                           (car table))))
    (goto-char (haddock-table-begin))
    (re-search-forward "|") (backward-char)
    (delete-region (point) (haddock-table-end))
    (insert (mapconcat
             (lambda(x)
               (concat "| " (mapconcat 'identity x " | " ) "  |\n"))
             contents ""))
    (haddock-table-goto-dline col_old)
    (haddock-table-goto-column dline_old))
  (haddock-table-align))

(defun haddock-table-sort-lines (&optional sorting-type)
  "Sort table lines according to the column at point.

The position of point indicates the column to be used for
sorting, and the range of lines is the range between the nearest
horizontal separator lines, or the entire table of no such lines
exist. If point is before the first column, user will be prompted
for the sorting column. If there is an active region, the mark
specifies the first line and the sorting column, while point
should be in the last line to be included into the sorting.

The command then prompts for the sorting type which can be
alphabetically or numerically. Sorting in reverse order is also
possible.

If SORTING-TYPE is specified when this function is called from a
Lisp program, no prompting will take place. SORTING-TYPE must be
a character, any of (?a ?A ?n ?N) where the capital letters
indicate that sorting should be done in reverse order."
  (interactive)
  (unless (haddock-table-at-point-p)
    (user-error "Not at a table"))
  ;; Set sorting type and column used for sorting
  (let ((column (let ((c (haddock-table-get-column)))
                  (cond ((> c 0) c)
                        ((called-interactively-p 'any)
                         (read-number "Use column N for sorting: "))
                        (t 1))))
        (sorting-type
         (or sorting-type
             (read-char-exclusive
              "Sort type: [a]lpha [n]umeric (A/N means reversed): "))))
    (save-restriction
      ;; Narrow buffer to appropriate sorting area
      (if (region-active-p)
          (narrow-to-region
           (save-excursion
             (progn
               (goto-char (region-beginning)) (line-beginning-position)))
           (save-excursion
             (progn
               (goto-char (region-end)) (line-end-position))))
        (let ((start (haddock-table-begin))
              (end (haddock-table-end)))
          (narrow-to-region
           (save-excursion
             (if (re-search-backward
                  haddock-table-hline-regexp start t)
                 (line-beginning-position 2)
               start))
           (if (save-excursion (re-search-forward
                                haddock-table-hline-regexp end t))
               (match-beginning 0)
             end))))
      ;; Determine arguments for `sort-subr'
      (let* ((extract-key-from-cell
              (cl-case sorting-type
                ((?a ?A) #'haddock--remove-invisible-markup) ;; #'identity)
                ((?n ?N) #'string-to-number)
                (t (user-error "Invalid sorting type: %c" sorting-type))))
             (predicate
              (cl-case sorting-type
                ((?n ?N) #'<)
                ((?a ?A) #'string<))))
        ;; Sort selected area
        (goto-char (point-min))
        (sort-subr (memq sorting-type '(?A ?N))
                   (lambda ()
                     (forward-line)
                     (while (and (not (eobp))
                                 (not (looking-at
                                       haddock-table-dline-regexp)))
                       (forward-line)))
                   #'end-of-line
                   (lambda ()
                     (funcall extract-key-from-cell
                              (haddock-table-get-cell column)))
                   nil
                   predicate)
        (goto-char (point-min))))))

(defun haddock-table-convert-region (begin end &optional separator)
  "Convert region from BEGIN to END to table with SEPARATOR.

If every line contains at least one TAB character, the function
assumes that the material is tab separated (TSV). If every line
contains a comma, comma-separated values (CSV) are assumed. If
not, lines are split at whitespace into cells.

You can use a prefix argument to force a specific separator:
\\[universal-argument] once forces CSV, \\[universal-argument]
twice forces TAB, and \\[universal-argument] three times will
prompt for a regular expression to match the separator, and a
numeric argument N indicates that at least N consecutive
spaces, or alternatively a TAB should be used as the separator."

  (interactive "r\nP")
  (let* ((begin (min begin end)) (end (max begin end)) re)
    (goto-char begin) (beginning-of-line 1)
    (setq begin (point-marker))
    (goto-char end)
    (if (bolp) (backward-char 1) (end-of-line 1))
    (setq end (point-marker))
    (when (equal separator '(64))
      (setq separator (read-regexp "Regexp for cell separator: ")))
    (unless separator
      ;; Get the right cell separator
      (goto-char begin)
      (setq separator
            (cond
             ((not (re-search-forward "^[^\n\t]+$" end t)) '(16))
             ((not (re-search-forward "^[^\n,]+$" end t)) '(4))
             (t 1))))
    (goto-char begin)
    (if (equal separator '(4))
        ;; Parse CSV
        (while (< (point) end)
          (cond
           ((looking-at "^") (insert "| "))
           ((looking-at "[ \t]*$") (replace-match " |") (beginning-of-line 2))
           ((looking-at "[ \t]*\"\\([^\"\n]*\\)\"")
            (replace-match "\\1") (if (looking-at "\"") (insert "\"")))
           ((looking-at "[^,\n]+") (goto-char (match-end 0)))
           ((looking-at "[ \t]*,") (replace-match " | "))
           (t (beginning-of-line 2))))
      (setq re
            (cond
             ((equal separator '(4))  "^\\|\"?[ \t]*,[ \t]*\"?")
             ((equal separator '(16)) "^\\|\t")
             ((integerp separator)
              (if (< separator 1)
                  (user-error "Cell separator must contain one or more spaces")
                (format "^ *\\| *\t *\\| \\{%d,\\}" separator)))
             ((stringp separator) (format "^ *\\|%s" separator))
             (t (error "Invalid cell separator"))))
      (while (re-search-forward re end t) (replace-match "| " t t)))
    (goto-char begin)
    (haddock-table-align)))

(defun haddock-insert-table (&optional rows columns align)
  "Insert an empty pipe table.
Optional arguments ROWS, COLUMNS, and ALIGN specify number of
rows and columns and the column alignment."
  (interactive)
  (let* ((rows (or rows (string-to-number (read-string "Row size: "))))
         (columns (or columns (string-to-number (read-string "Column size: "))))
         (align (or align (read-string "Alignment ([l]eft, [r]ight, [c]enter, or RET for default): ")))
         (align (cond ((equal align "l") ":--")
                      ((equal align "r") "--:")
                      ((equal align "c") ":-:")
                      (t "---")))
         (pos (point))
         (indent (make-string (current-column) ?\ ))
         (line (concat
                (apply 'concat indent "|"
                       (make-list columns "   |")) "\n"))
         (hline (apply 'concat indent "|"
                       (make-list columns (concat align "|")))))
    (if (string-match
         "^[ \t]*$" (buffer-substring-no-properties
                     (point-at-bol) (point)))
        (beginning-of-line 1)
      (newline))
    (dotimes (_ rows) (insert line))
    (goto-char pos)
    (if (> rows 1)
        (progn
          (end-of-line 1) (insert (concat "\n" hline)) (goto-char pos)))
    (haddock-table-forward-cell)))


;;; ElDoc Support =============================================================

(defun haddock-eldoc-function ()
  "Return a helpful string when appropriate based on context.
* Report URL when point is at a hidden URL.
* Report language name when point is a code block with hidden markup."
  (cond
   ;; Hidden URL or reference for inline link
   ((and (or (thing-at-point-looking-at haddock-regex-link-inline)
             (thing-at-point-looking-at haddock-regex-link-reference))
         (or haddock-hide-urls haddock-hide-markup))
    (let* ((imagep (string-equal (match-string 1) "!"))
           (edit-keys (haddock--substitute-command-keys
                       (if imagep
                           "\\[haddock-insert-image]"
                         "\\[haddock-insert-link]")))
           (edit-str (propertize edit-keys 'face 'font-lock-constant-face))
           (referencep (string-equal (match-string 5) "["))
           (object (if referencep "reference" "URL")))
      (format "Hidden %s (%s to edit): %s" object edit-str
              (if referencep
                  (concat
                   (propertize "[" 'face 'haddock-markup-face)
                   (propertize (match-string-no-properties 6)
                               'face 'haddock-reference-face)
                   (propertize "]" 'face 'haddock-markup-face))
                (propertize (match-string-no-properties 6)
                            'face 'haddock-url-face)))))
   ;; Hidden language name for fenced code blocks
   ((and (haddock-code-block-at-point-p)
         (not (get-text-property (point) 'haddock-pre))
         haddock-hide-markup)
    (let ((lang (save-excursion (haddock-code-block-lang))))
      (unless lang (setq lang "[unspecified]"))
      (format "Hidden code block language: %s (%s to toggle markup)"
              (propertize lang 'face 'haddock-language-keyword-face)
              (haddock--substitute-command-keys
               "\\[haddock-toggle-markup-hiding]"))))))


;;; Mode Definition  ==========================================================

(defun haddock-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "haddock-mode, version %s" haddock-mode-version))

(defun haddock-mode-info ()
  "Open the `haddock-mode' homepage."
  (interactive)
  (browse-url "https://jblevins.org/projects/haddock-mode/"))

;;;###autoload
(define-derived-mode haddock-mode text-mode "Haddock"
  "Major mode for editing Haddock files."
  ;; Natural Haddock tab width
  (setq tab-width 4)
  ;; Comments
  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->")
  (setq-local comment-start-skip "<!--[ \t]*")
  (setq-local comment-column 0)
  (setq-local comment-auto-fill-only-comments nil)
  (setq-local comment-use-syntax t)
  ;; Syntax
  (add-hook 'syntax-propertize-extend-region-functions
            #'haddock-syntax-propertize-extend-region)
  (add-hook 'jit-lock-after-change-extend-region-functions
            #'haddock-font-lock-extend-region-function t t)
  (setq-local syntax-propertize-function #'haddock-syntax-propertize)
  (syntax-propertize (point-max)) ;; Propertize before hooks run, etc.
  ;; Font lock.
  (setq font-lock-defaults
        '(haddock-mode-font-lock-keywords
          nil nil nil nil
          (font-lock-multiline . t)
          (font-lock-syntactic-face-function . haddock-syntactic-face)
          (font-lock-extra-managed-props
           . (composition display invisible rear-nonsticky
                          keymap help-echo mouse-face))))
  (if haddock-hide-markup
      (add-to-invisibility-spec 'haddock-markup)
    (remove-from-invisibility-spec 'haddock-markup))
  ;; Wiki links
  (haddock-setup-wiki-link-hooks)
  ;; Math mode
  (when haddock-enable-math (haddock-toggle-math t))
  ;; Add a buffer-local hook to reload after file-local variables are read
  (add-hook 'hack-local-variables-hook #'haddock-handle-local-variables nil t)
  ;; For imenu support
  (setq imenu-create-index-function
        (if haddock-nested-imenu-heading-index
            #'haddock-imenu-create-nested-index
          #'haddock-imenu-create-flat-index))
  ;; For menu support in XEmacs
  (easy-menu-add haddock-mode-menu haddock-mode-map)
  ;; Defun movement
  (setq-local beginning-of-defun-function #'haddock-beginning-of-defun)
  (setq-local end-of-defun-function #'haddock-end-of-defun)
  ;; Paragraph filling
  (setq-local fill-paragraph-function #'haddock-fill-paragraph)
  (setq-local paragraph-start
              ;; Should match start of lines that start or separate paragraphs
              (mapconcat #'identity
                         '(
                           "\f" ; starts with a literal line-feed
                           "[ \t\f]*$" ; space-only line
                           "\\(?:[ \t]*>\\)+[ \t\f]*$"; empty line in blockquote
                           "[ \t]*[*+-][ \t]+" ; unordered list item
                           "[ \t]*\\(?:[0-9]+\\|#\\)\\.[ \t]+" ; ordered list item
                           "[ \t]*\\[\\S-*\\]:[ \t]+" ; link ref def
                           "[ \t]*:[ \t]+" ; definition
                           "^|" ; table or Pandoc line block
                           )
                         "\\|"))
  (setq-local paragraph-separate
              ;; Should match lines that separate paragraphs without being
              ;; part of any paragraph:
              (mapconcat #'identity
                         '("[ \t\f]*$" ; space-only line
                           "\\(?:[ \t]*>\\)+[ \t\f]*$"; empty line in blockquote
                           ;; The following is not ideal, but the Fill customization
                           ;; options really only handle paragraph-starting prefixes,
                           ;; not paragraph-ending suffixes:
                           ".*  $" ; line ending in two spaces
                           "^#+"
                           "[ \t]*\\[\\^\\S-*\\]:[ \t]*$") ; just the start of a footnote def
                         "\\|"))
  (setq-local adaptive-fill-first-line-regexp "\\`[ \t]*[A-Z]?>[ \t]*?\\'")
  (setq-local adaptive-fill-regexp "\\s-*")
  (setq-local adaptive-fill-function #'haddock-adaptive-fill-function)
  (setq-local fill-forward-paragraph-function #'haddock-fill-forward-paragraph)
  ;; Outline mode
  (setq-local outline-regexp haddock-regex-header)
  (setq-local outline-level #'haddock-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  ;; ElDoc support
  (if (eval-when-compile (fboundp 'add-function))
      (add-function :before-until (local 'eldoc-documentation-function)
                    #'haddock-eldoc-function)
    (setq-local eldoc-documentation-function #'haddock-eldoc-function))
  ;; Inhibiting line-breaking:
  ;; Separating out each condition into a separate function so that users can
  ;; override if desired (with remove-hook)
  (add-hook 'fill-nobreak-predicate
            #'haddock-line-is-reference-definition-p nil t)
  (add-hook 'fill-nobreak-predicate
            #'haddock-pipe-at-bol-p nil t)

  ;; Indentation
  (setq-local indent-line-function haddock-indent-function)

  ;; Flyspell
  (setq-local flyspell-generic-check-word-predicate
              #'haddock-flyspell-check-word-p)

  ;; Electric quoting
  (add-hook 'electric-quote-inhibit-functions
            #'haddock--inhibit-electric-quote nil :local)

  ;; Backwards compatibility with haddock-css-path
  (when (boundp 'haddock-css-path)
    (warn "haddock-css-path is deprecated, see haddock-css-paths.")
    (add-to-list 'haddock-css-paths haddock-css-path))

  ;; Prepare hooks for XEmacs compatibility
  (when (featurep 'xemacs)
    (make-local-hook 'after-change-functions)
    (make-local-hook 'font-lock-extend-region-functions)
    (make-local-hook 'window-configuration-change-hook))

  ;; Make checkboxes buttons
  (when haddock-make-gfm-checkboxes-buttons
    (haddock-make-gfm-checkboxes-buttons (point-min) (point-max))
    (add-hook 'after-change-functions #'haddock-gfm-checkbox-after-change-function t t)
    (add-hook 'change-major-mode-hook #'haddock-remove-gfm-checkbox-overlays t t))

  ;; edit-indirect
  (add-hook 'edit-indirect-after-commit-functions
            #'haddock--edit-indirect-after-commit-function
            nil 'local)

  ;; Marginalized headings
  (when haddock-marginalize-headers
    (add-hook 'window-configuration-change-hook
              #'haddock-marginalize-update-current nil t))

  ;; add live preview export hook
  (add-hook 'after-save-hook #'haddock-live-preview-if-haddock t t)
  (add-hook 'kill-buffer-hook #'haddock-live-preview-remove-on-kill t t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.haddock\\'" . haddock-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.md\\'" . haddock-mode))


;;; GitHub Flavored Haddock Mode  ============================================

(defvar gfm-mode-hook nil
  "Hook run when entering GFM mode.")

;;;###autoload
(define-derived-mode gfm-mode haddock-mode "GFM"
  "Major mode for editing GitHub Flavored Haddock files."
  (setq haddock-link-space-sub-char "-")
  (setq haddock-wiki-link-search-subdirectories t)
  (setq-local haddock-table-at-point-p-function 'gfm--table-at-point-p)
  (haddock-gfm-parse-buffer-for-languages))

(define-obsolete-variable-alias
 'gfm-font-lock-keywords
 'haddock-mode-font-lock-keywords "v2.4")


;;; Viewing modes =============================================================

(defcustom haddock-hide-markup-in-view-modes t
  "Enable hidden markup mode in `haddock-view-mode' and `gfm-view-mode'."
  :group 'haddock
  :type 'boolean
  :safe 'booleanp)

(defvar haddock-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'haddock-outline-previous)
    (define-key map (kbd "n") #'haddock-outline-next)
    (define-key map (kbd "f") #'haddock-outline-next-same-level)
    (define-key map (kbd "b") #'haddock-outline-previous-same-level)
    (define-key map (kbd "u") #'haddock-outline-up)
    (define-key map (kbd "DEL") #'scroll-down-command)
    (define-key map (kbd "SPC") #'scroll-up-command)
    (define-key map (kbd ">") #'end-of-buffer)
    (define-key map (kbd "<") #'beginning-of-buffer)
    (define-key map (kbd "q") #'kill-this-buffer)
    (define-key map (kbd "?") #'describe-mode)
    map)
  "Keymap for `haddock-view-mode'.")

;;;###autoload
(define-derived-mode haddock-view-mode haddock-mode "Haddock-View"
  "Major mode for viewing Haddock content."
  (setq-local haddock-hide-markup haddock-hide-markup-in-view-modes)
  (read-only-mode 1))

(defvar gfm-view-mode-map
  haddock-view-mode-map
  "Keymap for `gfm-view-mode'.")

;;;###autoload
(define-derived-mode gfm-view-mode gfm-mode "GFM-View"
  "Major mode for viewing GitHub Flavored Haddock content."
  (setq-local haddock-hide-markup haddock-hide-markup-in-view-modes)
  (read-only-mode 1))


;;; Live Preview Mode  ========================================================
;;;###autoload
(define-minor-mode haddock-live-preview-mode
  "Toggle native previewing on save for a specific haddock file."
  :lighter " MD-Preview"
  (if haddock-live-preview-mode
      (if (haddock-live-preview-get-filename)
          (haddock-display-buffer-other-window (haddock-live-preview-export))
        (haddock-live-preview-mode -1)
        (user-error "Buffer %s does not visit a file" (current-buffer)))
    (haddock-live-preview-remove)))


(provide 'haddock-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:
;;; haddock-mode.el ends here