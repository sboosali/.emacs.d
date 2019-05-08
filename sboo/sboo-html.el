;;; sboo-html.el --- -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/sboosali/.emacs.d
;; Keywords: local
;; Created: 08 May 2019
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

;; Personal configuration for HTML languages, including Markdown.
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
  (require 'dom)                          ;TODO use `dom-attributes' et al?
  (require 'subr-x)
  (require 'seq))

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defsubst sboo-html-symbol-listp (object)

  "Whether OBJECT's type is a `listp' of `symbolp's.

Output:

• a `booleanp'.

Links:

• Info node `(elisp) Declare Form'."

  (declare (pure                      t)
           (side-effect-free          t)
           )

  (and (listp object)
       (seq-every-p #'symbolp object)
       ))

;;TODO...
;; Warning: Unknown defun property ‘safe-function’ in sboo-html-symbol-listp
;; Warning: Unknown defun property ‘safe-local-eval-function’ in sboo-html-sym

;;----------------------------------------------;;

(defsubst sboo-html-string-listp (object)

  "Whether OBJECT's type is a `listp' of `stringp's.

Output:

• a `booleanp'.

Links:

• Info node `(elisp) Declare Form'."

  (declare (pure                      t)
           (side-effect-free          t)
           )

  (and (listp object)
       (seq-every-p #'stringp object)
       ))

;;----------------------------------------------;;
;; Customization -------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-html

  nil

  "Personal HTML customization."

  :prefix 'html

  :group 'sboo
  :group 'html)

;;==============================================;;

(defcustom sboo-html-functions

  (list #'sboo-html-set-insert-command
        #'superword-mode
        )

  "Hooks for Major Modes in `sboo-html-modes-list'.

Zero-or-more function-symbols."

  :type '(repeat (function))

  :safe #'listp
  :group 'sboo-html)

;;----------------------------------------------;;

(defcustom sboo-html-modes-list

  '(
    html-mode
    mhtml-mode
    markdown-mode
    gfm-mode
   )

  "Major modes that are children of (or similar to) `html-mode'.

A `listp' of `symbolp's."

  :type '(repeated (choice (const nil)
                           (symbol :tag "Major Mode")))

  :safe #'sboo-symbol-listp
  :group 'sboo-html)

;;----------------------------------------------;;

(defcustom sboo-html-hooks

  '(
    html-mode-hook
    mhtml-mode-hook
    markdown-mode-hook
    gfm-mode-hook
   )

  "Hooks for `sboo-html-modes-list' (e.g. `html-mode-hook').

A `listp' of `symbolp's.

By default:

• `html-mode-hook'
• `mhtml-mode-hook'
• `markdown-mode-hook'
• `gfm-mode-hook'"

  :type '(repeated (choice (const nil)
                           (symbol :tag "Hook")))

  :safe #'sboo-symbol-listp
  :group 'sboo-html)

;;----------------------------------------------;;

(defcustom sboo-markdown-modes-list

  '(
    markdown-mode
    gfm-mode
   )

  "Major modes that are children of (or similar to) `markdown-mode'.

A `listp' of `symbolp's."

  :type '(repeated (choice (const nil)
                           (symbol :tag "Major Mode")))

  :safe #'sboo-symbol-listp
  :group 'sboo-html)

;;----------------------------------------------;;

(defcustom sboo-html-supermodes-alist

  '(
    (mhtml-mode    html-mode)
    (markdown-mode html-mode mhtml-mode)
    (gfm-mode      html-mode mhtml-mode markdown-mode)
   )

  "The (explicitly-transitive) parents `major-mode's for each `major-mode'.

Associates `keywordp's with `listp's of `symbolp's."

  :type '(alist :key-type   (symbol :tag "Major Mode")
                :value-type (repeated (symbol :tag "Parents (Major Modes)")))

  :safe #'listp
  :group 'sboo-html)

;;----------------------------------------------;;

(defcustom sboo-html-element-list

  '(
    "!DOCTYPE"
    "a"
    "abbr"
    "address"
    "area"
    "article"
    "aside"
    "b"
    "base"
    "bdi"
    "bdo"
    "blockquote"
    "body"
    "br"
    "button"
    "canvas"
    "caption"
    "cite"
    "code"
    "col"
    "colgroup"
    "data"
    "datalist"
    "dd"
    "del"
    "details"
    "dfn"
    "dialog"
    "div"
    "dl"
    "dt"
    "em"
    "embed"
    "fieldset"
    "figure"
    "footer"
    "form"
    "h1"
    "h2"
    "h3"
    "h4"
    "h5"
    "h6"
    "head"
    "header"
    "hgroup"
    "hr"
    "html"
    "i"
    "iframe"
    "img"
    "input"
    "ins"
    "kbd"
    "keygen"
    "label"
    "legend"
    "li"
    "link"
    "main"
    "map"
    "mark"
    "menu"
    "menuitem"
    "meta"
    "meter"
    "nav"
    "noscript"
    "object"
    "ol"
    "optgroup"
    "option"
    "output"
    "p"
    "param"
    "pre"
    "progress"
    "q"
    "rb"
    "rp"
    "rt"
    "rtc"
    "ruby"
    "s"
    "samp"
    "script"
    "section"
    "select"
    "small"
    "source"
    "span"
    "strong"
    "style"
    "sub"
    "summary"
    "sup"
    "table"
    "tbody"
    "td"
    "template"
    "textarea"
    "tfoot"
    "th"
    "thead"
    "time"
    "title"
    "tr"
    "track"
    "u"
    "ul"
    "var"
    "video"
    "wbr"
   )

  "All (standard) HTML Elements.

A `listp' of `stringp's.

Links:

• URL `https://developer.mozilla.org/en-US/docs/Web/HTML/Element'."

  :type '(repeated (choice (const nil)
                           (string :tag "HTML Element")))

  :safe #'sboo-string-listp
  :group 'sboo-html)

;;----------------------------------------------;;

(defcustom sboo-html-attribute-list

  '(
    ""
   )

  "All (standard) HTML Attributes.

A `listp' of `stringp's.

Links:

• URL `https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes#Attribute_list'."

  :type '(repeated (choice (const nil)
                           (string :tag "HTML Attribute")))

  :safe #'sboo-string-listp
  :group 'sboo-html)

;;----------------------------------------------;;

(defcustom sboo-markdown-wrappers-alist

  '( "*"
   )

  "`wrap-region' “wrappers”.

Examples (each is equivalent):

• « ?* »
• « \"*\" »
• « '(\"*\") »
• « '(\"*\" \"*\") »
• « '(\"*\" \"*\" \"*\") »

Associates right-wrappers with 
a(n optionally distinct) left-wrapper and 
a(n optionally distinct) trigger-keys."

  :type '(repeated (choice (character :tag "Left=Right=Key")
                           (string    :tag "Left=Right=Key")
                           (list      (string :tag "Left=Key")
                                      (string :tag "Right"))
                           (list      (string :tag "Left")
                                      (string :tag "Right")
                                      (string :tag "Key"))
                           ))

  :safe #'listp
  :group 'sboo-html)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Accessors -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-html-elements ()

  "Accessor for variable `sboo-html-element-list'."

  sboo-html-element-list)

;;----------------------------------------------;;

(defun sboo-html-attributes ()

  "Accessor for variable `sboo-html-attribute-list'."

  sboo-html-attribute-list)

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-html-set-insert-command ()

  "`sboo-insert' to `sgml-tag'."

  (setq-local sboo-insert-command #'sgml-tag))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(cl-defun sboo-html-element-format (element &key inner attributes)

  "Format ELEMENT as an open tag.

Inputs:

• ELEMENT — a `stringp'.

Output:

• a `stringp'.

Example:

• M-: (sboo-html-element-format \"a\" :inner \"HTML Elements\" :attributes '(:href \"https://developer.mozilla.org/en-US/docs/Web/HTML/Element\"))
    ⇒ \"<a>\"

Related:

• `sboo-html-element-format-open'
• `sboo-html-element-format-close'"

  (let* ((OPEN  (sboo-html-element-format-open  element :attributes attributes))
         (CLOSE (sboo-html-element-format-close element))
         (INNER (or inner ""))
         )

  (format "%s%s%s" OPEN INNER CLOSE)))

;; M-: (sboo-html-element-format "a" :inner "HTML Elements")
;;   ⇒ "<a>HTML Elements</a>"

;; M-: (sboo-html-element-format "a" :inner "HTML Elements" :attributes '(:href "https://developer.mozilla.org/en-US/docs/Web/HTML/Element"))
;;   ⇒ "<a href=\"https://developer.mozilla.org/en-US/docs/Web/HTML/Element\">HTML Elements</a>"

;; M-: (sboo-html-element-format "textarea" :inner "A readonly <textarea>." :attributes '(:name "textarea" :rows 5 :cols 68 :readonly nil))
;;   ⇒ <textarea name="textarea" rows="5" cols="68" readonly>A readonly &lt;textarea&gt;.</textarea>

;;----------------------------------------------;;

(cl-defun sboo-html-element-format-open (element &key attributes)

  "Format ELEMENT as an open tag.

Inputs:

• ELEMENT — a `stringp'.

Output:

• a `stringp'.

Example:

• M-: (sboo-html-element-format-open \"a\")
    ⇒ \"<a>\"

Related:

• `sboo-html-element-format-close"

  (let* ((ATTRIBUTES-PLIST  (or attributes '()))
         (ATTRIBUTES-STRING (sboo-html-attributes-format ATTRIBUTES-PLIST))
         )

    (if (and ATTRIBUTES-STRING (not (equal "" ATTRIBUTES-STRING)))
        (format "<%s %s>" element ATTRIBUTES-STRING)
      (format "<%s>" element))))

;; M-: (sboo-html-element-format-open "a")
;;   ⇒ "<a>"

;; M-: (sboo-html-element-format-open "a" :attributes '(:href "https://developer.mozilla.org/en-US/docs/Web/HTML/Element"))
;;   ⇒ "<a href=\"https://developer.mozilla.org/en-US/docs/Web/HTML/Element\">"

;;----------------------------------------------;;

(defun sboo-html-element-format-close (element)

  "Format ELEMENT as an closed tag.

Inputs:

• ELEMENT — a `stringp'.

Output:

• a `stringp'.

Example:

• M-: (sboo-html-element-format-close \"a\")
    ⇒ \"</a>\"

Related:

• `sboo-html-element-format-open"

  (format "</%s>" element))

;;----------------------------------------------;;

(defun sboo-html-attributes-format (attributes)

  "Format ATTRIBUTES via `sboo-html-attribute-format'.

Inputs:

• ELEMENT — a “`plistp'“ (a Property List).

Output:

• a `stringp'.

Example:

• M-: (sboo-html-attributes-format '(:readonly nil :rows 10 :wrap \"soft\" :spellcheck \"true\"))
    ⇒ \"readonly ...\"
"

  (let* ((LIST (cl-loop for     (k v)
                        on      attributes
                        by      'cddr
                        collect (sboo-html-attribute-format (sboo-html--keyword-name k) v)
                        ))
                         
         (STRING (string-join LIST " "))
         )

    STRING))

;; M-: (sboo-html-attributes-format '(:readonly nil :rows 10 :wrap "soft" :spellcheck t))
;;   ⇒ "readonly rows=\"10\" wrap=\"soft\" spellcheck=\"true\""

;;----------------------------------------------;;

(defun sboo-html-attribute-format (key &optional value)

  "Format an HTML Attribute.

Inputs:

• KEY   — a `stringp'.
• VALUE — a `stringp'.

Output:

• a `stringp'.

Example:

• M-: (sboo-html-attribute-format \"readonly\")
    ⇒ \"readonly\"

• M-: (sboo-html-attribute-format \"rows\" 10)
    ⇒ \"rows=\"10\"\"
"

  (let* ((KEY   key)
         (VALUE (if value
                    (format "=%S" (sboo-html-attribute-render-value value)) ;TODO escape HTML, not just Elisp.
                  ""))
         )

  (format "%s%s" KEY VALUE)))

;; M-: (sboo-html-attributes-format '(:readonly nil :rows 10 :wrap "soft" :spellcheck t))
;;   ⇒ "readonly rows=\"10\" wrap=\"soft\" spellcheck=\"true\""

;;----------------------------------------------;;

(defun sboo-html-attribute-render-value (object)

  "Render elisp object OBJECT as an HTML Attribute.

Example:

• M-: (sboo-html-attribute-render-value nil)
    ⇒ nil
    ; TODO or \"false\"?
• M-: (sboo-html-attribute-render-value t)
    ⇒ \"true\"
• M-: (sboo-html-attribute-render-value 10)
    ⇒ \"10\"
• M-: (sboo-html-attribute-render-value \"on\")
    ⇒ \"on\"
"
  (pcase object

    ('nil nil)
    ('t   "true")

    (_

     (pcase (type-of object)

       ('string  (substring-no-properties object))
       ('symbol  (symbol-name object))
       ('integer (format "%s" object)) 
       ('float   (format "%s" object))

       (_ nil)))))

;;----------------------------------------------;;
;; Functions: `compile' ------------------------;;
;;----------------------------------------------;;
 
(cl-defun sboo-html-set-compile-command (&key buffer)

  "Set `compile-command' to to `sboo-html-get-compile-command'.

See:

• `sboo-html-set-compile-command'"

  (when-let* ((COMPILE-COMMAND (sboo-html-get-compile-command :buffer buffer))
              )

    (setq-local compile-command COMPILE-COMMAND)

    ;;TODO add `sboo-html-set-compilation-search-paths' to `compile' hook, conditioned on html:

    ()))

;; TODO! default `compile-command's for: index.html (`browse-url-of-file'), Test.hs, TestSuite.hs, DocTest.hs, UnitTest.hs (test-suite), PropTest.hs, Bench.hs, Benchmark.hs, BenchTime.hs, BenchSpace.hs (bench), ForeignLibrary.hs, SO.hs, DLL.hs, DYLIB.hs (foreign-library).

;;----------------------------------------------;;

(cl-defun sboo-html-get-compile-command (&key buffer)

  "Guess a `compile-command' for an HTML file.

Uses:

• the interpreter — a “shebang”s of the program `xdg-open', (e.g. « #!/bin/env open »), program `open', program `firefox', and program `google-chrome' (c.f. `interpreter-mode-alist').
• File-Local Variables —  (c.f. `file-local-variables-alist', automatically set by « .dir-locals.el » ancestors, « ;;; -*- ... -*- » lines near the top of the buffer, and « ;; Local Variables: ;; ... ;; End: » lines near the bottom.)
• the “project” — guessed by a dominating `index.html' (c.f. `locate-dominating-file').

Links:

• URL `'"

  ;TODO get component from .dir-locals.el, like « sboo-cabal-target ».
  ;TODO get project-root from locate-dominating-file cabal.project.
  ;TODO get default-directory from subdirectory of project-root

  (let* ((BUFFER (or buffer (current-buffer)))
         (FILE   (buffer-file-name BUFFER))

         (COMPILE-COMMAND

          (pcase (sboo-html-guess-compile-command :buffer buffer)

            ('cabal (sboo-html-compile-command-cabal-script :file FILE))
            ('stack (sboo-html-compile-command-stack-script :file FILE))

            (_      sboo-html-compile-command-default)))
         )

    COMPILE-COMMAND))

;;----------------------------------------------;;
;; Configuration -------------------------------;;
;;----------------------------------------------;;

(defun sboo-html-wrap-region-table ()

  "Creates an argument for `wrap-region-add-wrappers'."

  (let* ((ELEMENTS (sboo-html-elements))
         (TABLE    (mapcar #'sboo-html-wrap-region-entry ELEMENTS))
         )

    TABLE))

;; M-: (sboo-html-wrap-region-table)
;;

;;----------------------------------------------;;

(cl-defun sboo-html-wrap-region-entry (element &key alias)

  "Creates an “arglist” for `wrap-region-add-wrapper'."

  (let* ((KEY   (or alias element))
         (LEFT  (sboo-html-element-format-open  element))
         (RIGHT (sboo-html-element-format-close element))
         (MODES sboo-html-modes-list)
         )

    (list LEFT RIGHT KEY MODES)))

;; M-: (sboo-html-wrap-region-entry "code" :alias "c")
;;   ⇒ '("<code>" "</code>" "c" (html-mode mhtml-mode markdown-mode gfm-mode))

;;----------------------------------------------;;

(defun sboo-markdown-wrap-region-table ()

  "Creates an argument for `wrap-region-add-wrappers'."

  (let* ((BINDINGS sboo-markdown-wrappers-alist)
         (TABLE    (mapcar #'sboo-markdown-wrap-region-entry BINDINGS))
         )

    TABLE))

;; M-: (sboo-markdown-wrap-region-table)
;;

;;----------------------------------------------;;

(cl-defun sboo-markdown-wrap-region-entry (left &key right key)

  "Creates an “arglist” for `wrap-region-add-wrapper'."

  (let* ((KEY   (or key left))
         (LEFT  left)
         (RIGHT (or right left))
         (MODES sboo-markdown-modes-list)
         )

    (list LEFT RIGHT KEY MODES)))

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-html--keyword-name (keyword)

  "Render KEYWORD.

Inputs:

• KEYWORD — a `keywordp'.

Output:

• a `stringp'.

Example:

• M-: (sboo-html--keyword-name :href)
    ⇒ \"href\"

Related:

• `symbol-name'"

  (pcase (type-of keyword)

    ('symbol (let ((S (symbol-name keyword))
                   )
               (string-remove-prefix ":" S)))

    (_ nil)))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; M-: 
;;   ⇒ <textarea name="textarea" rows="5" cols="68" readonly>I am a readonly textarea</textarea>

;;TODO
;;
;; & becomes &amp;
;; < becomes &lt;
;; > becomes &gt;
;;
;; Inside of attribute values you must also escape the quote character you're using:
;;
;; " becomes &quot;
;; ' becomes &#39;

;; ^ “`plist-map'”
;;
;;   (cl-loop for (key value) on '(:prop1 a :prop2 b :prop3 c) by 'cddr
;;            collect value)
;;   ;;; (a b c)
;;
;; M-: (cl-loop for (key value) on '(:prop1 a :prop2 b :prop3 c) by 'cddr collect value)
;;  ⇒ '(a b c)
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-html)

;;; sboo-html.el ends here