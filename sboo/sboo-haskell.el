;;; sboo-haskell.el --- -*- coding: utf-8; lexical-binding: t -*-

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

;; Personal configuration for the Haskell programming language.
;;
;; See:
;;
;; • `sboo-haskell-compile-command'
;; • `sboo-dante-mode'
;; • `sboo-haskell-eldoc'
;; 
;; Libraries:
;; 
;; • `haskell'
;; • `dante'
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile
  (require 'rx)
  (require 'cl-lib))

;;----------------------------------------------;;

(progn
  (require 'pcase)
  (require 'seq))

;;----------------------------------------------;;
;; Customization -------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-haskell nil

  "Personal Haskell customization."

  :prefix 'haskell

  :group 'sboo
  :group 'haskell)

;;----------------------------------------------;;

(defgroup sboo-coq nil

  "Personal Coq customization."

  :prefix 'coq

  :group 'sboo
  :group 'coq)

;;==============================================;;

(defcustom sboo-haskell-compile-command-default

  (format-message "%s"
                  "cabal new-build all")

  "Default `compile-command' for `haskell-mode' buffers.

a `stringp'."

  :type '(string :tag "Command-Line")

  :safe #'stringp
  :group 'sboo-haskell)

;;----------------------------------------------;;

(defcustom sboo-haskell-prettify-symbols-alist

  '(("::"     . ?∷)
    ("=>"     . ?⇒)
    ("->"     . ?→)
    ("<-"     . ?←)
    ("forall" . ?∀)

    ("=="     . ?≡)
    ("/="     . ?≠)
    ("<="     . ?≤)
    (">="     . ?≥)

    ("-:"     . ?⫣)
    ("=:"     . ?⫤)

    ;; ("<>"     . ?)

    ;; ("<$>"    . ?)
    ;; ("<*>"    . ?⊗)
    ;; ("<|>"    . ?)
    ;; ("<+>"    . ?⊕)

    ;; (">>"     . ?)
    ;; ("<<"     . ?)
    ;; (">>="    . ?)
    ;; ("=<<"    . ?)

    ;;(""     . ?)
    )

  "`prettify-symbols-alist' for `haskell-mode'.

Prettifies:

• some Arithmetic/Logical operations.
• some Applicative methods
(á la Applicative Programming with Effects).
• some other identifiers.

Extends `haskell-font-lock-symbols-alist'.

Links:

• URL `http://xahlee.info/comp/unicode_arrows.html'"

  :type '(alist :key-type   (string    :tag "String (to match)")
                :value-type (character :tag "Char   (to show) ")
                )

  :safe #'listp
  :group 'sboo-haskell)

;; (defcustom haskell-font-lock-symbols-alist
;;   '(("\\" . "λ")
;;     ("not" . "¬")
;;     ("->" . "→")
;;     ("<-" . "←")
;;     ("=>" . "⇒")
;;     ("()" . "∅")
;;     ("==" . "≡")
;;     ("/=" . "≢")
;;     (">=" . "≥")
;;     ("<=" . "≤")
;;     ("!!" . "‼")
;;     ("&&" . "∧")
;;     ("||" . "∨")
;;     ("sqrt" . "√")
;;     ("undefined" . "⊥")
;;     ("pi" . "π")
;;     ("~>" . "⇝") ;; Omega language
;;     ;; ("~>" "↝") ;; less desirable
;;     ("-<" . "↢") ;; Paterson's arrow syntax
;;     ;; ("-<" "⤙") ;; nicer but uncommon
;;     ("::" . "∷")
;;     ("." "∘" ; "○"
;;      ;; Need a predicate here to distinguish the . used by
;;      ;; forall <foo> . <bar>.
;;      haskell-font-lock-dot-is-not-composition)
;;     ("forall" . "∀"))
;;   "Alist mapping Haskell symbols to chars.

;; Each element has the form (STRING . COMPONENTS) or (STRING
;; COMPONENTS PREDICATE).

;; STRING is the Haskell symbol.
;; COMPONENTS is a representation specification suitable as an argument to
;; `compose-region'.
;; PREDICATE if present is a function of one argument (the start position
;; of the symbol) which should return non-nil if this mapping should
;; be disabled at that position."
;;   :type '(alist string string)
;;   :group 'haskell-appearance)

;;----------------------------------------------;;

(defcustom sboo-coq-prettify-symbols-alist

  '(("|-"     . ?⊢)
    ("True"   . ?⊤)
    ("False"  . ?⊥)
    ("->"     . ?→)
    ("-->"    . ?⟶)
    ("<-"     . ?←)
    ("<--"    . ?⟵)
    ("<->"    . ?↔)
    ("<-->"   . ?⟷)
    ("==>"    . ?⟹)
    ("<=="    . ?⟸)
    ("++>"    . ?⟿)
    ("<++"    . ?⬳)
    ("fun"    . ?λ)
    ("forall" . ?∀)
    ("exists" . ?∃)
    ("/\\"    . ?∧)
    ("\\/"    . ?∨)
    ("~"      . ?¬)
    ("+-"     . ?±)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("<>"     . ?≠)
    ("*"      . ?×)
    ;;
    ("++"     . ?⧺)
    ("nat"    . ?𝓝)
    ("Z"      . ?ℤ)
    ("N"      . ?ℕ)
    ("Q"      . ?ℚ)
    ("Real"   . ?ℝ)
    ("bool"   . ?𝔹)
    ("Prop"   . ?𝓟)
    )

  "`prettify-symbols-alist' for Coq.

Extends `coq-font-lock-symbols-alist'."

  :type '(alist :key-type   (string    :tag "String (to match)")
                :value-type (character :tag "Char   (to show) ")
                )

  :safe #'listp
  :group 'sboo-coq)

;;----------------------------------------------;;

(defcustom sboo-haskell-font-lock-alist

  '(
    (,(rx word-start (group-n 1 "TODO") word-end) 1 font-lock-warning-face t)
    ;; ("" . font-lock-)
    ;; ("" . font-lock-)
    ;; ("" . font-lock-)
   )

  "Extra keywords (to fontify) for Haskell files.

Associates regexps with faces. See `font-lock-add-keywords' 

(This only affects display, not navigation.)"

  :type '(alist :key-type   (regexp)
                :value-type (string :tag "Face"))

  :safe #'listp
  :group 'sboo-haskell)

;; M-: (rx word-start (group-n 1 "TODO") word-end)
;;   ⇒ "\\<\\(?1:TODO\\)\\>"

;;----------------------------------------------;;

(defcustom sboo-haskell-quasi-quote-alist

  `(

    ("sql"  . sql-mode)

    ("html" . html-mode)
    ("css"  . css-mode)
    ("js"   . javascript-mode)
    ("json" . json-mode)

    ("bash" . shell-mode)
    ("sh"   . shell-mode)

    ;; ("hsx" . xml-mode)
    ;; ("xmlQQ" . xml-mode)
    ;; ("xml" . xml-mode)
    ;; ("cmd" . shell-mode)
    ;; ("sh_" . shell-mode)
    ;; ("jmacro" . javascript-mode)
    ;; ("jmacroE" . javascript-mode)
    ;; ("r" . ess-mode)
    ;; ("rChan" . ess-mode)
    ;; ("aesonQQ" . json-mode)
   )

  "Mapping from QuasiQuoter to fontification mode.

For example, given an association « '(\"html\" . html-mode) », 
and given an expression « [html| <div>...</div> |] »,
then « <div>...</div> » will be fontified by « `html-mode' ».

Extends `haskell-font-lock-quasi-quote-modes'."

  :type '(repeat (cons string symbol))

  :safe #'listp
  :group 'sboo-haskell)

;;----------------------------------------------;;

(defcustom sboo-haskell-hooks-list

  (list #'sboo-haskell-set-compile-command
        #'sboo-haskell-prettify-symbols
        #'superword-mode
 ;;TODO #'sboo-haskell-font-lock-add-keywords
        )

  "Hooks for `haskell-mode'.

Zero-or-more function-symbols."

  :type '(repeat (function))

  :safe #'listp
  :group 'sboo-haskell)

;;; sub word mode lets you navigate (e.g. M-b) between "sub words" of a camelcased word

;;----------------------------------------------;;

(defcustom sboo-dante-methods

  '( ;;new-impure-nix
     new-build
     stack
     )

  "Susbset of `dante-methods-alist' keys.

How `dante' will truy to launch GHCi."

  :type  '(repeated (symbol :tag "`dante-methods-alist' key."))

  :safe #'listp
  :group 'sboo-haskell)

;;----------------------------------------------;;

(defcustom sboo-dante-method-default

  (car sboo-dante-methods)

  "Default key of `dante-methods-alist'.

How `dante' will launch GHCi."

  :type '(symbol :tag "`dante-methods-alist' key.")

  :safe #'symbolp
  :group 'sboo-haskell)

;;----------------------------------------------;;

(defcustom sboo-haskell-eldoc nil

  "Which type-provider `sboo-haskell-doc-current-info' will call.

Each symbol represents a particular type (/ info / docs / etc) provider."

  :type '(choice (const nil)
                 (const dante
                        :tag "`dante-type-at'"))

  :safe #'symbolp
  :group 'sboo-haskell)

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-dante-display-buffer

  `( ,(rx bos "*dante:")
     (display-buffer-no-window)
   )

  "Display Rule which tells Emacs to never bring-to-front the internal dante buffer.")

;; e.g. a `dante' buffer: « *dante:spiros:lib:spiros:~/haskell/spiros/* »

;;----------------------------------------------;;
;; Faces ---------------------------------------;;
;;----------------------------------------------;;

;; (defface sboo-haskell-keyword-face
;;   '((t (:foreground "yellow")))
;;   "Custom face for `haskell-keyword-face'."
;;   :group 'haskell)

;; (defface sboo-haskell-constructor-face
;;   '((t (:background "red")))
;;   "Custom face for `haskell-constructor-face'."
;;   :group 'haskell)

;; ;; TODO `setq` doesn't work for faces:

;; (setq haskell-keyword-face 'sboo-haskell-keyword-face)
;; (setq haskell-constructor-face 'sboo-haskell-constructor-face)

;;----------------------------------------------;;
;; Functions: `font-lock' ----------------------;;
;;----------------------------------------------;;

(cl-defun sboo-haskell-font-lock-add-keywords (&key (mode nil))

  "Call `font-lock-add-keywords'.

Examples:

• M-: (sboo-haskell-font-lock-add-keywords :mode 'haskell-mode)
• M-: (sboo-haskell-font-lock-add-keywords :mode nil)
"

  (let* ((KEYWORD-ALIST sboo-haskell-font-lock-alist)
         (KEYWORD-LIST  KEYWORD-ALIST)
         )

    (font-lock-add-keywords mode KEYWORD-LIST)))

;; e.g. '("\\<\\(FIXME\\):" 1 font-lock-warning-face t)

;;----------------------------------------------;;
;; Functions: `compile' ------------------------;;
;;----------------------------------------------;;
 
(cl-defun sboo-haskell-set-compile-command (&key buffer)

  "Set `compile-command' to to `sboo-haskell-get-compile-command'.

See:

• `sboo-haskell-set-compile-command'"

  (let* ((COMPILE-COMMAND (sboo-haskell-get-compile-command :buffer buffer))
         )

    (setq-local compile-command COMPILE-COMMAND)

    ;;TODO add `sboo-haskell-set-compilation-search-paths' to `compile' hook, conditioned on haskell:

    ()))

;; TODO! default `compile-command's for: Main.hs (executable), Test.hs, TestSuite.hs, DocTest.hs, UnitTest.hs (test-suite), PropTest.hs, Bench.hs, Benchmark.hs, BenchTime.hs, BenchSpace.hs (bench), ForeignLibrary.hs, SO.hs, DLL.hs, DYLIB.hs (foreign-library).

;;----------------------------------------------;;

(cl-defun sboo-haskell-get-compile-command (&key buffer)

  "Guess a `compile-command' for a Haskell file.

Uses:

• the « #!/bin/env cabal » “shebang”.
• the file-buffer's « .dir-locals.el » file.
• the dominating « .project » file.
• the dominating « .cabal » file.

Links:

• `interpreter-mode-alist'"

  ;TODO get component from .dir-locals.el, like « sboo-cabal-target ».
  ;TODO get project-root from locate-dominating-file cabal.project.
  ;TODO get default-directory from subdirectory of project-root

  (let* ((FILE (buffer-file-name buffer))

         (COMPILE-COMMAND

          (pcase (sboo-haskell-guess-compile-command :buffer buffer)

            ('cabal (sboo-haskell-compile-command-cabal-script :file FILE))
            ('stack (sboo-haskell-compile-command-stack-script :file FILE))

            (_      sboo-haskell-compile-command-default)))
         )

    COMPILE-COMMAND))

;;----------------------------------------------;;

(cl-defun sboo-haskell-set-compilation-search-path ()

  "Extend `compilation-search-path' (with subdirectories which have a Haskell package).

See:

• `sboo-haskell-get-compilation-search-paths'"

  (interactive)

  (let* ((COMPILE-PATHS (sboo-haskell-get-compilation-search-paths))
        )

    (setq-local compilation-search-path (append compilation-search-path COMPILE-PATHS))

    compilation-search-path))

;;----------------------------------------------;;

(cl-defun sboo-haskell-guess-compile-command (&key buffer)

  "Guess which kind of compiler should compile the Haskell BUFFER.

Inputs:

• BUFFER — a `bufferp'.
  Defaults to `current-buffer'.

Output:

• a `symbolp' or
  One of:

    • `nil'
    • `cabal'
    • `stack'
    • `ghc' (TODO)
    • `ghcjs' (TODO)"

  (let* ((BUFFER (or buffer (current-buffer)))
         )

    (with-current-buffer BUFFER

      (let* ((STRING

              ;; (copy-pasted from `set-auto-mode'):

              (save-excursion
		(goto-char (point-min))
		(if (looking-at auto-mode-interpreter-regexp)
		    (match-string 2))))

             (SYMBOL (intern-soft STRING))
             )

        SYMBOL))))

;;----------------------------------------------;;

(cl-defun sboo-haskell-compile-command-cabal-script (&key file)

  "A `compile-command' that `cabal-run's FILE.

Inputs:

• FILENAME — a `stringp'.
  Defaults to `buffer-file-name'.

Output:

• a `stringp'."

  (let* ((FILE (or file buffer-file-name (read-file-name "Haskell File: ")))
         )

    (format-message "%s %s"
                    "cabal new-run"
                    FILE)))

;;----------------------------------------------;;

(cl-defun sboo-haskell-compile-command-stack-script (&key file)

  "A `compile-command' that runs FILE via `stack-script'.

Inputs:

• FILENAME — a `stringp'.
  Defaults to `buffer-file-name'.

Output:

• a `stringp'."

  (let* ((FILE (or file buffer-file-name (read-file-name "Haskell File: ")))
         )

    (format-message "%s %s"
                    "stack script"
                    FILE)))

;;----------------------------------------------;;

(cl-defun sboo-haskell-get-compilation-search-paths (&key root)

  "Extra `compilation-search-path's, given project-directory ROOT.

Inputs:

• ROOT — a `stringp'.
  Defaults to `sboo-haskell-guess-project-root'.

Output:

• a `stringp'."

  (let* ((ROOT (or root
                   (sboo-haskell-guess-project-root)))

         (SUBDIRECTORIES (directory-files ROOT)) ; TODO strip ./dist( » and version-control directories.
          All (recursive) subdirectories, of the project root directory, with a « .cabal » file. 
         )

    SUBDIRECTORIES))

;;----------------------------------------------;;

(cl-defun sboo-haskell-guess-project-root (&key file)

  "Guess (the directory of) the project which FILE is part of.

Inputs:

• FILE — a `stringp'.
  a filepath.

Output:

• a `stringp'.
  a filepath, a directory which is an ancestor of FILE.

Implementation:

• Tries `dante-project-root' (from the `dante' package, if available).
• Tries `intero-project-root' (from the `intero' package, if available).
• Tries `projectile-project-root' (from the `projectile' package, if available).
• Calls function `sboo-haskell-locate-dominating-project-directory'.
• Defaults to `default-directory'."

  (or (bound-and-true-p dante-project-root)
      (bound-and-true-p intero-project-root)
      (bound-and-true-p projectile-project-root)
      (sboo-haskell-locate-dominating-project-directory :directory (file-name-directory file))
      default-directory))

;;----------------------------------------------;;

(cl-defun sboo-haskell-locate-dominating-project-directory (&key directory)

  "« dirname » of `'sboo-haskell-locate-dominating-project-file'."

  (file-name-directory (sboo-haskell-locate-dominating-project-file :directory directory)))

;;----------------------------------------------;;

(cl-defun sboo-haskell-locate-dominating-project-file (&key directory)

  "`locate-dominating-file' for « cabal.project » and « stack.yaml »."

  (let ((DIRECTORY (or directory default-directory))
        )

  (or (locate-dominating-file DIRECTORY "cabal.project")
      (locate-dominating-file DIRECTORY "stack.yaml")
      (locate-dominating-file DIRECTORY "package.yaml")
      (locate-dominating-file DIRECTORY #'sboo-haskell--has-cabal-file-p))))

;;----------------------------------------------;;
;; Functions: Aesthetic ------------------------;;
;;----------------------------------------------;;

(defun sboo-haskell-prettify-symbols ()

  "Extend `prettify-symbols-alist' with `sboo-haskell-prettify-symbols-alist'."

  (interactive)

  (progn

    (setq-local prettify-symbols-alist sboo-haskell-prettify-symbols-alist)
      ;; (setq prettify-symbols-unprettify-at-point t)

    ;; toggle Prettify-Symbols:

    (if prettify-symbols-mode
  
        (prettify-symbols-mode 0)

      (prettify-symbols-mode +1))))

;;----------------------------------------------;;
;; Functions: `eldoc' --------------------------;;
;;----------------------------------------------;;

(defun sboo-haskell-doc-current-info ()

  "Custom `haskell-doc-current-info'."

  (interactive)

  (pcase sboo-haskell-eldoc
    
    ('dante

     (when (commandp #'dante-type-at)
       (dante-type-at nil)))

    ;; ^ (`dante-type-at' `nil') only echoes (c.f. doesn't insert).

    (_

     nil)))

;; ^ `dante'+`eldoc' integration.
;;
;; NOTE disable if too slow.
;;
;; Alternatively:
;;
;; * `dante-info': more informative, pops-up a `*Help' buffer.
;; * `dante-type-at': more concise, prints to Echo Area (like `eldoc' should),
;;
;; 

;; TODO (make-local-variable 'eldoc-documentation-function) (setq eldoc-documentation-function 'scheme-get-current-symbol-info)

;;----------------------------------------------;;

(cl-defun sboo-add-help-echo (&key echo string region)

  "Add ECHO as « 'kbd-help » and « 'help-echo » display properties to STRING or REGION.

Inputs:

• ECHO   — a string (`stringp'). a message.
• STRING — a string (`stringp'),
• REGION — a buffer region (`consp').

Outputs:

• the object (STRING or REGION) that was propertized.

Examples:

• M-: (sboo-add-help-echo :echo \"help\" :region (cons (region-beginning) (region-end)))
• M-: (insert (sboo-add-help-echo :echo \"help-echo\" :string \"xyz\"))

Related:

• `put-text-property'"

  (let* ((OBJECT (or string region))
         (START (if string 0
                  (if region (car region)
                    (if (region-active-p) (region-beginning)
                      ()))))
         (END   (if string (length string)
                  (if region (cdr region)
                    (if (region-active-p) (region-end)
                      ()))))
         )

    (progn
      (put-text-property START END 'help-echo echo OBJECT)
      (put-text-property START END 'kbd-help  echo OBJECT)
      OBJECT)))

;; `help-echo' — a String or a Function. when the character is hovered over, emacs shows a Tooltip with the `help-echo` String (a Tooltip is either displayed in the Echo Area, or in a popup Tooltip Window). the Function has type `{window, object, position} -> String`.
;; (insert (sboo-add-help-echo :echo "help-echo" :string "xyz"))
;; (insert (propertize "xyz" 'help-echo "help-echo"))

;;----------------------------------------------;;
;; Functions: `dante' --------------------------;;
;;----------------------------------------------;;

(defun sboo-dante-cabal-new-repl (root)

  "Locate a `cabal.project' file in project-directory ROOT."

  (interactive)

  (when (or (directory-files root nil ".+\\.project$") (file-exists-p "cabal.project"))

    '("cabal" "new-repl" dante-target "--builddir=dist-dante")))

;;----------------------------------------------;;

(defvar sboo-dante-repl-command-line-methods-alist

  `((new-build . sboo-dante-cabal-new-repl)
    (stack     . ,(lambda (root) (dante-repl-by-file root '("stack.yaml") '("stack" "repl" dante-target))))
    (bare      . ,(lambda (_) '("cabal" "repl" dante-target "--builddir=dist/dante"))))

  "Override `dante-repl-command-line-methods-alist'.")

;;----------------------------------------------;;

(defun sboo-dante-mode ()
  
  "Start/restart `dante'.

Conditions:

• Doesn't start for non-file buffers.
  Thus, in `markdown-mode', we can `edit-indirect-region'
  a « ``` haskell ... ``` » code block,
  with syntax highlight via `haskell-mode',
  without spawning failing Dante processes
  (since `sboo-dante-mode' is registered with `haskell-mode-hook').

Related:

• `dante-mode' 
• `dante-restart'"

  (interactive)

  (when (buffer-file-name)

    (if (bound-and-true-p dante-mode)

        (progn
          (message "%s" "[sboo] Restarting `dante'...")

          (dante-restart))

      (progn
        (message "%s" "[sboo] Importing `dante' and `flycheck'...")

        (when (require 'flycheck nil :no-error)
          (flycheck-mode 1))

        (when (require 'dante nil :no-error)
          (dante-mode 1))

        t))))

;;----------------------------------------------;;
;; Functions: `syntax-table' -------------------;;
;;----------------------------------------------;;

(defun sboo-haskell-modify-syntax-entries ()

  "Modify `haskell-mode-syntax-table'.

Effects:

• the apostrophe should have:

    • the ”Symbol” Syntax-Category — « ' »  can be within Haskell-Identifiers.
      e.g. « doCheck » and « don'tCheck ».
    • the ”Prefix-Character” Syntax-Flag — « ' » is the Quote-Operator in « -XTemplateHaskellQuotes » (like LISP).
      e.g. « makeLenses ''Point »."

  (progn

    (modify-syntax-entry ?\' "_ p" haskell-mode-syntax-table)

    haskell-mode-syntax-table))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-haskell-new-module (&optional file-name module-name)

  "Create a.

Inputs:

• FILE-NAME — a `stringp'.
• MODULE-NAME — a `stringp'.

Output:

• a .

Example:

• M-: (sboo-haskell-new-module)
    ⇒ 

Links:

• URL `'

Related:

• `haskell-guess-module-name-from-file-name'"

  (cl-check-type file-name   string)
  (cl-check-type module-name string)

  (let* ((FILE-NAME   (or file-name   (read-file-name "New Haskell File (e.g. « A/B.hs »): ")))
         (MODULE-NAME (or module-name (or (haskell-guess-module-name-from-file-name FILE-NAME)
                                          (read-string "Haskell Module (e.g. « A.B »): "))))
         )

    ))

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-haskell--has-cabal-file-p (directory)

  "Does DIRECTORY have a « .cabal » file?"

  (cl-find-if #'sboo-haskell--is-cabal-file-p
              (directory-files directory)))

;;----------------------------------------------;;

(defun sboo-haskell--is-cabal-file-p (file)

  "Is FILE a « .cabal » file?

Examples:

• M-: (sboo-haskell--is-cabal-file-p \"example.cabal\")
  t
• M-: (sboo-haskell--is-cabal-file-p \"example.yaml\")
  nil"

  (if (string-match-p (rx (1+ any) ".cabal" eos) file)
      t
    nil))

;;----------------------------------------------;;
;; Hacks ---------------------------------------;;
;;----------------------------------------------;;

;;TODO rm (defun haskell-mode-after-save-handler () (progn))

 ;; ^ HACK fixes this pseudo-error:
 ;;
 ;;     Error running timer ‘real-auto-save-buffers’: (void-function haskell-mode-after-save-handler)
 ;; 

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; ERROR "Couldn't guess that module name. Does it exist?"
;;
;; « -fdefer-type-errors »

;; `directory-files-recursively':
;;
;; (directory-files-recursively DIRECTORY REGEXP &optional INCLUDE-DIRECTORIES)

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'sboo-haskell)

;;; sboo-haskell.el ends here