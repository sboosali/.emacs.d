;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for the Haskell programming language.
;;
;; See:
;;
;; * `sboo-haskell-compile-command'
;; * `sboo-haskell-eldoc'
;; * `sboo-dante-cabal-new-repl'
;; * `'
;;
;; 
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

;; (require 'cl-lib)
;; (require 'pcase)

;;

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-haskell

  nil

  "Personal Haskell customization."

  :prefix "sboo-haskell-"

  :group 'sboo
  :group 'haskell)

;;==============================================;;

(defcustom sboo-haskell-eldoc nil

  "Which type-provider `sboo-haskell-doc-current-info' will call.

Each symbol represents a particular type (/ info / docs / etc) provider."

  :type '(choice (const nil)
                 (const dante
                        :tag "`dante-type-at'"))

  :safe t

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

  "`prettify-symbols-alist' for Haskell.

Prettify some Applicative methods 
(á la Applicative Programming with Effects).

Extends `haskell-font-lock-symbols-alist'.

Links:

• URL `http://xahlee.info/comp/unicode_arrows.html'"

  :type '(alist :key-type   (string    :tag "String to match")
                :value-type (character :tag "Char to show")
                )

  :safe t
  :group 'sboo-haskell)

;;----------------------------------------------;;

(defcustom sboo-haskell-quasi-quote-alist

  `(
    ("bash" . shell-mode)
    ("sh"   . shell-mode)
    ("html" . html-mode)
    ("css"  . css-mode)
    ("js"   . javascript-mode)
   )

  "Mapping from QuasiQuoter to fontification mode.

For example, given an association « '(\"html\" . html-mode) », 
and given an expression « [html| <div>...</div> |] »,
then « <div>...</div> » will be fontified by « `html-mode' ».

Extends `haskell-font-lock-quasi-quote-modes'."

  :group 'sboo-haskel

  :safe t
  :type '(repeat (cons string symbol)))

;;----------------------------------------------;;

(defcustom sboo-haskell-hooks-list

  (list #'sboo-haskell-prettify-symbols
        #'sboo-haskell-set-compile-command
        #'subword-mode
        )

  "Hooks for `haskell-mode'.

Zero-or-more function-symbols."

  :type '(repeat (function))

  :safe t
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

  :safe  t
  :group 'sboo-haskell)

;;----------------------------------------------;;

(defcustom sboo-dante-method-default

  (car sboo-dante-methods)

  "Default key of `dante-methods-alist'.

How `dante' will launch GHCi."

  :type  '(symbol :tag "`dante-methods-alist' key.")
  :safe  t
  :group 'sboo-haskell)

;;==============================================;;

(defvar sboo-dante-display-buffer

  `( ,(rx bos "*dante:")
     (display-buffer-reuse-window display-buffer-in-side-window)
   )

  "TODO This display rule tells Emacs to never bring-to-front the internal dante buffer.")

;; e.g. a `dante' buffer: « *dante:spiros:lib:spiros:~/haskell/spiros/* »

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-haskell-set-compile-command ()

  "Set `compile-command' from « .dir-locals.el » and « .project ».

Files:

• the file-buffer's « .dir-locals.el » file.
• the dominating « .project » file.
• the dominating « .cabal » file."

  ;TODO get component from .dir-locals.el, like « sboo-cabal-target ».
  ;TODO get project-root from locate-dominating-file cabal.project.
  ;TODO get default-directory from subdirectory of project-root

  (setq-local compile-command
              (format-message "%s"
                              "cabal new-build all")))

;;----------------------------------------------;;
;; ElDoc ---------------------------------------;;
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
;; Hacks ---------------------------------------;;
;;----------------------------------------------;;

;;TODO rm (defun haskell-mode-after-save-handler () (progn))

 ;; ^ HACK fixes this pseudo-error:
 ;;
 ;;     Error running timer ‘real-auto-save-buffers’: (void-function haskell-mode-after-save-handler)
 ;; 

;;----------------------------------------------;;
;; Dante ---------------------------------------;;
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

;;==============================================;;

(defun sboo-dante-mode ()
  
  "Start/restart `dante'.
  
(i.e. `dante-mode' or `dante-restart')."

  (interactive)

  (if (bound-and-true-p 'dante-mode)

    (dante-restart)

   (dante-mode 1)))

;;----------------------------------------------;;

(defun sboo-haskell-prettify-symbols ()

  (interactive)

  (if prettify-symbols-mode

      (prettify-symbols-mode 0)

    (progn
      (setq-local prettify-symbols-alist sboo-haskell-prettify-symbols-alist)

      (prettify-symbols-mode 1))))

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
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; ERROR "Couldn't guess that module name. Does it exist?"
;;
;; « -fdefer-type-errors »

;; 

;;----------------------------------------------;;
(provide 'sboo-haskell)