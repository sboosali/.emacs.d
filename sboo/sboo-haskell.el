;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for the Haskell programming language.
;;
;; See:
;;
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

;; (require 'cl)
;; (require 'pcase)

;;

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defcustom sboo-haskell-eldoc nil

  "Which type-provider `sboo-haskell-doc-current-info' will call.

Each symbol represents a particular type (/ info / docs / etc) provider."

  :type '(choice (const nil)
                 (const dante
                        :tag "`dante-type-at'"))

  :safe t

  :group 'sboo)

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
(á la Applicative Programming with Effects)."

  :type '(alist :key-type   (string    :tag "String to match")
                :value-type (character :tag "Char to show")
                )

  :safe t

  :group 'sboo)

;;----------------------------------------------;;

(defun sboo-haskell-set-compile-command ()

  "Set `compile-command' from « .dir-locals.el » and « .project ».

Files:

• the file-buffer's « .dir-locals.el » file.
• the dominating « .project » file.
• the dominating « .cabal » file."

  (setq-local compile-command (concat "dmd -de -w -unittest ";TODO
                                      buffer-file-name)))

;(add-hook 'd-mode-hook #'sboo-haskell-set-compile-command)))

;;----------------------------------------------;;
;; ElDoc ---------------------------------------;;
;;----------------------------------------------;;

(defun sboo-haskell-doc-current-info ()

  "Custom `haskell-doc-current-info'."

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

(defun haskell-mode-after-save-handler () (progn))

 ;; ^ HACK fixes this pseudo-error:
 ;;
 ;;     Error running timer ‘real-auto-save-buffers’: (void-function haskell-mode-after-save-handler)
 ;; 

;;----------------------------------------------;;
;; Dante ---------------------------------------;;
;;----------------------------------------------;;

(defcustom sboo-dante-default-method 'new-impure-nix

  "Default key of `dante-methods-alist'."

  :type  '(symbol :tag "How `dante' launches GHCi.")
  :safe  t
  :group 'sboo-haskell)

;;==============================================;;

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
  
  (i.e. `dante-mode' or `dante-restart').
  "
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
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
(provide 'sboo-haskell)