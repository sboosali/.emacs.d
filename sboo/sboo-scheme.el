;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; Configuration for the Haskell programming language.
;;
;; See:
;;
;; * `sboo-scheme-compile-command'
;; * `sboo-scheme-'
;; 

;;==============================================;;
;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; builtins:

(require 'cl-lib)
(require 'pcase)
(require 'rx)

;;----------------------------------------------;;
;; Macros --------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Types ---------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup sboo-scheme

  nil

  "Personal Scheme customization."

  :prefix 'sboo

  :group 'sboo
  :group 'scheme)

;;==============================================;;

(defcustom sboo-scheme-compile-command

  nil

  "Command to build « .scheme » files.

Type:

• a `stringp'.
• a `listp's of `stringp's.
• a nullary `functionp' that evaluates to a `stringp'.
• a form that evaluates to a `stringp'.

Usage:

• Set as a file-local variable (i.e. in « .dir-locals.el »).
• Set as a custom variable (i.e. `custom-set-variables' »)."

  :type '(choice (const nil       :tag "Default (`sboo-scheme-compile-command-default')")
                 (string          :tag "Command-Line")
                 (repeated string :tag "Command and Arguments (arguments will be shell-escaped)")
                 (variable        :tag "Variable (quoted, a `stringp')")
                 (function        :tag "Function (no non-optional input, `stringp' output)")
;                (restricted-sexp :match-alternatives :tag "S-Expression (quoted, starts with « (`eval' ...) »")
          )

  :safe #'stringp
  :group 'sboo-scheme)

;;----------------------------------------------;;

(defcustom sboo-scheme-hooks-list

  (list #'sboo-scheme-set-compile-command
        #'subword-mode
        )

  "Hooks for `scheme-mode'.

Type:

• `listp' of nullary functions.

Zero-or-more function-symbols."

  :type '(repeat (function :tag "Callback"))

  :safe t
  :group 'sboo-scheme)

;;; sub word mode lets you navigate (e.g. M-b) between "sub words" of a camelcased word
;;----------------------------------------------;;

(defcustom sboo-scheme-build-file-regexp

  (rx bos
      " at " (group-n 1 (1+ (any "./~" "a-f" "A-F" "0-9")) ".scheme")
      (? ":" (group-n 2 (1+ digit))
         (? ":" (group-n 3 (1+ digit))))
      eos)

  "Regular expression for « scheme-build ».

Matches « error: ... at FILE:COLUMN:LINE » from « scheme-build »'s stdout."

  :type 'regexp

  :safe t
  :group 'sboo)

;;(rx "at" FILE ":" COLUMN ":" LINE)

;; M-: (string-match-p sboo-scheme-build-file-regexp "error: undefined variable 'fetchurl' at /home/sboo/configuration/scheme/bin/default.scheme:73:21")

;; M-: (string-match-p (rx bos (zero-or-more anything) " at " (group-n 1 (one-or-more (any "./~" "a-f" "A-F" "0-9"))) ".scheme" ":"  (group-n 2 (one-or-more digit)) ":"  (group-n 3 (one-or-more digit)) (zero-or-more anything) eos) "error: undefined variable 'fetchurl' at /home/sboo/configuration/scheme/bin/default.scheme:73:21")

;; e.g. match « error: ... at FILE:COLUMN:LINE » from « scheme-build »'s stdout:
;;
;;   $ scheme-build --show-trace ./default.scheme
;;   error: undefined variable 'SEC' at ./default.scheme:73:21
;;
;;(rx bol "error:" (not newline) "at" FILE ":" COLUMN ":" LINE)
;;(rx (not newline) "at" FILE ":" COLUMN ":" LINE)

;; e.g.:
;;
;;   (let* ((G-FILENAME 1)
;;          (G-LINE     2)
;;          (G-COLUMN   3)
;;          (REGEXP     (rx (group-n 1 (1+ (or "/~." letter digit)) ".scheme")
;;                          (? ":" (group-n 2 (1+ digit))
;;                             (? ":" (group-n 3 (1+ digit)))))))
;;
;;     (let ((STRING "~/default.scheme:999:78"))
;;
;;       (string-match REGEXP STRING)
;;
;;       `(:file   ,(expand-file-name (match-string G-FILENAME STRING))
;;         :line   ,(string-to-number (match-string G-LINE     STRING))
;;         :column ,(string-to-number (match-string G-COLUMN   STRING)))))
;;
;; ;; ⇒ '(:file "/home/sboo/default.scheme" :line 999 :column 78)
;;

;;==============================================;;

(defvar sboo-scheme-compile-command-default

  `("scheme-build" "--show-trace" (buffer-file-name))

  "Default `sboo-scheme-compile-command'.")

;;"scheme-build --show-trace *.scheme"
;;"scheme-build" "--show-trace" (buffer-file-name))

;; e.g.: guide, racket, ...

;; (defun scheme-mode-quack-hook ()
;;   (require 'quack)
;;   (setq quack-fontify-style 'emacs))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-scheme-set-compile-command ()

  "Set `compile-command' to `sboo-scheme-compile-command'.

Files:

• the file-buffer's « .dir-locals.el » file.
• the dominating « .project » file.
• the dominating « .cabal » file."

  ;TODO get component from .dir-locals.el, like « sboo-cabal-target ».
  ;TODO get project-root from locate-dominating-file cabal.project.
  ;TODO get default-directory from subdirectory of project-root

  (let* ((STRING (format-message "%s"
                                 (sboo-scheme-compile-command)))
         )

    (setq-local compile-command STRING)))

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-scheme-compile-command  ()

  "Accessor for variable `sboo-scheme-compile-command'.

Output:

• a `stringp'."

  (let* ((OBJECT (or sboo-scheme-compile-command
                     sboo-scheme-compile-command-default))
         )

  (pcase OBJECT

    ((pred stringp)   OBJECT)
    ((pred listp)     (string-join (eval (append '(list) OBJECT)) " "))
;;    ((pred list-of-string-p)     (string-join OBJECT " "))
    ((pred symbolp)   (symbol-value OBJECT))
    ((pred functionp) (call-interactively OBJECT))

    (_ (read-string "Compile Command: " "scheme-build --show-trace " nil "scheme-build --show-trace *.scheme")))))

;; e.g. (string-join '("xyz" "123") " ")

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 
;;==============================================;;
(provide 'sboo-scheme)