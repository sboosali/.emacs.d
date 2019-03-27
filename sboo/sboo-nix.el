;;; -*- lexical-binding: t -*-

;;==============================================;;
;;; Commentary:

;; Configuration for the Haskell programming language.
;;
;; See:
;;
;; * `sboo-nix-compile-command'
;; * `sboo-nix-'
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

(defgroup sboo-nix

  nil

  "Personal Nix customization."

  :prefix "sboo-nix-"

  :group 'sboo
  :group 'nix)

;;==============================================;;

(defcustom sboo-nix-compile-command

  nil

  "Command to build « .nix » files.

Type:

• a `stringp'.
• a `listp's of `stringp's.
• a nullary `functionp' that evaluates to a `stringp'.
• a form that evaluates to a `stringp'.

Usage:

• Set as a file-local variable (i.e. in « .dir-locals.el »).
• Set as a custom variable (i.e. `custom-set-variables' »)."

  :type '(choice (const nil       :tag "Default (`sboo-nix-compile-command-default')")
                 (string          :tag "Command-Line")
                 (repeated string :tag "Command and Arguments (arguments will be shell-escaped)")
                 (variable        :tag "Variable (quoted, a `stringp')")
                 (function        :tag "Function (no non-optional input, `stringp' output)")
;                (restricted-sexp :match-alternatives :tag "S-Expression (quoted, starts with « (`eval' ...) »")
          )

  :safe #'stringp
  :group 'sboo-nix)

;;----------------------------------------------;;

(defcustom sboo-nix-hooks-list

  (list #'sboo-nix-set-compile-command
        #'subword-mode
        )

  "Hooks for `nix-mode'.

Type:

• `listp' of nullary functions.

Zero-or-more function-symbols."

  :type '(repeat (function :tag "Callback"))

  :safe t
  :group 'sboo-nix)

;;; sub word mode lets you navigate (e.g. M-b) between "sub words" of a camelcased word
;;----------------------------------------------;;

(defcustom sboo-nix-build-file-regexp

  (rx bos
      " at " (group-n 1 (1+ (any "./~" "a-f" "A-F" "0-9")) ".nix")
      (? ":" (group-n 2 (1+ digit))
         (? ":" (group-n 3 (1+ digit))))
      eos)

  "Regular expression for « nix-build ».

Matches « error: ... at FILE:COLUMN:LINE » from « nix-build »'s stdout."

  :type 'regexp

  :safe t
  :group 'sboo)

;;(rx "at" FILE ":" COLUMN ":" LINE)

;; M-: (string-match-p sboo-nix-build-file-regexp "error: undefined variable 'fetchurl' at /home/sboo/configuration/nix/bin/default.nix:73:21")

;; M-: (string-match-p (rx bos (zero-or-more anything) " at " (group-n 1 (one-or-more (any "./~" "a-f" "A-F" "0-9"))) ".nix" ":"  (group-n 2 (one-or-more digit)) ":"  (group-n 3 (one-or-more digit)) (zero-or-more anything) eos) "error: undefined variable 'fetchurl' at /home/sboo/configuration/nix/bin/default.nix:73:21")

;; e.g. match « error: ... at FILE:COLUMN:LINE » from « nix-build »'s stdout:
;;
;;   $ nix-build --show-trace ./default.nix
;;   error: undefined variable 'SEC' at ./default.nix:73:21
;;
;;(rx bol "error:" (not newline) "at" FILE ":" COLUMN ":" LINE)
;;(rx (not newline) "at" FILE ":" COLUMN ":" LINE)

;; e.g.:
;;
;;   (let* ((G-FILENAME 1)
;;          (G-LINE     2)
;;          (G-COLUMN   3)
;;          (REGEXP     (rx (group-n 1 (1+ (or "/~." letter digit)) ".nix")
;;                          (? ":" (group-n 2 (1+ digit))
;;                             (? ":" (group-n 3 (1+ digit)))))))
;;
;;     (let ((STRING "~/default.nix:999:78"))
;;
;;       (string-match REGEXP STRING)
;;
;;       `(:file   ,(expand-file-name (match-string G-FILENAME STRING))
;;         :line   ,(string-to-number (match-string G-LINE     STRING))
;;         :column ,(string-to-number (match-string G-COLUMN   STRING)))))
;;
;; ;; ⇒ '(:file "/home/sboo/default.nix" :line 999 :column 78)
;;

;;==============================================;;

(defvar sboo-nix-compile-command-default

  `("nix-build" "--show-trace" (buffer-file-name))

  "Default `sboo-nix-compile-command'.")

;;"nix-build --show-trace *.nix"
;;"nix-build" "--show-trace" (buffer-file-name))

;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;


(defun sboo-nix-set-compile-command ()

  "Set `compile-command' to `sboo-nix-compile-command'.

Files:

• the file-buffer's « .dir-locals.el » file.
• the dominating « .project » file.
• the dominating « .cabal » file."

  ;TODO get component from .dir-locals.el, like « sboo-cabal-target ».
  ;TODO get project-root from locate-dominating-file cabal.project.
  ;TODO get default-directory from subdirectory of project-root

  (let* ((STRING (format-message "%s"
                                 (sboo-nix-compile-command)))
         )

    (setq-local compile-command STRING)))

;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-nix-compile-command  ()

  "Accessor for variable `sboo-nix-compile-command'.

Output:

• a `stringp'."

  (let* ((OBJECT (or sboo-nix-compile-command
                     sboo-nix-compile-command-default))
         )

  (pcase OBJECT

    ((pred stringp)   OBJECT)
    ((pred listp)     (string-join (eval (append '(list) OBJECT)) " "))
;;    ((pred list-of-string-p)     (string-join OBJECT " "))
    ((pred symbolp)   (symbol-value OBJECT))
    ((pred functionp) (call-interactively OBJECT))

    (_ (read-string "Compile Command: " "nix-build --show-trace " nil "nix-build --show-trace *.nix")))))

;; e.g. (string-join '("xyz" "123") " ")

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;
;; 
;; 
;; 
;;==============================================;;
(provide 'sboo-nix)