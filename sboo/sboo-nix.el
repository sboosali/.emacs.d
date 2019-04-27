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
;; Constants -----------------------------------;;
;;----------------------------------------------;;

(defconst sboo-nix-compilation-warning-type 0
  "Alias `compilation-error-regexp-alist-alist' « WARNING » level.")

(defconst sboo-nix-compilation-info-type    1
  "Alias `compilation-error-regexp-alist-alist' « INFO » level.")

(defconst sboo-nix-compilation-error-type   2
  "Alias `compilation-error-regexp-alist-alist' « ERROR » level.")

;;----------------------------------------------;;

(defconst sboo-nix-compilation-group-file 10
  "Alias for a `group-n' in `sboo-nix-compilation-rx'.")

(defconst sboo-nix-compilation-group-line 20
  "Alias for a `group-n' in `sboo-nix-compilation-rx'.")

(defconst sboo-nix-compilation-group-column 30
  "Alias for a `group-n' in `sboo-nix-compilation-rx'.")

;; ^ NOTE these numbers are arbitrary naturals.

;;----------------------------------------------;;

(defconst sboo-nix-compilation-hyperlink-only-match nil
  "Whether to the whole line (t), or just the match (nil), becomes a hyperlink.
  
a `booleanp'.")

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
        #'sboo-nix-prettify-symbols
        #'superword-mode
        )

  "Hooks for `nix-mode'.

Type:

• `listp' of nullary functions.

Zero-or-more function-symbols."

  :type '(repeat (function :tag "Callback"))

  :safe t
  :group 'sboo-nix)

;; ^ with `subword-mode', you can navigate (e.g. « M-b » )
;;   between "sub words" of a camelcased word.

;;----------------------------------------------;;

(defcustom sboo-nix-build-compilation-error-rx

  (rx line-start
      "error: "
      (1+ (not (any ?\n)))
      (group-n 1 (1+ (any "./~" "a-f" "A-F" "0-9")) ".nix")
      (? ":" (group-n 2 (1+ digit))
         (? ":" (group-n 3 (1+ digit))))
      line-end)

  "Regular expression for « nix-build » errors.

Matches an « error: ... FILE:COLUMN:LINE ... »
line in « nix-build »'s stdout."

  :type 'regexp

  :safe t
  :group 'sboo)

;; e.g.
;; 
;;   $ nix-build --show-trace ./x11/lib/keycodes.nix
;;
;;   error: attribute 'XF86Tools' at ./x11/lib/keycodes.nix:175:2 already defined at ./x11/lib/keycodes.nix:166:2
;;

;; TODO

;; (let* ((G-FILENAME 1)
;;        (G-LINE     2)
;;        (G-COLUMN   3)
;;        (REGEX      (rx bos
;;                        " at "
;;                        (group-n 1 (1+ (any "./~" "a-f" "A-F" "0-9")) ".nix")
;;                        (? ":" (group-n 2 (1+ digit))
;;                           (? ":" (group-n 3 (1+ digit))))
;;                        eos))
;;        )

;;   (let* ((LINE "error: attribute 'XF86Tools' at ./x11/lib/keycodes.nix:175:2 already defined at ./x11/lib/keycodes.nix:166:2")
;;          )

;;     (string-match REGEX LINE)

;;     `(:file   ,(expand-file-name (match-string G-FILENAME LINE))
;;       :line   ,(string-to-number (match-string G-LINE     LINE))
;;       :column ,(string-to-number (match-string G-COLUMN   LINE)))))

;;----------------------------------------------;;

(defcustom sboo-nix-compilation-location-rx

  (rx bos
      " at " (group-n 1 (1+ (any "./~" "a-f" "A-F" "0-9")) ".nix")
      (? ":" (group-n 2 (1+ digit))
         (? ":" (group-n 3 (1+ digit))))
      eos)

  "Regular expression for « nix-build ».

Matches « ... at FILE:COLUMN:LINE ... »,
anywhere in « nix-build »'s stdout."

  :type 'regexp

  :safe t
  :group 'sboo)

;;(rx "at" FILE ":" COLUMN ":" LINE)

;; M-: (string-match-p sboo-nix-compilation-location-rx "error: undefined variable 'fetchurl' at /home/sboo/configuration/nix/bin/default.nix:73:21")

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

(defun sboo-haskell-prettify-symbols ()

  "Extend `prettify-symbols-alist' with `sboo-haskell-prettify-symbols-alist'."

  (interactive)

  (if prettify-symbols-mode

      (prettify-symbols-mode 0)

    (progn
      (setq-local prettify-symbols-alist sboo-haskell-prettify-symbols-alist)

      (prettify-symbols-mode +1))))

;;----------------------------------------------;;

(defun sboo-nix-set-compile-command ()

  "Set `compile-command' to `sboo-nix-compile-command'."

  (let* ((STRING (format-message "%s"
                                 (sboo-nix-compile-command)))
         )

    (setq-local compile-command STRING)))

;; TODO! default `compile-command's for: home.nix.
;; TODO? default `compile-command's for: default.nix, shell.nix.

;;----------------------------------------------;;

(defun sboo-nix-compilation ()

  "Hook for `compilation-mode' via `nix-mode'."

  (interactive)

  (let* ()

    (setq-local next-error-highlight                 t)
    (setq-local compilation-auto-jump-to-first-error t)

    (next-error-follow-minor-mode +1)))

;; `next-error-follow-minor-mode':
;;
;; • (setq `next-error-follow-minor-mode' t)
;;

;;----------------------------------------------;;

(defun sboo-nix-build-compilation-register ()

  "Register regexps for « nix-build » with `compilation-mode'.

Extends `compilation-error-regexp-alist' with:

• `sboo-nix-build-compilation-error-rx'
• `sboo-nix-build-compilation-warning-rx'"

  (add-to-list 'compilation-error-regexp-alist
               'sboo-nix-build-compilation-error)

  (add-to-list 'compilation-error-regexp-alist-alist
               `(sboo-nix-build-compilation-error
                 ,sboo-nix-build-compilation-error-rx
                 ,sboo-nix-compilation-group-file
                 ,sboo-nix-compilation-group-line
                 ,sboo-nix-compilation-group-column
                 ,sboo-nix-compilation-error-type
                 ,sboo-nix-compilation-hyperlink-only-match))

  ;; (add-to-list 'compilation-warning-regexp-alist
  ;;              'sboo-nix-build-compilation-warning)

  ;; (add-to-list 'compilation-warning-regexp-alist-alist
  ;;              `(sboo-nix-build-compilation-warning
  ;;                ,sboo-nix-build-compilation-warning-rx
  ;;                ,sboo-nix-compilation-group-file
  ;;                ,sboo-nix-compilation-group-line
  ;;                ,sboo-nix-compilation-group-column
  ;;                ,sboo-nix-compilation-warning-type
  ;;                ,sboo-nix-compilation-hyperlink-only-match))

  ())

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

;; e.g.
;; 
;;   $ nix-build --show-trace ./x11/lib/keycodes.nix
;;
;;   error: attribute 'XF86Tools' at ./x11/lib/keycodes.nix:175:2 already defined at ./x11/lib/keycodes.nix:166:2
;;

;;==============================================;;
(provide 'sboo-nix)