
;;==============================================;;
;;; Commentary:

;; 
;; 
;; 
;; 

;;==============================================;;
;;; Code:

;; `natlink-bnf', `natlink-bnf-mode', 

;;==============================================;;

(defvar natlink-bnf-keywords

  '("imported" "exported")

  "Keywords for `natlink-bnf-mode'.")


;;----------------------------------------------;;

(defvar natlink-bnf-file-extensions

  '("\\.sapi\\'" "\\.sapi.py\\'")

  "File Extensions for `natlink-bnf-mode'.
Override `python-mode'  for this compound-file-extension.")

;;----------------------------------------------;;

(defvar natlink-bnf-

  '(
    ("^#.*"      . 'font-lock-comment-face)       ;; comments at start of line
    ("<dgn.*?>"  . 'font-lock-builtin-face)       ;; 
    ("^<.*?>"    . 'font-lock-function-name-face) ;; LHS nonterminals
    ("<.*?>"     . 'font-lock-variable-name-face) ;; other nonterminals
    ("{.*?}"     . 'font-lock-variable-name-face) ;;
    ("="         . 'font-lock-constant-face)      ;; "goes-to" symbol
    (";"         . 'font-lock-constant-face)      ;; statement delimiter
    ("\|"        . 'font-lock-keyword-face)       ;; "OR" symbol
    ("\+"        . 'font-lock-keyword-face)       ;; 
    ("\["        . 'font-lock-keyword-face)       ;; 
    ("\]"        . 'font-lock-keyword-face)       ;; 
    )

  "AssociationList of RegularExpressions for syntax-highlighting `natlink-bnf-mode'.")

;;==============================================;;

(defalias 'natlink-bnf-mode-version #'pkg-info-package-version)

;;----------------------------------------------;;

(defun natlink-bnf-mode-help ()

  "Open a Help Buffer for `natlink-bnf-mode'."

  (interactive)

  )

;;==============================================;;

(defvar natlink-bnf-mode-syntax-table nil

  "Syntax table for `natlink-bnf-mode'. Natlink's BNF has Bash-style comment syntax.")

;;----------------------------------------------;;

(setq natlink-bnf-mode-syntax-table

      (let ((*SyntaxTable* (make-syntax-table)))

        ;; Bash-style comment: “# …”

        (modify-syntax-entry ?#  "<" *SyntaxTable*)
        (modify-syntax-entry ?\n ">" *SyntaxTable*)

        *SyntaxTable*))

;;==============================================;;



;;----------------------------------------------;;



;;==============================================;;

(define-derived-mode natlink-bnf-mode prog-mode "natlink-bnf"

  "`natlink-bnf-mode' is a major mode for editing grammar files 
in an EBNF (extended backus-naur format) which can be parsed by Natlink."

  (progn

    (setq font-lock-defaults (list nil nil))

    (set-syntax-table natlink-bnf-mode-syntax-table)

    ()))

;;==============================================;;

;; (define-generic-mode 'natlink-grammar-simple-mode
;; 
;;   ()                                              ;; comment char: inapplicable because # must be at start of line
;; 
;;   '("imported" "exported")                        ;; keywords
;; 
;;   '(
;;     ("^#.*"      . 'font-lock-comment-face)       ;; comments at start of line
;;     ("<dgn.*?>"  . 'font-lock-builtin-face)       ;; 
;;     ("^<.*?>"    . 'font-lock-function-name-face) ;; LHS nonterminals
;;     ("<.*?>"     . 'font-lock-variable-name-face) ;; other nonterminals
;;     ("{.*?}"     . 'font-lock-variable-name-face) ;;
;;     ("="         . 'font-lock-constant-face)      ;; "goes-to" symbol
;;     (";"         . 'font-lock-constant-face)      ;; statement delimiter
;;     ("\|"        . 'font-lock-keyword-face)       ;; "OR" symbol
;;     ("\+"        . 'font-lock-keyword-face)       ;; 
;;     ("\["        . 'font-lock-keyword-face)       ;; 
;;     ("\]"        . 'font-lock-keyword-face)       ;; 
;;    )
;; 
;;   '("\\.sapi\\'" "\\.sapi.py\\'")                 ;; filename suffixes
;;                                                   ;; override python-mode, only for this compound-file-extension.
;; 
;;   nil                                             ;; extra function hooks
;; 
;;   "Major mode for highlighting a NatLink/SAPI grammar.")
;; 
;;         ;; token can be '=', '|', '+', ';', '(', ')', '[', ']' (with value None)
;;         ;; or 'list' (value without {})
;;         ;; or 'rule' (value wihtout <>)
;;         ;; or 'sqword', 'dqword', 'word'  (a word, in single quotes, double quotes or unquoted)
;; 
