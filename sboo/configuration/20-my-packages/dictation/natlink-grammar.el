;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple `major-mode' for editing `Dragon-NaturallySpeaking' (`NatLink') grammars.
;;
;; The format is (E)BNF, with:
;;
;; - a few extensions (e.g. "{x}" for lists versus "<x>" for rules,
;;; `imported' and `exported' keywords, and so on)
;;
;; - and a few restrictions (e.g. no "+" operator, only "*", i.e. repetition,
;; and "?", i.e. optionality).
;;
;; This format is a human-readable represenation of `Microsoft's `SAPI'
;; (the "Speech API"), which is a binary format.
;;
;; Its canonical parser is `gramparser.py`, at 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic-mode 'natlink-grammar-simple-mode

  ()                                              ;; comment char: inapplicable because # must be at start of line

  '("imported" "exported")                        ;; keywords

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

  '("\\.sapi\\'" "\\.sapi.py\\'")                 ;; filename suffixes
                                                  ;; override python-mode, only for this compound-file-extension.

  nil                                             ;; extra function hooks

  "Major mode for highlighting a NatLink/SAPI grammar.")

        ;; token can be '=', '|', '+', ';', '(', ')', '[', ']' (with value None)
        ;; or 'list' (value without {})
        ;; or 'rule' (value wihtout <>)
        ;; or 'sqword', 'dqword', 'word'  (a word, in single quotes, double quotes or unquoted)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-generic-mode 'bnf-mode
;;   ()                                               ;; comment char:
;;                                                    ;; inapplicable because # must be at start of line
;;   nil                                              ;; keywords
;;   '(
;;     ("^#.*"       . 'font-lock-comment-face)       ;; comments at start of line
;;     ("^<.*?>"     . 'font-lock-function-name-face) ;; LHS nonterminals
;;     ("<.*?>"      . 'font-lock-builtin-face)       ;; other nonterminals
;;     ("::="        . 'font-lock-const-face)         ;; "goes-to" symbol
;;     ("\|"         . 'font-lock-warning-face)       ;; "OR" symbol
;;     ("\{:\\|:\}"  . 'font-lock-keyword-face)       ;; special pybnf delimiters
;;    )
;;   '("\\.bnf\\'" "\\.pybnf\\'")                     ;; filename suffixes
;;   nil                                              ;; extra function hooks
;;   "Major mode for BNF highlighting.")

;; ^ 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; `font-lock-...-face':
;;
;; C-h v font-lock-.+-face'
;;
;;   font-lock-doc-face
;;   font-lock-type-face
;;   font-lock-string-face
;;   font-lock-comment-face
;;   font-lock-warning-face
;;   font-lock-keyword-face
;;   font-lock-builtin-face
;;   font-lock-constant-face
;;   font-lock-reference-face
;;   font-lock-preprocessor-face
;;   font-lock-negation-char-face
;;   font-lock-function-name-face
;;   font-lock-variable-name-face
;;   font-lock-comment-delimiter-face
;;
;; 

;; See:
;;     - https://stackoverflow.com/questions/1800199/is-there-a-bnf-mode-for-emacs
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'natlink-grammar)