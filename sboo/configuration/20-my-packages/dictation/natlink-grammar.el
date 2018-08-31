;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple `major-mode' for editing `Dragon-NaturallySpeaking' (`NatLink') gramamrs.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic-mode 'natlink-grammar-mode

  ()                                           ;; comment char: inapplicable because # must be at start of line
  nil                                          ;; keywords
  '(
    ("^#.*" . 'font-lock-comment-face)         ;; comments at start of line
    ("^<.*?>" . 'font-lock-function-name-face) ;; LHS nonterminals
    ("<.*?>" . 'font-lock-builtin-face)        ;; other nonterminals
    ("::=" . 'font-lock-const-face)            ;; "goes-to" symbol
    ("\|" . 'font-lock-warning-face)           ;; "OR" symbol
    ("\{:\\|:\}" . 'font-lock-keyword-face)    ;; special pybnf delimiters
   )
  '("\\.sapi\\'")               ;; filename suffixes
  nil                                          ;; extra function hooks
  
  "Major mode for BNF highlighting.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic-mode 'bnf-mode
  
  ()                                               ;; comment char:
                                                   ;; inapplicable because # must be at start of line
  nil                                              ;; keywords
  '(
    ("^#.*"       . 'font-lock-comment-face)       ;; comments at start of line
    ("^<.*?>"     . 'font-lock-function-name-face) ;; LHS nonterminals
    ("<.*?>"      . 'font-lock-builtin-face)       ;; other nonterminals
    ("::="        . 'font-lock-const-face)         ;; "goes-to" symbol
    ("\|"         . 'font-lock-warning-face)       ;; "OR" symbol
    ("\{:\\|:\}"  . 'font-lock-keyword-face)       ;; special pybnf delimiters
   )
  '("\\.bnf\\'" "\\.pybnf\\'")                     ;; filename suffixes
  nil                                              ;; extra function hooks
  
  "Major mode for BNF highlighting.")

;; ^ 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; See:
;;     - https://stackoverflow.com/questions/1800199/is-there-a-bnf-mode-for-emacs
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dragon-grammar)