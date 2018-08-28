;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My custom `interactive' Commands.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-require (FEATURE)
  "
  `require' one my configs (namespaced under `sboo-*').

  For example, this command:

      M-x sboo-require yasnippets

  equals this expression:

      M-: (require 'sboo-yasnippets)

  "
  (interactive "SFeature to require (`sboo-...'): ")
  
  (require FEATURE))       ;;TODO

;; ^ 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; `completing-read':
;; 
;;     (completing-read `PROMPT' `COLLECTION'
;;            &optional `PREDICATE' `REQUIRE-MATCH' `INITIAL-INPUT' `HIST' DEF INHERIT-INPUT-METHOD)
;;
;; Read a string in the minibuffer, with completion.
;;
;; `PROMPT' is a string to prompt with; normally it ends in a colon and a space.
;; e.g.
;;     "Symbol: "
;;
;; `COLLECTION' can be:
;; - a list of strings,
;; - an alist,
;; - an obarray,
;; - a hash table,
;; - a function that performs (HOW?) the completion itself.
;;
;; `PREDICATE'
;;
;; `REQUIRE-MATCH'
;;
;; `INITIAL-INPUT'
;;
;; `HIST' 
;;
;; 
;; 
;; 
;; 
;; 
;; 
;; 


;; Interactive Commands
;; 
;; 

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-commands)