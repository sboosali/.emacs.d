;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SyntaxTables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs

  :config  
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?. "w")
  ;; ^ make these Characters into Words.
  ;; (i.e. give these character the symbol SyntaxClass).
  
  ;; (modify-syntax-entry ?_ "_")
  ;; (modify-syntax-entry ?. "_")
  ;; ;; ^ make these Characters into Symbols.
  ;; ;; (i.e. give these character the symbol SyntaxClass).

  ) 

;; ^ 
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; SyntaxTables
;; 
;; 

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-syntax-table)