;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIRectory LOCAL variables.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; TODO

;; (add-to-list 'safe-local-eval-forms
;;              '(locate-dominating-file default-directory "cabal.project"))

;; (add-to-list 'safe-local-variable-values
;;              ('dante-project-root
;;               . '(locate-dominating-file default-directory "cabal.project")))

;;TODO (put 'dante-project-root 'safe-local-variable (== (list 'locate-dominating-file 'default-directory "cabal.project")))
;; (put 'dante-project-root 'safe-local-variable #'stringp) is in dante.el
;;

;; ^ for `.dir-locals.el' & `dante'.
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; Related:
;; `safe-local-variable-values'
;; `safe-local-eval-forms'
;; `safe-local-eval-function'

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-dirlocals)