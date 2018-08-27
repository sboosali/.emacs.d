;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my `compilation-mode' configuration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO
;; ^ continuously recompile, on each save.

(setq
 ;;TODO cabal new-build doesn't prefix errors with subdir, maybe cd (or `with-default-directory'??) first?       compilation-auto-jump-to-first-error t
 ;; ^ start at the first error (link).
 next-error-highlight                 t
 next-error-follow-minor-mode         t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; 
;; 
;; 

;; See:
;;     - 
;;     - 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-compilation-1st)