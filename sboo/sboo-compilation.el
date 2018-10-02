;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my `compilation-mode' configuration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq compilation-always-kill t)

 ;; ^TODO

;TODO
;; ^ continuously recompile, on each save.

(setq
 next-error-highlight                 t
 next-error-follow-minor-mode         t)

 ;; ^ start at the first error (link).

 ;;TODO cabal new-build doesn't prefix errors with subdir, maybe cd (or `with-default-directory'??) first?      
 ;; (setq compilation-auto-jump-to-first-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. add NodeJS error format
;; 
;; See
;; - https://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/
;;
;; (setq compilation-error-regexp-alist-alist
;;       (cons '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
;;                          1 ;; file
;;                          2 ;; line
;;                          3 ;; column
;;                          )
;;             compilation-error-regexp-alist-alist))
;;
;;
;; ;; ^ [1] define a regex for filename / line number /column number, of errors/warnings.
;;
;; (setq compilation-error-regexp-alist
;;       (cons 'node compilation-error-regexp-alist))
;;
;; ;; ^ [2] register that (named) regex.
;;

;; See:
;;     - 
;;     - 
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-compilation)