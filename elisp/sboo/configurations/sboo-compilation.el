;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `compilation-mode`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 compilation-always-kill t)
 ;; ^TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(use-package compilation-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-compilation)