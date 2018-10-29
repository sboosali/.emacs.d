;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My `compilation-mode' configuration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `:init'

(defun sboo-compilation-init! ()

  "Initialize `compilation-mode' variables."  
  (interactive)

  ;;TODO ;; ^ continuously recompile, on each save.

  (setq compilation-ask-about-save nil)

  ;; ^ Save buffer(s) (without asking).

  (setq compilation-always-kill t)

  ;; ^ `t' means TODO.

  (setq compilation-scroll-output t)

  ;; ^ 

  (setq next-error-highlight         t)
  (setq next-error-follow-minor-mode t)

  ;; ^ start at the first error (link).

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `:config'

(defun sboo-compilation-config! ()

  "Configure `compilation-mode' and enable it."
  (interactive)

  ;; ^ Compilation.
  ;; 
  ;; `compilation-mode' is a MajorMode.

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE The `compilation-mode' configuration also affects `projectile-compile-project'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.g. add NodeJS error format:
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

;; See:
;;     - 
;;     - 
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-compilation)