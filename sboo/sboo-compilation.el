;;; -*- lexical-binding: t -*-

;;; Commentary:

;; My `compilation-mode' configuration.
;;
;; See:
;;
;; * `'.
;;
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(require 'cl-lib)
(require 'pcase)

(require 'compile)

;;

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(defun sboo-colorize-compilation-buffer ()

  "Invoke `ansi-color-apply-on-region'.

See:

• URL `http://stackoverflow.com/a/13408008/1219634'.
• URL `https://github.com/kaushalmodi/.emacs.d/blob/08f8256f3de346bf6d389f922c52b4605f700fc4/setup-files/setup-compile.el'."

  (unless (or (derived-mode-p 'grep-mode) ;Don't mess up colors in Grep/Ag results buffers
              (derived-mode-p 'ag-mode))
    
    (ansi-color-apply-on-region compilation-filter-start (point))))
  
;;----------------------------------------------;;
;; Functions -----------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
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

;;----------------------------------------------;;
;;; `:config'

(defun sboo-compilation-config! ()

  "Configure `compilation-mode'."

  (interactive)

  (when (require 'ansi-color nil :noerror)

    (add-hook 'compilation-filter-hook #'sboo-colorize-compilation-buffer))

  ;; ^ Compilation.
  ;; 
  ;; `compilation-mode' is a MajorMode.

  ())

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; NOTE The `compilation-mode' configuration also affects `projectile-compile-project'.

;;----------------------------------------------;;

;; 

;;----------------------------------------------;;

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

;;----------------------------------------------;;

;; See:
;;     - 
;;     - 
;;

;;----------------------------------------------;;
(provide 'sboo-compilation)