;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq use-package-verbose t)

 ;; ^ 
 ;;
 ;; > When a package is loaded, and if you have use-package-verbose set to t, or if the package takes longer than 0.1s to load, you will see a message to indicate this loading activity in the *Messages* buffer. The same will happen for configuration, or :config blocks that take longer than 0.1s to execute. 
 ;; In general, you should keep :init forms as simple and quick as possible, and put as much as you can get away with into the :config block. This way, deferred loading can help your Emacs to start as quickly as possible.
 ;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(require 'use-package nil t)
;; ^ `NOERROR=t` means "don't error if the `require`ment fails (can't be found, or is buggy, etc).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes

;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ^ `(use-package ... :load-path '("..."))`
;; ;;
;; Extending The Load-Path.
;; If your package needs a directory added to the load-path in order to load, use :load-path. 
;; This takes a symbol, a function, a string or a list of strings. 
;; If the path is relative, it is expanded within user-emacs-directory. 

;; e.g.

;;   (use-package ess-site
;;     :load-path "site-lisp/ess/lisp/")

;; is the same as:

;;   (add-to-list 'load-path (concat user-emacs-directory "site-lisp/ess/lisp/"))
;;   (use-package ess-site)
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-use-package)