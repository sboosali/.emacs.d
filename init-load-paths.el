;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'init-variables)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'load-path
  (concat user-emacs-directory "elisp/"))
 ;; ^ e.g. "~/.emacs.d/elisp/*.el"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "SBOO", my configurations and utilities

;;TODO
;(require 'sboo-install)

(progn

 (add-to-list 'load-path
  (concat user-emacs-directory "elisp/sboo/"))
 
 (add-to-list 'load-path
  (concat user-emacs-directory "elisp/sboo/utilities/"))
 
 (add-to-list 'load-path
  (concat user-emacs-directory "elisp/sboo/initialization/"))
 
 (add-to-list 'load-path
  (concat user-emacs-directory "elisp/sboo/configurations/"))

 (add-to-list 'load-path
  (concat user-emacs-directory "elisp/sboo/packages/"))
 
)

;; (add-to-list 'load-path
;;  (concat user-emacs-directory "elisp/sboo/applications/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-load-paths)