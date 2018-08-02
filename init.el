;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS/VARIABLES

;;TODO defvar
(setq sboo-init-file       (or load-file-name (buffer-file-name)))
(setq sboo-emacs-directory (file-name-directory sboo-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD PATHS

(progn

  (add-to-list 'load-path
    sboo-emacs-directory);;TODO remove, emacs warned me against this.
    ;; ^ e.g. "~/.emacs.d/*.el"

  (add-to-list 'load-path
    (concat sboo-emacs-directory "elisp/"))
    ;; ^ e.g. "~/.emacs.d/elisp/*.el"
)

(progn

 (add-to-list 'load-path
  (concat sboo-emacs-directory "elisp/sboo/"))
 
 (add-to-list 'load-path
  (concat sboo-emacs-directory "elisp/sboo/utilities/"))
 
 (add-to-list 'load-path
  (concat sboo-emacs-directory "elisp/sboo/initialization/"))
 
 (add-to-list 'load-path
  (concat sboo-emacs-directory "elisp/sboo/configurations/"))

 (add-to-list 'load-path
  (concat sboo-emacs-directory "elisp/sboo/packages/"))

;; (add-to-list 'load-path
;;  (concat sboo-emacs-directory "elisp/sboo/applications/"))
 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; my configs (namespaced under "sboo").

(require 'sboo-settings)

(require 'sboo-init-with-builtin-packages-only)
(require 'sboo-init-with-installed-packages-too)

(require 'sboo)

;;TODO
(require 'haskell--projectile-compile--direnv-nixshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-server nil t)
  (server-start-unless-running))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-init-effects)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; `use(d)-package(s)`

(require 'use-package)

;; see:
;;      https://github.com/jwiegley/use-package/

;; (use-package )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MORE SHORTCUTS (this is later to be defined after its dependent definitions)

;;(require 'sboo-utilities)
(global-set-key "\M-w" 'eval-region-or-last-sexp) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file "~/.emacs.d/custom.el")
;; ^ the `custom-file` is automatically managed (i.e. inserted into) by emacs,
;; via `customize-variable`.
(load custom-file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;