;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants / Variables ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO defvar

(setq sboo-init-file
      (or load-file-name (buffer-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sboo-emacs-directory
      (file-name-directory sboo-init-file))

(defun sboo-emacs-file (FILENAME)
  "Like `(concat '~/.emacs.d/' FILENAME)`, but rather than being hard-coded, the emacs base directory is configured by `sboo-emacs-directory`. This safely falls back to the default location if, for whatever reason, this function has been called despite the variable it references not being bound."

  (let ((d (or sboo-emacs-directory user-emacs-directory)))
    (concat d FILENAME)))

;; ^
;;
;; `user-emacs-directory`:
;;
;; Default value is "~/.emacs.d/"
;; Defined in ‘subr.el’.
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sboo-database-directory
      (sboo-emacs-file "db/"))
 ;;^ e.g. "~/.emacs.d/db/"

(defun sboo-database-file (NAMESPACE FILENAME)
  "For the many files that various emacs packages persist, I: [1] relocate them, to not spam `~/` or `~/.emacs.d/`; and \"namespace\" them per their \"client\" package, to help myself keep track of them. 

  e.g. 

    M-: (sboo-database-file \"desktop\" \".emacs.desktop\")
    \"~/.emacs.d/db/desktop/.emacs.desktop\"

  "
  
  (if (boundp 'sboo-database-directory)
      (concat sboo-database-directory NAMESPACE "/" FILENAME)
      ;; ^ e.g.
      ;; `(concat  "~/.emacs.d/db"  "/"  "desktop"  "/"  ".emacs.desktop")
      (concat user-emacs-directory FILENAME)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load Paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;TODO rm
;;(setq custom-file "~/.emacs.d/custom.el")
;; ^ the `custom-file` is automatically managed (i.e. inserted into) by emacs,
;; via `customize-variable`.
;;(load custom-file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;