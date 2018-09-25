
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (setq undo-limit        20000000)
  (setq undo-strong-limit 40000000)
  ())

;; ^ ensure undo limits are as high as possible.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (progn
;;   (setq shell-command-switch "-c"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hacks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when t
  (setenv "LD_PRELOAD" ""))                ;TODO mv to `.bash_emacs.sh', once the file works.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'dante-project-root 'safe-local-variable #'stringp)
(put 'dante-target       'safe-local-variable #'stringp)

;; ^ why?
;; with « $ emacs --desktop », to prevent requiring user input
;; (i.e. "Please enter y, n, or !: ") on **every** startup
;; (whose `desktop' has a haskell file under `dante-mode').

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LoadPaths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "elisp"))

(add-to-list 'load-path (expand-file-name "vendor/dante"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Install `sboo' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/sboo"))
  (require 'sboo)
  (sboo-register-sboo-load-paths!)
  ;; ^ register all `sboo-*` `load-path`s before `load`ing any `sboo-*` package.
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Installation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'sboo-dependencies)
;; 
;; ;;TODO expose command-line-arguments or environment-variables;
;; ;; `--install-dependencies' or `$SBOO_EMACS_INSTALL_DEPENDENCIES'.
;; 
;; (when ()                                ;TODO; (getenv "SBOO_EMACS_INSTALL")
;;   (sboo-configure-emacs-package-repositories!)
;;   (sboo-install-emacs-packages! t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-settings-safe) ;TODO mv these to before use-package, and remove all external dependencies
;; ^
;; `sboo-settings-safe` should always succeed,
;; even if the `load-path` is corrupt, since:
;; [1] only packages built into Emacs25+ are imported; and
;; [2] only simple configurations are performed, e.g. `(setq ...)`.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my configs (namespaced under "sboo").

(require 'sboo-initialization)    ;TODO mv these to before use-package, and remove all external dependencies
;; ^
;; Initially, do simple configurations (like keybindings, custom variables, etc),
;; which (should) always succeed.

(require 'sboo-internal) ;TODO mv these to before use-package, and remove all external dependencies
;; ^ builtin-packages i.e. "1st party".
;;
;; Properly configure any builtin-packages,
;; before configuring installed(i.e. third-party) packages.
;;

(require 'sboo-packages)
;; ^ my packages i.e. "2nd party".
;;
;; 

(require 'sboo-settings)
;; ^ Further settings.

(require 'sboo)            ;TODO rename
;; ^ other packages i.e. "3rd party".
;;
;; 

;;TODO (require 'haskell--projectile-compile--direnv-nixshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-server nil t)
  (server-start-unless-running))
  ;; ^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-init-effects)
;; ^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO mv these two to own file.

(when (require 'sboo-desktop nil t)  ;;TODO rm duplication with import below?
      ;; ^ the `sboo-desktop` module has *only* definitions, no actions.
  (require 'sboo-quitting))
  ;; ^ requires `sboo-desktop`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (require 'sboo-desktop)
  (sboo-config-desktop))
  ;;^ 
  ;; Don't restore any buffers until all modes have been initialized/configured.
  ;; Otherwise, file-extensions won't have been registered with the correct modes,
  ;; custom typefaces won't have been associated, etc.
  ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-external)
;; ^
;; Finally, configure any installed (i.e. third-party) packages.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More Shortcuts (TODO rm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-utilities)
(global-set-key "\M-w" 'eval-region-or-last-sexp) 

;; (this is later to be defined after its dependent definitions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finalize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 sboo-emacs-initialized t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;