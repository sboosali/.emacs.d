;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS/VARIABLES

;;TODO defvar
(setq user-init-file       (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;;SIMPLE CUSTOMIZATION

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD PATHS

(add-to-list 'load-path
  user-emacs-directory);;TODO remove, emacs warned me against this.
  ;; ^ e.g.
  ;; "~/.emacs.d/*.el"

(require 'init-load-paths);;TODO no, this should be in init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; my configs (namespaced under "sboo").

(require 'sboo-settings)

(require 'sboo-init-with-builtin-packages-only)
(require 'sboo-init-with-installed-packages-too)

(require 'sboo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-server nil t)
  (server-start-unless-running))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-init-effects)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; `use(d)-package(s)`

(require 'use-package)

(use-package term
  :bind
  (("C-c t" . term)
   :map term-mode-map
   ("<kp-prior>" . term-send-up)
   ("<kp-next>"  . term-send-down)
   ("<tab>"      . self-insert-command)));;TODO
   ;; ^ "Keys defined by global-set-key are shadowed by any local binding."

;; see:
;;      https://github.com/jwiegley/use-package/

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://emacs.stackexchange.com/questions/31224/how-to-test-programmatically-whether-the-current-emacs-session-among-several
;; 
;; "server-running-p predicate will evaluate to t if the Emacs server is running, irrespective of which Emacs session currently "owns" the server process."
;;

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