;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants / Variables / Functions ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-emacs-initialized nil
 "Whether Emacs has finished initializing. i.e. It's `t` when `init.el` has reached last line (of this file); it's `nil` or unbound otherwise")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-init-file
  (or load-file-name (buffer-file-name))
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-emacs-directory
  (file-name-directory sboo-init-file)
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-emacs-file (PATH)
  "Like `(concat '~/.emacs.d/' PATH)`, but rather than being hard-coded, the emacs base directory is configured by `sboo-emacs-directory`. This safely falls back to the default location if, for whatever reason, this function has been called despite the variable it references not being bound."

  (let ((d (or sboo-emacs-directory user-emacs-directory)))
    (concat d PATH)))

;; ^
;;
;; `user-emacs-directory`:
;;
;; Default value is "~/.emacs.d/"
;; Defined in ‘subr.el’.
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-database-directory
  (sboo-emacs-file "db/")
  "")
  ;;^ e.g. "~/.emacs.d/db/"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun sboo-add-to-load-path (FILEPATH)
  "`add-to-list` FILEPATH to the `load-path`."
  (add-to-list 'load-path
               (sboo-emacs-file FILEPATH)))
               ;; ^ i.e. the filepath `EMACSD/FILEPATH`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-add-subdirs-to-load-path (FILEPATH SUBDIRS)
  "Register each subdirectory (of FILEPATH) in SUBDIRS, relative to `sboo-emacs-directory`, to the `load-path`.

  Trailing slashes (i.e. `\"sboo/\"` versus `\"sboo\"`) should be accepted.

  Relative-paths in SUBDIRS (e.g. `'(\"./\")`) should work.

  e.g.
      (sboo-add-subdirs-to-load-path \"sboo\" '(\"initialization\" \"configuration\"))
  "

  (mapc (lambda (d)
          (add-to-list 'load-path
                       (sboo-emacs-file (concat FILEPATH "/" d))))
                       ;; ^ i.e. the filepath `EMACSD/FILEPATH/SUBDIR`

        (reverse SUBDIRS)))

  ;; ^ 
  ;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load Paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (sboo-add-to-load-path "elisp/"))
  ;; ^ e.g. "~/.emacs.d/elisp/*.el"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-register-sboo-load-paths! ()
  "Register `sboo`'s sub-directories to the `load-path`.

  e.g.

      ~/.emacs.d/sboo/*.el
      ~/.emacs.d/sboo/initialization/*.el
      ~/.emacs.d/sboo/configuration/*.el
      ...

  "
  (interactive)

  (sboo-add-subdirs-to-load-path "elisp/sboo"
                                   '("./"
                                     "utilities/"
                                     "initialization/"
                                     "keybindings/"
                                     "configurations/"
                                     "configurations/external-packages/"
                                     "configurations/internal-packages/"
                                     "settings/"
                                     "platforms/"
                                     "window-systems/"
                                     "packages/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; my configs (namespaced under "sboo").

(require 'use-package)

(sboo-register-sboo-load-paths!)
;; ^ register all `sboo-*` `load-path`s before `load`ing any `sboo-*` package.

(require 'sboo-settings-safe)
;; ^
;; `sboo-settings-safe` should always succeed,
;; even if the `load-path` is corrupt, since:
;; [1] only packages built into Emacs25+ are imported; and
;; [2] only simple configurations are performed, e.g. `(setq ...)`.
;;

(require 'sboo-initialization)
;; ^
;; First configure builtin-packages,
;; then if able, configure installed(/ third-party) packages.

(require 'sboo-settings)
;; ^ Further settings.

(require 'sboo)
;; ^

;;TODO (require 'haskell--projectile-compile--direnv-nixshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'sboo-server nil t)
  (server-start-unless-running))
  ;; ^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-init-effects)
;; ^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Todo mv these two to own file.

(when (require 'sboo-desktop nil t)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MORE SHORTCUTS
;; (this is later to be defined after its dependent definitions)

(require 'sboo-utilities)
(global-set-key "\M-w" 'eval-region-or-last-sexp) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 sboo-emacs-initialized t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;