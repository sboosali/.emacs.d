;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pre-Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (setq
   shell-command-switch "-ic"))

;; ^
;;
;; TODO `-i' -> `.bash_emacs.sh'
;;
;; `shell-*':
;;
;; - `shell-file-name'      "/bin/bash"
;; - `shell-command-switch': "-ic"
;;
;; this overrides the default option "-c" (execute the following command), adding the option "-i", which forces the bash shell into interactive mode, which leads to the sourcing of `~/`.
;;
;; replaces `exec-path-from-shell'.
;; it's a more general solution, because it sets all variables and aliases, not just PATH.
;;
;; why `exec-path-from-shell' originally?
;; when launched from the dock, either in Linux (KDE) or Mac,
;; emacs' `$PATH` is wrong (e.g. can't find `cabal` for `dante`, can't find `git` for `magit`).
;; because the dock is under the graphical-enironment, which was run from a login-shell (?),
;; not an interactive-shell, and thus didn't `source` `.bashrc` (only `.profile`).

;; `bash' (command) Syntax
;;
;;   $ bash --help
;;
;;   Usage:  bash [GNU long option] [option] ...
;;           bash [GNU long option] [option] script-file ...
;;   
;;   GNU long options:
;;           --debug
;;           --debugger
;;           --dump-po-strings
;;           --dump-strings
;;           --help
;;           --init-file
;;           --login
;;           --noediting
;;           --noprofile
;;           --norc
;;           --posix
;;           --rcfile
;;           --restricted
;;           --verbose
;;           --version
;;   
;;   Shell options:
;;           -ilrsD or -c command or -O shopt_option         (invocation only)
;;           -abefhkmnptuvxBCHP or -o option
;;   

;; Debugging
;;
;; to debug `shell-command-switch' (for example, when `M-x' `grep' doesn't work),
;; try running bash directly with those options:
;;
;;   $ bash --rcfile /home/sboo/.bash_emacs.sh -i -c ls
;;
;; Or, for a quick fix, just call `sboo-restore-default-shell-command-switch'.
;;

;; e.g. older versions:
;;
;; shell-command-switch "                                   -ic"
;; shell-command-switch "--rcfile /home/sboo/.bash_emacs.sh -ic"

;; See
;;     - https://stackoverflow.com/a/12229404/1190077
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Relocatable `.emacs.d' ;;;;;;;;;;;;;;;;;;;;;;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs uses following environment variables for configuration:
;; 1. EMACS_ROOT - path to .emacs.d directory.
;;
;; 2. EMACS_ENV_DEFS - paths to .bash_env file - shell script that sets
;; up environment variables on the system for current user.t
;;
;; 3. BASHRC_ENV_LOADED - whecher ~/.bash_env was already loaded.

;; (unless (featurep 'start)
;;   (let ((emacs-root (getenv "EMACS_ROOT")))
;;     (if emacs-root
;;         (progn
;;           (cl-assert (file-directory-p emacs-root))
;;           (let ((src-dir (concat emacs-root "/src")))
;;             (cl-assert (file-directory-p src-dir))
;;             (add-to-list 'load-path src-dir)))
;;       (error "EMACS_ROOT not defined")))
;;   (load-library "start"))

;; ^ from `sergv/dotemacs'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants / Variables / Functions ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-emacs-initialized nil
 "Whether Emacs has finished initializing. i.e. It's `t' when `init.el' has reached last line (of this file); it's `nil' (or unbound) otherwise")

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
  "Like `(concat '~/.emacs.d/' PATH)`, but rather than being hard-coded, the emacs base directory is configured by `sboo-emacs-directory'. This safely falls back to the default location if, for whatever reason, this function has been called despite the variable it references not being bound."

  (let ((d (or sboo-emacs-directory user-emacs-directory)))
    (concat d PATH)))

;; ^
;;
;; `user-emacs-directory':
;;
;; Default value is "~/.emacs.d/"
;; Defined in ‘subr.el’.
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'sboo-emacs-path
  'sboo-emacs-file)
;; ^
;; `defalias':
;;     (defalias 'NEW 'OLD)

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

(defun sboo-database-path (PATH)
  "Like `sboo-database-file'.

  e.g. 

    M-: (sboo-database-path \"desktop/\")
    \"~/.emacs.d/db/desktop/\"

  "
  
  (let ((DIRECTORY (if (boundp 'sboo-database-directory)
                       sboo-database-directory
                     user-emacs-directory)))
      (concat DIRECTORY "/" PATH)))
      ;; ^ e.g.
      ;; `(concat  "~/.emacs.d/db/"  "desktop")

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
;;; Load Paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-register-sboo-load-paths! ()      ;;TODO clean up? mv to sboo-path? does default-directory changing fuck things up?
  "Register `sboo`'s sub-directories to the `load-path`.

  e.g.

      ~/.emacs.d/sboo/*.el
      ~/.emacs.d/sboo/initialization/*.el
      ~/.emacs.d/sboo/configuration/*.el
      ...

  "
  (interactive)

  (progn

    (sboo-add-to-load-path "elisp/")
    ;; ^ e.g. "~/.emacs.d/elisp/*.el"

    (sboo-add-to-load-path "sboo/installation/")

    (sboo-add-subdirs-to-load-path "sboo/configuration"
                                   '("02-platforms"
                                     "03-window-systems"
                                     "04-utilities"
                                     "05-keybindings"
                                     "06-initialization"
                                     "07-settings"
                                     "10-internal-packages"
                                     "25-vendored-packages"
                                     "30-external-packages"
                                     "35-external-configurations"
                                     "50-meta-configurations"
                                     "./"))

    (sboo-add-subdirs-to-load-path "vendor"
                                   '("dante"))

    (sboo-add-subdirs-to-load-path "sboo/configuration/20-my-packages"
                                   '(;;"ghcid"
                                     "dictation"))

    load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-register-sboo-load-paths!)
;; ^ register all `sboo-*` `load-path`s before `load`ing any `sboo-*` package.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Installation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-dependencies)

;;TODO expose command-line-arguments or environment-variables;
;; `--install-dependencies' or `$SBOO_EMACS_INSTALL_DEPENDENCIES'.

(when t
  (sboo-configure-emacs-package-repositories!   )
  (sboo-install-emacs-packages!                t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my configs (namespaced under "sboo").

(require 'sboo-settings-safe) ;TODO mv these to before use-package, and remove all external dependencies
;; ^
;; `sboo-settings-safe` should always succeed,
;; even if the `load-path` is corrupt, since:
;; [1] only packages built into Emacs25+ are imported; and
;; [2] only simple configurations are performed, e.g. `(setq ...)`.
;;

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

;; (this is later to be defined after its dependent definitions)

(require 'sboo-utilities)
(global-set-key "\M-w" 'eval-region-or-last-sexp) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finalize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 sboo-emacs-initialized t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;