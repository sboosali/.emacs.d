


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Relocatable `.emacs.d' 
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
;; `shell-command-switch'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; NOTE
;;
;; `-i' causes this error:
;;
;;     bash: cannot set terminal process group (-1): Inappropriate ioctl for device bash: no job control in this shell
;; 
;; because:
;;
;; > The -i flag requests that Bash run in interactive mode, which requires a terminal. The solution is to leave the shell-command-switch variable at its default value, which is just -c.
;;
;; https://emacs.stackexchange.com/questions/3447/cannot-set-terminal-process-group-error-when-running-bash-script
;;

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