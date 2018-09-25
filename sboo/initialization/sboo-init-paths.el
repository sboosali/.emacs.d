;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants / Variables / Functions ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-emacs-initialized nil
 "Whether Emacs has finished initializing. i.e. It's `t' when `init.el' has reached last line (of this file); it's `nil' (or unbound) otherwise")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-init-file

  (or load-file-name
      (buffer-file-name)
      (expand-file-name "~/.emacs.d/init.el"))

  "e.g. « ~/.emacs.d/init.el »")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-emacs-directory

  (or (getenv "SBOO_EMACS_ROOT")
      (file-name-directory sboo-init-file)
      (expand-file-name "~/.emacs.d"))

  "e.g. « ~/.emacs.d »")

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

;;;(defalias 'sboo-emacs-path  'sboo-emacs-file)
;; ^
;; `defalias':
;;     (defalias 'NEW 'OLD)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-database-directory

  (sboo-emacs-file "db/")

  "e.g. « ~/.emacs.d/db/ »")

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
                                     ;;                                      "use-package"))

    (sboo-add-subdirs-to-load-path "sboo/configuration/20-my-packages"
                                   '(;;"ghcid"
                                     "dictation"))

    load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-init-paths)