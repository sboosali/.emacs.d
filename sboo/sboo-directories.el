;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants (and wrappers) for particular directories.
;;
;; Subdirectories include:
;;
;; * `emacs-directory'
;; * `sboo-directory'
;; * `database-directory'
;; * `*-package-directory'
;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst emacs-directory 

  (expand-file-name (or user-emacs-directory "~/.emacs.d/"))

  "The configuration directory for the current user and (TODO) the current profile.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emacs-file (FilePath)

  "Return an absolute filepath under `emacs-directory'.
  `FilePath' is a relative filepath."

  (concat emacs-directory FilePath))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SBoo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-directory 

  (concat emacs-directory "sboo")

  "The configuration directory for my default configuration (whose files and symbols are namespaced under `sboo-*').")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-file (FilePath)

  "Return an absolute filepath under `sboo-directory'.
  `FilePath' is a relative filepath."

  (concat sboo-directory FilePath))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst database-directory 

  (concat emacs-directory "database")

  "Where to store files that're automatically written to (e.g. history, customization, desktop settings, etc).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun database-file (FilePath)

  "Return an absolute filepath under `database-directory'.
  `FilePath' is a relative filepath."

  (concat database-directory FilePath))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cloned Packages (Git SubModules)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst cloned-package-directory 

  (concat emacs-directory "submodules")

  "Where vendored (cloned) packages are.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installed Packages (MELPA Stable)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst installed-package-directory

  (concat emacs-directory "elpa")

  "Where installed packages are.")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cloned-package-directory
;; installed-package-directory

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-directories)