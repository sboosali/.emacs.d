;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External Packages to Install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-package-names

  '(
    helm           ;;(helm . "3.0")
    real-auto-save
    yasnippet

    projectile
    flycheck
    magit
    haskell-mode
    dante
    flycheck-haskell

    s
    f
    dash
   )

  "Packages which I need to be installed.
  
  A list; each item is either:
  
  * a symbol+string pair (e.g. « '(helm . \"3.0\") »); the package's name and its version.
  * a symbol (e.g. « 'dante »); just the package's name.

  (An absent version is unconstrained, and implicitly the latest available one).
  ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-package-archives

  '(
    ;;;("melpa"        . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
   )

  "Override `package-archives':
  
  * remove GNU ELPA
  * add MELPA Stable
  * use HTTPS
  ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (setq package-enable-at-startup nil)

  ;;;(setq package-archives sboo-package-archives)

  (dolist ($archive sboo-package-archives)
    (add-to-list 'package-archives $archive))

  (package-initialize)

  (package-refresh-contents)

  (dolist ($package sboo-package-names)
    (package-install $package))
  
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `package-initialize'
;;  ==================
;; 
;;     (package-initialize &optional NO-ACTIVATE)
;; 
;; Load Emacs Lisp packages, and activate them.
;;
;; The variable ‘package-load-list’ controls which packages to load.
;;
;; If optional arg NO-ACTIVATE is non-nil, don’t activate packages.
;;
;; If ‘user-init-file’ does not mention ‘(package-initialize)’, add
;; it to the file.
;;
;; If called as part of loading ‘user-init-file’, set
;; ‘package-enable-at-startup’ to nil, to prevent accidentally
;; loading packages twice.
;;
;; It is not necessary to adjust ‘load-path’ or ‘require’ the
;; individual packages after calling ‘package-initialize’ -- this is
;; taken care of by ‘package-initialize’.
;;

;; `package-load-list'
;;  ==================
;; 
;; List of packages for ‘package-initialize’ to load.
;;
;; Each element in this list should be a list (NAME VERSION), or the
;; symbol ‘all’.  The symbol ‘all’ says to load the latest installed
;; versions of all packages not specified by other elements.
;; 
;; For an element (NAME VERSION), NAME is a package name (a symbol).
;;
;; VERSION should be t, a string, or nil.
;; If VERSION is t, the most recent version is activated.
;; If VERSION is a string, only that version is ever loaded.
;;  Any other version, even if newer, is silently ignored.
;;  Hence, the package is "held" at that version.
;; If VERSION is nil, the package is not loaded (it is "disabled").

;; `format-message'
;;  ==============
;;
;; %S’
;; Replace the specification with the printed representation of the object, made with quoting (that is, using prin1—see Output Functions). 
;; Thus, strings are enclosed in ‘"’ characters, and ‘\’ characters appear where necessary before special characters.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-packages)