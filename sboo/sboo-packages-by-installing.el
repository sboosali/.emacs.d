;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages to Load or Install

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-definitions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-package-dependencies ;;TODO;; (helm . "3.0")

  '(
    (use-package t)

    (helm      t)              
    (helm-core t)
    
    (real-auto-save t)

    (yasnippet  t)
    (projectile t)
    (flycheck   t)

    (haskell-mode     t)
    (dante            t)
    (flycheck-haskell t)

    (s    t)
    (f    t)
    (dash t)
   )

  "Packages which I need to be installed.

An `alist', where each item's:

* `car' is the package name (a symbol).
* `cdr' is the package version (a boolean).

`package-load-list' accepts this `alist'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-package-archives

  '(
    ("melpa"        . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
   )

  "Override `package-archives':

* remove GNU ELPA
* add MELPA Stable
* use HTTPS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (setq package-load-list sboo-package-dependencies)

  (package-initialize)

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `package-load-list':
;;
;; List of packages for `package-initialize' to load.
;; Each element in this list should be a list (NAME VERSION), or the
;; symbol `all'.  The symbol `all' says to load the latest installed
;; versions of all packages not specified by other elements.
;; For an element (NAME VERSION), NAME is a package name (a symbol).
;; VERSION should be t, a string, or nil.
;; If VERSION is t, the most recent version is activated.
;; If VERSION is a string, only that version is ever loaded.
;;  Any other version, even if newer, is silently ignored.
;;  Hence, the package is "held" at that version.
;; If VERSION is nil, the package is not loaded (it is "disabled").
;;
;; See:
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html
;; - https://emacs.stackexchange.com/questions/17542/how-to-load-only-a-subset-of-installed-packages
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-packages-by-installing)
