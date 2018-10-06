;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages to Load or Install

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-definitions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-package-names ;;TODO;; (helm . "3.0")

  '(
    (helm		. t)              
    (real-auto-save	. t)
    (use-package	. t)

    (yasnippet		. t)
    (projectile		. t)
    (flycheck		. t)

    (haskell-mode	. t)
    (dante		. t)
    (flycheck-haskell	. t)

    (s			. t)
    (f			. t)
    (dash		. t)
   )

  "Packages which I need to be installed.

An `alist', where each item's:

* `car' is the package name (a symbol).
* `cdr' is the package version (a boolean).

`package-load-list' accepts this `alist'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-package-archives

  '(
    ;;;("melpa"        . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
   )

  "Override `package-archives':

* remove GNU ELPA
* add MELPA Stable
* use HTTPS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (setq package-load-list sboo-package-names)

  (package-initialize)

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `package-load-list':
;;
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-packages-by-installing)
