;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Packages to Load or Install

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-definitions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-package-list

  '(
    (use-package t)
    (real-auto-save t)

    (async   t)
    (compat  t)
    (popup   t)
    (seq t)
    (wfnames t)
    (s       t)
    (f       t)
    (dash    t)

    (helm      t)
    (helm-core t)
    
    (selected      t)              
    (wrap-region   t)
    (expand-region t)

    (company    t)
    (flycheck   t)
    (yasnippet  t)
    (projectile t)
    (magit      t)
    (magit-section t)

    (haskell-mode     t)
    ;; (dante            t)
    ;; (flycheck-haskell t)

    (nix-mode         t)

   )

  "Packages which I need to be installed.

An `alist', where each item's:

* `car' is the package name (a symbol).
* `cdr' is the package version (a boolean).

`package-load-list' accepts this `alist'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-package-archives

  '(
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("melpa"        . "https://melpa.org/packages/")
    ("elpa"         . "https://elpa.gnu.org/packages/")
   )

  "Override `package-archives':

* remove GNU ELPA
* add MELPA Stable
* use HTTPS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-package-installables-configure ()
  (interactive)
  (setq package-archives  sboo-package-archives)
  (setq package-load-list sboo-package-list)
  sboo-package-list)

;;

(defun sboo-package-installables-initialize (&optional refresh-p)
  (interactive "P")
  (package-initialize)

  (if refresh-p
      (package-refresh-contents))

  (package-activate-all)

  (dolist (PKG sboo-package-list)
    (let ((PACKAGE (if (consp PKG)
                       (car PKG)
                     PKG)))
      (unless (package-installed-p 
               (package-install PACKAGE)))))

  sboo-package-list)

(sboo-package-installables-configure)
(sboo-package-installables-initialize)

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
