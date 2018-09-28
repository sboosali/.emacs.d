;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External Packages to Install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-packages-required

  '(

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Helm:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (helm . "3.0")

    ;;;async      ; `helm` dependency
    ;;;popup      ; `helm` dependency
    ;;;helm-core  ; `helm` dependency

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Text/Buffer/Window Management:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    real-auto-save
    yasnippet

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Development: General
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    projectile
    flycheck
    magit            ; git <C-x g>

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Development: Haskell
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    haskell-mode
    dante
    flycheck-haskell

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Utilities:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    dash           ; (the `-` prefix)
    s              ; (`s`trings)
    f              ; (`f`iles)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   )

  "Packages which I need to be installed.
  
  A list; each item is either:
  
  * a symbol+string pair (e.g. « '(helm . \"3.0\") »); the package's name and its version.
  * a symbol (e.g. « 'dante »); just the package's name.

  (An absent version is unconstrained, and implicitly the latest available one).
  ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-package-archives

  '(("melpa-stable" . "https://stable.melpa.org/packages/")    ;;TODO is https still buggy on windows?
   )

  "Override `package-archives':
  
  * remove GNU ELPA
  * add MELPA Stable
  * use HTTPS
  ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-normalize-package-identifier (PackageIdentifier)

  "Convert:
  
  * from my package identifers (e.g. « 'dante » or « '(helm . \"3.0\") »).
  * into the format of `package-load-list'.

  (`nil' when invalid).
  "

  (pcase PackageIdentifier

      (`(,(pred symbolp) . ,(pred stringp))
        PackageIdentifier)

      ((pred symbolp)
       `(,PackageIdentifier . t))

      (_ nil)))

;; ^ 
;;
;; NOTE « '(<p> . t) » means "the latest version available" for `<p>'.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-normalize-package-list (PackageIdentifiers)

  "map `sboo-normalize-package-identifier', drop `nil's, drop duplicates.
  "

  (seq-uniq
    (seq-filter #'identity
      (seq-map #'sboo-normalize-package-identifier 
        PackageIdentifiers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-packages-print! ()
  ""
  (interactive)

  (message "==================================================\n")

  (dolist (@p sboo-packages-required)
    (message "[package] %S" @p))
    
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-packages-config! ()

  "Configure this feature (i.e. `sboo-packages') before calling its exports.
  "

  (setq package-archives 
        sboo-package-archives)

  (setq sboo-packages-required
        (sboo-normalize-package-list sboo-packages-required))

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-packages-activate! ()

  "`load' and activate all installed packages.
  "

  (interactive)

  (setq package-enable-at-startup nil)

  (let ((package-load-list sboo-packages-required))
    (package-initialize))

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-packages-install-required! ()

  "For each package in `sboo-packages-required':

  * install it (via `package-install'), unless already installed'
  * load it (via `require'), unless already loaded;
  * activate it (TODO wdtm?)
  "

  (interactive)

  (message "==================================================\n")
  (message "[sboo] Installing packages...")

  (dolist (@package-identifier sboo-packages-required)
    (unless (package-installed-p *p*)
      (package-install *p*)))

  (message "==================================================\n")
  (message "[sboo] Activating packages...")

  (sboo-packages-activate!)

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sboo-packages-config!)

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