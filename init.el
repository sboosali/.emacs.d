;;; init.el --- sboo's Emacs Configuration -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities: Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ~EMACS~ (FilePath)
  "Construct a filepath literal, relative to `user-emacs-directory'.

  M-: (~EMACS ./lisp)
  \"/home/sboo/.emacs.d/lisp"
  "

  `(expand-file-name 
    (concat (or user-emacs-directory "~/.emacs.d/")
            (symbol-name (quote ,FilePath)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro load-path! (FilePath)
  "Register a filepath literal onto the `load-path'.
  
  M-: (macroexpand (load-path! ./lisp))
  (add-to-list 'load-path \"/home/sboo/.emacs.d/lisp\")
  "

  `(add-to-list 'load-path
     (expand-file-name 
       (concat (or user-emacs-directory "~/.emacs.d/")
               (symbol-name (quote ,FilePath))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO;(defmacro sboo-bind! (KeySequence Command &optional KeyMap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities: Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LoadPaths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (load-path! ./elisp)
  ;(load-path! ./submodules/use-package)
  ;(load-path! ./submodules/dante)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (load-path! ./sboo)
  (load-path! ./sboo/installation)
  (load-path! ./sboo/initialization)
  (load-path! ./sboo/configuration)
  (load-path! ./sboo/configuration/02-platforms)
  (load-path! ./sboo/configuration/03-window-systems)
  (load-path! ./sboo/configuration/04-utilities)
  (load-path! ./sboo/configuration/05-keybindings)
  (load-path! ./sboo/configuration/06-initialization)
  (load-path! ./sboo/configuration/07-settings)
  (load-path! ./sboo/configuration/10-internal-packages)
  (load-path! ./sboo/configuration/20-my-packages/dictation)
  (load-path! ./sboo/configuration/25-vendored-packages)
  (load-path! ./sboo/configuration/30-external-packages)
  (load-path! ./sboo/configuration/35-external-configurations)
  (load-path! ./sboo/configuration/50-meta-configurations)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)

;; ^ needed by `package-install' (among others).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (setq undo-limit        20000000)
  (setq undo-strong-limit 40000000)
  ())

;; ^ ensure undo limits are as high as possible.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (progn
;;   (setq shell-command-switch "-c"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Settings: Hacks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when t
  (setenv "LD_PRELOAD" ""))                ;TODO mv to `.bash_emacs.sh', once the file works.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'dante-project-root 'safe-local-variable #'stringp)
(put 'dante-target       'safe-local-variable #'stringp)

;; ^ why?
;; with « $ emacs --desktop », to prevent requiring user input
;; (i.e. "Please enter y, n, or !: ") on **every** startup
;; (whose `desktop' has a haskell file under `dante-mode').

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Variables ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-environment-variable-profile

  "EMACS_PROFILE"

  "See `sboo-profile'.

  Example Usage: « $ EMACS_PROFILE=minimal emacs ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-environment-variable-install

  "EMACS_INSTALL"

  "See `sboo-install?'.

  Example Usage: « $ EMACS_INSTALL=t emacs ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-environment-variable-directory

  "EMACS_SBOO_DIR"

  "See `sboo-directory'. 
  
  Example Usage: « $ EMACS_SBOO_DIR=~/configuration/submodules/.emacs.d/sboo emacs ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-profile

  (let ((*value* (getenv sboo-environment-variable-profile)))
    (pcase *value*
      
      ("0"        'sboo-only-builtins)
      ("builtins" 'sboo-only-builtins)
      
      ("1"        'sboo-core)
      ("core"     'sboo-core)

      ("2"        'sboo-default)
      ("default"  'sboo-default)
      ('()        'sboo-default)    
      (""         'sboo-default)))

  "Which emacs profile has been loaded (or will be). 
  
  The environment variable can reference a profile:
  * by name (a string); or
  * by level (a number).
  
  e.g. `\"0\"' is the most robust, `\"2\"' is the most featured.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-install?

  (let ((*value* (getenv sboo-environment-variable-install)))
    (pcase *value*

      ('()     nil)

      (""      nil)
      ("0"     nil)
      ("no"    nil)
      ("false" nil)

      ("1"     t)
      ("yes"   t)
      ("true"  t)

      (_       t)))

  "Whether to install packages (e.g. when `emacs' is first launched on a new computer).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-directory

  (or (getenv sboo-environment-variable-directory)
      (user-emacs-directory "sboo")

  "The location of my emacs dotfiles.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration: Internal Packages (a.k.a Builtins)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-keybindings) ;TODO replace require with load?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-settings-safe) ;TODO mv these to before use-package, and remove all external dependencies

;; ^
;; `sboo-settings-safe` should always succeed,
;; even if the `load-path` is corrupt, since:
;; [1] only packages built into Emacs25+ are imported; and
;; [2] only simple configurations are performed, e.g. `(setq ...)`.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effects: Initialization ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (find-file user-init-file)

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO
;(when (require 'sboo-server nil t)
;  (server-start-unless-running))
  ;; ^ 
  ;; for `emacsclient'.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation: External Packages ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-required-packages

  '(

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Meta-Configuration:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;use-package-el-get  ; `use-package` dependency
    ;;;bind-key            ; `use-package` dependency
    use-package

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Text/Buffer/Window Stuff:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    real-auto-save
    yasnippet

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Helm:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;async      ; `helm` dependency
    ;;;popup      ; `helm` dependency
    ;;;helm-core  ; `helm` dependency
    helm

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Development:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    projectile
    flycheck
    magit            ; git <C-x g>

    haskell-mode     ; haskell
    dante            ; haskell
    flycheck-haskell ; haskell

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Utilities:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    dash           ; (the `-` prefix)
    s              ; (`s`trings)
    f              ; (`f`iles)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   )

  "Packages which I need to be installed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (require 'package)
  ;; ^ 
  ;; (>= emacs-major-version 24)

  (set         'package-archives ())
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
  ;; ^ 
  ;; remove GNU ELPA, add MELPA Stable. 
  ;; use HTTPS.

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when t ;sboo-install?

  (dolist (*p* sboo-required-packages)
    (package-install *p*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration: External Packages ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effects: Finalization ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;