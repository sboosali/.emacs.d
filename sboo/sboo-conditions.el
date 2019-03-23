;;; -*- lexical-binding: t -*-

;;; Commentary:

;;----------------------------------------------;;
;; `sboo-conditions' normalizes the current platform and some environment variables.
;;
;; See:
;;
;; * GUI vs CLI
;; * Environment Variables (`$EMACS_*'
;; * 
;;
;;----------------------------------------------;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(require 'cl)
(require 'pcase)

;;
 
;;----------------------------------------------;;
;; Environment Variables: Names
;;----------------------------------------------;;

;; (defconst SBOO_EMACS_PROFILE_BUILTINS_ONLY "SBOO_EMACS_PROFILE_BUILTINS_ONLY"

;;   "See `sboo-profile-builtins-only'.

;;   Example Usage: « $ SBOO_EMACS_PROFILE_BUILTINS_ONLY=1 emacs ... ».")

;; ;;----------------------------------------------;;

;; (defconst SBOO_EMACS_PROFILE_SBOO_ONLY "SBOO_EMACS_PROFILE_SBOO_ONLY"

;;   "See `sboo-profile-sboo-only'.

;;   Example Usage: « $ SBOO_EMACS_PROFILE_SBOO_ONLY=1 emacs ... ».")

;; ;;----------------------------------------------;;

;; (defconst SBOO_EMACS_PROFILE_ "SBOO_EMACS_PROFILE_"

;;   "See `sboo-profile-'.

;;   Example Usage: « $ SBOO_EMACS_PROFILE_=1 emacs ... ».")

;; ;;----------------------------------------------;;

;; (defconst SBOO_EMACS_PROFILE_ "SBOO_EMACS_PROFILE_"

;;   "See `sboo-profile-'.

;;   Example Usage: « $ SBOO_EMACS_PROFILE_=1 emacs ... ».")

;; ;;----------------------------------------------;;

;; (defconst SBOO_EMACS_PROFILE_ "SBOO_EMACS_PROFILE_"

;;   "See `sboo-profile-'.

;;   Example Usage: « $ SBOO_EMACS_PROFILE_=1 emacs ... ».")

;; ;;----------------------------------------------;;

;; (defconst SBOO_EMACS_PROFILE_ "SBOO_EMACS_PROFILE_"

;;   "See `sboo-profile-'.

;;   Example Usage: « $ SBOO_EMACS_PROFILE_=1 emacs ... ».")

;; ;;----------------------------------------------;;
;; ;; Environment Variables: Values
;; ;;----------------------------------------------;;

;; (defvar sboo-emacs-profile-builtins-only

;;   (getenv SBOO_EMACS_PROFILE_BUILTINS_ONLY)

;;   "Whether to configure Emacs with only emacs-builtins: No custom scripts; No package downloads."

;;----------------------------------------------;;
;; Environment Variables
;;----------------------------------------------;;

(defconst sboo-environment-variable--profile-builtins "SBOO_EMACS_PROFILE_BUILTINS"

  "See `sboo-profile-builtins'.

  Example Usage: « $ SBOO_EMACS_PROFILE_BUILTINS=1 emacs ... ».")

;;----------------------------------------------;;

(defconst sboo-environment-variable--profile-minimal "SBOO_EMACS_PROFILE_MINIMAL"

  "See `sboo-profile-minimal'.

  Example Usage: « $ SBOO_EMACS_PROFILE_MINIMAL=1 emacs ... ».")

;;----------------------------------------------;;

(defconst sboo-environment-variable--profile-small "SBOO_EMACS_PROFILE_SMALL"

  "See `sboo-profile-small'.

  Example Usage: « $ SBOO_EMACS_PROFILE_SMALL=1 emacs ... ».")

;;----------------------------------------------;;

(defconst sboo-environment-variable--profile-large "SBOO_EMACS_PROFILE_LARGE"

  "See `sboo-profile-large'.

  Example Usage: « $ SBOO_EMACS_PROFILE_LARGE=1 emacs ... ».")

;;----------------------------------------------;;

(defconst sboo-environment-variable--profile-full "SBOO_EMACS_PROFILE_FULL"

  "See `sboo-profile-full'.

  Example Usage: « $ SBOO_EMACS_PROFILE_FULL=1 emacs ... ».")

;;----------------------------------------------;;

(defconst sboo-environment-variable--profile-builtins "SBOO_EMACS_PROFILE_BUILTINS"

  "See `sboo-profile-builtins'.

  Example Usage: « $ SBOO_EMACS_PROFILE_=1 BUILTINSEMACS ... ».")

;;----------------------------------------------;;

(defconst sboo-environment-variable--profile-personal "SBOO_EMACS_PROFILE_PERSONAL"

  "See `sboo-profile-personal'.

  Example Usage: « $ SBOO_EMACS_PROFILE_=1 PERSONALEMACS ... ».")

;;----------------------------------------------;;

(defconst sboo-environment-variable--profile-external "SBOO_EMACS_PROFILE_EXTERNAL"

  "See `sboo-profile-external'.

  Example Usage: « $ SBOO_EMACS_PROFILE_=1 EXTERNALEMACS ... ».")

;;----------------------------------------------;;


(defconst sboo-environment-variable--profile- "SBOO_EMACS_PROFILE_"

  "See `sboo-profile-'.

  Example Usage: « $ SBOO_EMACS_PROFILE_=1 emacs ... ».")

;;----------------------------------------------;;


(defconst sboo-environment-variable--profile- "SBOO_EMACS_PROFILE_"

  "See `sboo-profile-'.

  Example Usage: « $ SBOO_EMACS_PROFILE_=1 emacs ... ».")

;;----------------------------------------------;;


(defconst sboo-environment-variable--profile- "SBOO_EMACS_PROFILE_"

  "See `sboo-profile-'.

  Example Usage: « $ SBOO_EMACS_PROFILE_=1 emacs ... ».")

;;----------------------------------------------;;


(defconst sboo-environment-variable--profile- "SBOO_EMACS_PROFILE_"

  "See `sboo-profile-'.

  Example Usage: « $ SBOO_EMACS_PROFILE_=1 emacs ... ».")

;;----------------------------------------------;;

(defconst sboo-environment-variable-profile "SBOO_EMACS_PROFILE"

  "See `sboo-profile'.

  Example Usage: « $ SBOO_EMACS_PROFILE=minimal emacs ».")

;;----------------------------------------------;;

;;----------------------------------------------;;
;; Environment Variables
;;----------------------------------------------;;

(defconst sboo-environment-variable-install "SBOO_EMACS_INSTALL"

  "See `sboo-install-p'.

  Example Usage: « $ SBOO_EMACS_INSTALL=t emacs ».")

;;----------------------------------------------;;

(defconst sboo-environment-variable-directory "SBOO_EMACS_SBOO_DIR"

  "See `sboo-directory'.

  Example Usage: « $ SBOO_EMACS_SBOO_DIR=~/configuration/submodules/.emacs.d/sboo emacs --debug-init ».")

;;----------------------------------------------;;

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

;;----------------------------------------------;;

(defvar sboo-install-p

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

;;----------------------------------------------;;

(defvar sboo-directory

  (or (getenv sboo-environment-variable-directory)
      (expand-file-name
        (concat (or user-emacs-directory
                    "~/.emacs.d/")
                "sboo")))

  "The location of my emacs dotfiles.")

;;----------------------------------------------;;
;; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;----------------------------------------------;;

(defconst platform

  (cond

   ((or (memq system-type   '(gnu/linux)))

    'platform-linux)

   ((or (memq system-type   '(cygwin windows-nt ms-dos))
        (memq window-system '(w32 pc)))

    'platform-windnows)

   ((or (memq system-type   '(darwin))
        (memq window-system '(mac ns)))

    'platform-apple)

   ((or (eq window-system 'x))  ;; TODO and Wayland?
    ;; ^ X11 runs on non-Linux operating-systems too (like Apple).

    'platform-linux)

   (t
    'platform-unknown))

  "The current platform, a symbol (prefixed with `platform-').

One of: 

* 'platform-linux
* 'platform-windows
* 'platform-apple
* 'platform-unknown

Depends on the variables `system-type' and `window-system'.")

;; TODO? `case'
;; (case window-system
;;  ('mac (require 'sboo-macintosh)))

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; `window-system':
;;
;; is the window system. it's one of:
;;
;; * `graphic` — any graphics-capable display.
;; * `x` — X11.
;; * `pc` — for the MS-DOS console.
;; * `w32` — for MS Windows 9X/NT/2K/XP.
;; * `tty` — a non-graphics-capable display.

;; size words (alternate naming for the profiling environment variables):
;;
;; * 
;; * small, large, 
;; * minor, major
;; * first, second, third, fourth, ...
;; * smallest, smaller, bigger, biggest
;; * part, full, 
;; * 

;; descriptive words (alternate naming for the profiling environment variables):
;;
;; * 
;; * builtins, personal, external
;; * builtins-only, personal-only, external-too
;; * stable, provisional, experimental (like Hackage/Haddocks `portability`)
;; * 1st-party, 2nd-party, 3rd-party, 
;; * 

;;(defconst sboo-environment-variable- "SBOO_EMACS_"
;;
;;  "See `sboo-'.
;;
;;  Example Usage: « $ SBOO_EMACS_=1 emacs ... ».")

;;(defconst sboo-environment-variable--profile- "SBOO_EMACS_PROFILE_"
;;
;;  "See `sboo-profile-'.
;;
;;  Example Usage: « $ SBOO_EMACS_PROFILE_=1 emacs ... ».")

;;(defconst SBOO_ENVIRONMENT_VARIABLE_BUILTINS "SBOO_EMACS_BUILTINS"
;;
;;  "See `sboo-builtins'.
;;
;;  Example Usage: « $ SBOO_EMACS_PROFILE_BUILTINS=1 emacs ... ».")

;;----------------------------------------------;;
(provide 'sboo-conditions)