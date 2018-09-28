;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditionality.
;;
;; - GUI vs CLI
;; - Environment Variables (`$EMACS_*'
;; -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Variables ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-environment-variable-profile "EMACS_PROFILE"

  "See `sboo-profile'.

  Example Usage: « $ EMACS_PROFILE=minimal emacs ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-environment-variable-install "EMACS_INSTALL"

  "See `sboo-install-p'.

  Example Usage: « $ EMACS_INSTALL=t emacs ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-environment-variable-directory "EMACS_SBOO_DIR"

  "See `sboo-directory'.

  Example Usage: « $ EMACS_SBOO_DIR=~/configuration/submodules/.emacs.d/sboo emacs --debug-init ».")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-directory

  (or (getenv sboo-environment-variable-directory)
      (expand-file-name
        (concat (or user-emacs-directory
                    "~/.emacs.d/")
                "sboo")))

  "The location of my emacs dotfiles.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-conditions)