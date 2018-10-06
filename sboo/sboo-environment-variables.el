;;; Environment Variables for choosing different features, dependencies, and profiles. -*- lexical-binding: t -*-
;;
;; - GUI vs CLI
;; - Environment Variables (under `$EMACS_*' and `$EMACS_SBOO_*')
;; -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo/string->symbol (STRING)
  "`intern` the `STRING', returning its symbol.
  
  Munge the string into an idiomatic elisp symbol:
  
  * clean up, by dropping any non-printable characters;
  * normalize casing, via `downcase';
  * normalize word boundaries, by replacing *all* "unidiomatic" substrings 
  (i.e. one-or-more whitespace and/or non-alphanumeric characters)
  with a single hyphen.

  M-: (sboo/string->symbol "foo-bar")
  'foo-bar

  M-: (sboo/string->symbol "foo, bar!")
  'foo-bar

  M-: (sboo/string->symbol ":Foo:|:Bar:")
  'foo-bar

  "
  (interactive)

  (let ((*symbolic-string* STRING)) ;TODO; implement
  
    (intern *symbolic-string*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Variables ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-envar-profile "EMACS_PROFILE"

  "See `sboo-profile'.

  Example Usage: « $ EMACS_PROFILE=minimal emacs ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-envar-install "EMACS_INSTALL"

  "See `sboo-install-p'.

  Example Usage: « $ EMACS_INSTALL=t emacs ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sboo-envar-directory "EMACS_SBOO_DIR"

  "See `sboo-directory'.

  Example Usage: « $ EMACS_SBOO_DIR=~/configuration/submodules/.emacs.d/sboo emacs --debug-init ».")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-profile

  (let ((*value* (getenv sboo-envar-profile)))
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

  (let ((*value* (getenv sboo-envar-install)))
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

  (or (getenv sboo-envar-directory)
      (expand-file-name
        (concat (or user-emacs-directory
                    "~/.emacs.d/")
                "sboo")))

  "The location of my emacs dotfiles.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; See:
;; - 
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-environment-variables)