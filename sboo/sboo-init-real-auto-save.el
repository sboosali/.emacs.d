;;; Configuration for `real-auto-save' -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq real-auto-save-interval 1)

;; ^ "1" means autosave every second".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'real-auto-save)

(autoload #'real-auto-save-mode
          "real-auto-save.el"
          "Autosave this buffer (via `real-auto-save')." 
          'interactive 
          nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'find-file-hook #'real-auto-save-mode)

;; ^ autosave all file-buffers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unloading ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-init-real-auto-save-unload-function (&optional PrefixArgument)
  "Undo `load'ing (/ `require`ing) this `feature'.

Reverts both the `:init' and `:config' settings.

By:

* removing `hook's.
* re`setq'ting variables to their defaults.

Is Idempotent.

For `unload-feature'."

  (interactive "P")

  (when t  ;;;(featurep 'real-auto-save)

    (setq real-auto-save-interval 10)
    ;; ^ the default.

    (remove-hook 'find-file-hook #'real-auto-save-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `real-auto-save-mode':
;;
;; auto-saves a (file-)buffer to the visited file itself (not the `~`-suffixed backup file.")
;;

;; See:
;; - https://github.com/ChillarAnand/real-auto-save

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AutoLoads: `require' vs `autoload'
;;
;; (autoload FUNCTION FILE &optional DOCSTRING INTERACTIVE TYPE)
;;

;; Unloading a Feature: `unload-feature'
;;
;; `(unload-feature '<feature>)`:
;; * calls `<feature>-unload-function`,
;; * then un`load's `<feature>`.
;;
;; Signature: `(unload-feature FEATURE &optional FORCE)`
;;
;; > If loading the file adds functions to hooks, define a function <feature>-unload-function, where <feature> is the name of the feature the package provides, and make it undo any such changes. Using unload-feature to unload the file will run this function.
;;

;; See:
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Unloading.html
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html
;; - http://ergoemacs.org/emacs/elisp_check_defined.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-init-real-auto-save)
