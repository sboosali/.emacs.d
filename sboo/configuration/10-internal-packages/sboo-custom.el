;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `Customize`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs

  :init
  (setq
   sboo-custom-file (sboo-database-file "custom" "custom.el")
   custom-file sboo-custom-file)

  :config
  (load sboo-custom-file))

;; ^
;;
;; `custom-file`:
;;
;; The `custom-file` is automatically managed (i.e. inserted into) by emacs,
;; via `customize-variable`.
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-custom)