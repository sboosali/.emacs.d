;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My `customize' configuration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-custom-file

  (sboo-database-file "custom" "custom.el")

  "My `custom-file'.")

;; ^
;; NOTE if emacs is launched with « emacs -q »,
;; then « M-x customize » refuses to save it,
;; even if `user-init-file' and `custom-file' are safely distinct.
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-load-custom-file! ()
  "`load' `sboo-custom-file'."
  (interactive)
  
  (progn
    (load sboo-custom-file t)))

;; ^
;;
;; `load':
;;
;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (setq custom-file sboo-custom-file)

  (sboo-load-custom-file!))

;; ^
;;
;; `custom-file':
;;
;; The `custom-file' is automatically managed (i.e. inserted into) by emacs,
;; via `customize-variable'.
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-custom)