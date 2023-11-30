;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External (Vendored) Packages to (Build and) Load

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-definitions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo--helm-dependencies
 '((helm-core t) (helm t) (async t) (popup t)))

(defvar sboo--use-package-dependencies 
 '((bind-key t) (use-package t)))

(defvar sboo--real-auto-save-dependencies 
 '((real-auto-save t)))

(defvar sboo--all-dependencies
  (append sboo--helm-dependencies 
          sboo--use-package-dependencies
          sboo--real-auto-save-dependencies))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-package-vendorables-configure ()
  (setq package-load-list sboo--all-dependencies))

;;

(defun sboo-package-vendorables-initialize ()
  (package-initialize)
  ;;(sboo-register-submodule-packages! "helm/")
  (sboo-register-submodule-packages! "real-auto-save/")
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `package-load-list':
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-packages-by-vendoring)