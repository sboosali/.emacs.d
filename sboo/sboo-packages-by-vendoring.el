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
 '((bind-key t) (use-package t) (async t) (popup t)))

(defvar sboo--real-auto-save-dependencies 
 '((real-auto-save t)))

(defvar sboo--all-dependencies
  (append sboo--helm-dependencies 
          sboo--use-package-dependencies
          sboo--real-auto-save-dependencies))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (setq package-load-list sboo--all-dependencies)

  (package-initialize)
  
  (sboo-register-submodule! "helm/")
  (sboo-register-submodule! "use-package/")
  (sboo-register-submodule! "real-auto-save/")

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `package-load-list':
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-packages-by-vendoring)