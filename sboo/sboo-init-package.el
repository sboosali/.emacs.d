
(defvar sboo-package-list

  '(use-package
    helm
    real-auto-save
    yasnippet
    projectile
    haskell-mode
    dante
   )

 "Packages that must be installed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-package-archives 

  '(("melpa-stable" . "https://stable.melpa.org/packages/")
   )

  "Override `package-archives':
  
  * remove GNU ELPA
  * add MELPA Stable
  * use HTTPS
  ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (require 'package)

  (setq package-enable-at-startup nil)

  (setq package-archives sboo-package-archives)

  (package-refresh-contents)

  (package-initialize)

  (dolist (p sboo-package-list)
    (package-install p))

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-init-package)