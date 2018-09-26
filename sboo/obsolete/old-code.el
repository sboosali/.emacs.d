












      (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/")  t)
      ;;;(add-to-list 'package-archives '("melpa"        . "http://melpa.milkbox.net/packages/") t)




#TODO# melpaPackages ++ stablePackages ++ optionalPackages







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'sboo-dependencies)
;; 
;; ;;TODO expose command-line-arguments or environment-variables;
;; ;; `--install-dependencies' or `$SBOO_EMACS_INSTALL_DEPENDENCIES'.
;; 
;; (when ()                                ;TODO; (getenv "SBOO_EMACS_INSTALL")
;;   (sboo-configure-emacs-package-repositories!)
;;   (sboo-install-emacs-packages! t))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pcase sboo-profile-name

  ((nil | "default" "sboo"
    (load "~/.emacs.d/sboo/sboo-default-init.el"))

  ("minimal"
    (load "~/.emacs.d/sboo/sboo-minimal-init.el"))

  (_
    (load "~/.emacs.d/sboo/sboo-default-init.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;(setq sboo-profile-name
;;  (sboo/string->symbol
;;    (getenv sboo-profile-environment-variable)))

;;(pcase sboo-profile-name
;;  ('sboo    (load "~/.emacs.d/sboo/sboo-init.el"))
;;  ('minimal (load "~/.emacs.d/sboo/sboo-init-minimal.el")) 
;;  (_        (load "~/.emacs.d/sboo/sboo-init.el"))


(let ((*sboo-profile-string-raw* ))
  (pcase *sboo-profile-string-raw* 
    ("minimal" (setq sboo-profile-name 'minimal))))

