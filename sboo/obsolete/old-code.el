









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

