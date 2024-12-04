

;;

(use-package zenburn-theme
  :disabled t
  :preface
  (setq my/zenburn-colors-alist
        '((fg . "#DCDCCC") (bg . "#1C1C1C") (cyan . "#93E0E3")))
  :custom-face
  (region ((t (:background ,(alist-get my/zenburn-colors-alist 'cyan)))))
  :config
  (load-theme 'zenburn t))

;;

