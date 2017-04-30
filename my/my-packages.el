(provide 'my-package) 

(require 'package)
(add-to-list
  'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;(package-install 'intero)


;; (use-package company
;;   :ensure t
;;   :defer t
;;   :idle (global-company-mode))

