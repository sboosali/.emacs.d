;;; Bootstrap configuration by loading `use-package'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (require 'package)

  (setq package-enable-at-startup nil)

  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (setq use-package-verbose t)

  (eval-when-compile (require 'use-package))
  (require 'diminish)
  (require 'bind-key)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-init-use-package-2)