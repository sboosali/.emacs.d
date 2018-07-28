;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-haskell)
;; ^ my `haskell-mode` configuration. 

(require 'sboo-dante)
;; ^ my `dante` configuration. 

;(require 'sboo-ghcid)
;; ^ my custom `ghcid`-mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-projectile)
;; ^ my `projectile` configuration; project directories.

(require 'sboo-compilation)
;; ^ my `compilation-mode` configuration; i.e. running `make`.

(require 'sboo-magit)
;; ^ my `magit` configuration; i.e. interface to `git` version control.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;TODO helm [1] ignores mouse clicks and [2] doesn't respect CUA-mode.
(require 'sboo-helm)
;; ^ my `helm` configuration. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-config)