;(package-initialize)
(require 'cl)

(setq HOME (expand-file-name "~/.emacs.d/"))

;;;;;;;;;;;;;;; Paths ;;;;;;;;;;;;;;;;;;;;;;

(defvar my-load-paths '(
 "my"                                   ; utilities or wrappers around libraries
 "secret"                               ; super secret data files
 "packages"                             ; package files that were copied and pasted



;; Warning (initialization): Your ‘load-path’ seems to contain
;; your ‘.emacs.d’ directory: /home/sboo/.emacs.d/.
;; This is likely to cause problems...
;; Consider using a subdirectory instead, e.g.: /home/sboo/.emacs.d/lisp
;; "."

) "load paths that don't obey the normal package-name/module-name.el format.")

;;
(loop for location in my-load-paths
      do (add-to-list 'load-path (concat HOME location)))

;; git clone, package-name/module-name.el
(let ((default-directory  "~/.emacs.d/vendor/"))
  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;

(require 'my-functions)
(require 'my-settings)
(require 'my-server)

;;;;;;;;;;;;;;; Per-package configuration

(require 'my-frame)
(require 'my-widget)

(require 'my-tabbar)
(require 'my-haskell)
(require 'my-autosave)
;; (require 'my-speedbar) ;; disabled for the Emacs daemon
(require 'my-windmove)
(require 'my-tramp)
;(require 'my-deft); crashed after NixOS 17.03 upgrade
;(require 'my-template)
;(require 'my-python)
(require 'my-notes)
;(require 'my-abbrev)
;; (require 'my-smex)
(require 'my-paredit)
(require 'my-ido)
(require 'my-org)
;(require 'my-persist)  ; disabled because all apps share the same state
;(require 'my-shm) ; structured haskell mode doesn't work
;(require 'my-helm) ; helm won't install
;(require 'my-commands)
(require 'my-register)
(require 'my-projectile)
;(require 'my-evil)
;; (require 'my-linum)
;; (require 'my-purpose) 
(require 'my-magit)
(require 'my-desktop)
(require 'my-nix)
(require 'my-erc)
(require 'my-company)
;(require 'my-svg)

;;;;;;;;;;;;;;; final configuration

(require 'my-shortcuts) ; overrides everything above
(require 'my-keymaps) 

(require 'my-macros)
(require 'my-customization)

;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(org-startup-folded nil)
 '(package-selected-packages
   (quote
    (dante intero idris-mode window-purpose tabbar solarized-theme smooth-scrolling smart-tabs-mode s replace+ projectile magit ido-complete-space-or-hyphen htmlize help-fns+ helm flx-ido exec-path-from-shell evil company-ghc centered-cursor-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
