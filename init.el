;;;;;;;;;;;;;;; Dependencies ;;;;;;;;;;;;;;;;;;;;;;
; ~/.templates/*.tpl

;; to debug, with bisect:
;;  (error "STOP")


;;;;;;;;;;;;;;; INIT ;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/my-init.el")


;;;;;;;;;;;;;;; SETTINGS ;;;;;;;;;;;;;;;;;;;;;;

(require 'etc)
(require 'my-functions)
(require 'my-settings)


;;;;;;;;;;;;;;; simple configuration

(require 'compile)
(set 'compilation-scroll-output t)

;(require 'back-button)
;(back-button-mode 1)

(require 'dired-details)
(dired-details-install)

(require 'transpar)

(require 'saveplace)
(setq-default save-place t) ;can't use setq because the variable is buffer-local.
(setq save-place-file "~/.emacs.d/saved-places") ;your saved places are written to this file

;(require 'smooth-scrolling)

(require 'help-fns+)


;;;;;;;;;;;;;;; complex configuration

(require 'my-frame)
(require 'my-tabbar)
(require 'my-haskell)
(require 'my-autosave)
;; (require 'my-speedbar) ;; disabled for the Emacs daemon
(require 'my-windmove)
(require 'my-tramp)
(require 'my-deft)
(require 'my-template)
(require 'my-python)
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
(require 'my-evil)
;; (require 'my-linum)
;; (require 'my-purpose) 
(require 'my-magit)


;;;;;;;;;;;;;;; Apps

(require 'notes-app)
(require 'work-app)
(require 'diary-app)
(require 'observations-app)
(require 'server-app)
(require 'terminal-app)
(require 'commands-app)


;;;;;;;;;;;;;;; last

(require 'my-shortcuts) ; overrides everything above
(require 'my-keymaps) 

(require 'my-macros)
(require 'my-customization)


;;;;;;;;;;;;;;; et cetera 
