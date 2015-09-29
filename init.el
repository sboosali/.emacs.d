;;;;;;;;;;;;;;; Dependencies ;;;;;;;;;;;;;;;;;;;;;;
; ~/.templates/*.tpl

;; to debug, with bisect:
;;  (error "STOP")

;;;;;;;;;;;;;;; INIT ;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)
;(require 'cl-lib) ;?

(setq HOME (expand-file-name "~/.emacs.d/"))

; see my-emacs.note for examples


;;;;;;;;;;;;;;; Paths ;;;;;;;;;;;;;;;;;;;;;;

(defvar load-paths '(
 "."
 "my"                                   ; utilities or wrappers around libraries
 "apps"                                 ; initialization depending on <X>.app
 "secret"                               ; super secret data files

 "packages"                             ; package files that were copied and pasted

 "structured-haskell-mode/elisp"        ; Haskell
 "ghc-server/elisp"
 "hindent/elisp"

 "emacs_chrome/servers"                 ; Emacs in chromium
; "edit-server-htmlize"

) "load paths that don't obey the normal package-name/module-name.el format.")

(loop for location in load-paths
      do (add-to-list 'load-path (concat HOME location)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Packaging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(
 ("melpa" . "http://melpa.milkbox.net/packages/")
 ("gnu" . "http://elpa.gnu.org/packages/")
))

(setq package-enable-at-startup nil) ; ?
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)) ; Run this every week or so

; to upgrade:
; M-x list-packages
; "U" to mark as "should be upgraded"
; "x" to execute the marked actions

(defvar packages '(
 cl-lib
 org
 ;; smex
 undo-tree
 magit
 solarized-theme
 smart-tabs-mode
 ido-complete-space-or-hyphen
 tabbar
 smooth-scrolling
 centered-cursor-mode
 haskell-mode
 ;web-server
 htmlize
; elscreen
; async
 helm
 exec-path-from-shell
; edit-server
 dash
 s

; web-server
 projectile
 flx-ido

 evil
 help-fns+ 
 window-purpose
))

(defvar all-packages-installed t)
(dolist (p packages)
    (unless (package-installed-p p)
      (setq all-packages-installed nil)))

(unless all-packages-installed
  (package-refresh-contents)
  (dolist (p packages)
    (unless (package-installed-p p)
      (package-install p))))





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
