;;;;;;;;;;;;;;; Dependencies ;;;;;;;;;;;;;;;;;;;;;;
; ~/.templates/*.tpl


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

 "packages"                             ; package files that were copied and pasted

 "back-button"                          ; etc.

 "structured-haskell-mode/elisp"        ; Haskell
 "ghc-server/elisp"
 "hindent/elisp"

 "emacs_chrome/servers"                 ; Emacs in chromium
 "edit-server-htmlize"

 "mu/mu4e"

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
  (package-refresh-contents)) ; run this every week or so

(defvar packages '(
 cl-lib
 starter-kit
 org
 smex
 undo-tree
 magit
 solarized-theme
 smart-tabs-mode
 ido-complete-space-or-hyphen
 tabbar
 smooth-scrolling
 centered-cursor-mode

; elscreen
; async
; helm

; edit-server
))

(dolist (p packages)
  (unless (package-installed-p p)
    (package-install p)))


;;;;;;;;;;;;;;; SETTINGS ;;;;;;;;;;;;;;;;;;;;;;

(require 'etc)
(require 'my-functions)
(require 'my-settings)


;;;;;;;;;;;;;;; simple configuration

(require 'compile)
(set 'compilation-scroll-output t)

(require 'back-button)
;(back-button-mode 1)

(require 'dired-details)
(dired-details-install)

(require 'transpar)

(require 'saveplace)
(setq-default save-place t) ;can't use setq because the variable is buffer-local.
(setq save-place-file "~/.emacs.d/saved-places") ;your saved places are written to this file

(require 'tabbar)
(tabbar-mode) ; tab on each window  not one per frame)

;(require 'smooth-scrolling)


;;;;;;;;;;;;;;; complex configuration

(require 'my-frame)
(require 'my-haskell)
(require 'my-autosave)
(require 'my-speedbar)
(require 'my-windmove)
(require 'my-tramp)
(require 'my-deft)
(require 'my-template)
(require 'my-python)
(require 'my-notes)
;(require 'my-abbrev)
(require 'my-smex)
(require 'my-paredit)
(require 'my-ido)
(require 'my-org)
;(require 'my-persist)  ; disabled because all apps share the same state
;(require 'my-shm) ; structured haskell mode doesn't work
;(require 'my-helm) ; helm won't install
(require 'my-tabbar)
;(require 'my-commands)


;;;;;;;;;;;;;;; Apps

(require 'notes-app)
(require 'work-app)
(require 'diary-app)
(require 'observations-app)
(require 'server-app)
(require 'terminal-app)


;;;;;;;;;;;;;;; last

(require 'my-shortcuts) ; overrides everything above


;;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;
; from
;; kmacro-start-macro
;; kmacro-name-last-macro
;; insert-kbd-macro

(fset 'munge-facebook-songs
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([down 5 M-left M-left left 11 18 98 121 67108896 1 134217848 down 11 11 down] 0 "%d")) arg)))

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
 '(org-startup-folded nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
