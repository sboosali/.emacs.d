(provide 'my-init)
(require 'cl)
;(require 'cl-lib) ;?

;;;;;;;;;;;;;;; Dependencies ;;;;;;;;;;;;;;;;;;;;;;
; variable HOME 

;; to debug: 
;;  emacs --debug-init --no-init-file --load ~/.emacs.d/my-init.el 


;;;;;;;;;;;;;;; INIT ;;;;;;;;;;;;;;;;;;;;;;

; see my-emacs.note for examples

(setq HOME (expand-file-name "~/.emacs.d/"))


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

 replace+ 

 ghc
 company-ghc 

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


