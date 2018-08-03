;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECTILE: PROJECT-MANAGEMENT ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 sboo-exclusions--global--directories      '(".sboo" "ignore")
 sboo-exclusions--global--file-names       '()
 sboo-exclusions--global--file-extensions  '("~" "#" "log"))

(setq
 sboo-exclusions--haskell--directories     '("dist-newstyle" ".stack-work" "dist" ".cabal-sandbox")
 sboo-exclusions--haskell--file-names      '()
 sboo-exclusions--haskell--file-extensions '("o" "hi" "chi" "chs.h"))

(setq
 sboo-exclusions--emacs--directories       '("db" "auto-save-list" "backups" "elpa" "eshell" "smex-items")
 sboo-exclusions--emacs--file-names        '(".emacs.desktop" ".emacs.desktop.lock" ".emacs-buffers" "places" "saved-places" "ido.last" "tramp" ".abbrev_defs" ".smex-items")
 sboo-exclusions--emacs--file-extensions   '("elc"))
  ;;TODO `session.*` (prefix, not suffix)

(setq
 sboo-exclusions--nix--directories       '("result")
 sboo-exclusions--nix--file-names        '()
 sboo-exclusions--nix--file-extensions   '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sboo-exclusions-directories
      (append
        sboo-exclusions--global--directories      
        sboo-exclusions--haskell--directories     
        sboo-exclusions--emacs--directories       
        sboo-exclusions--nix--directories))

(setq sboo-exclusions-file-names
      (append
        sboo-exclusions--global--file-names      
        sboo-exclusions--haskell--file-names     
        sboo-exclusions--emacs--file-names       
        sboo-exclusions--nix--file-names))

(setq sboo-exclusions-file-extensions
      (append
        sboo-exclusions--global--file-extensions      
        sboo-exclusions--haskell--file-extensions     
        sboo-exclusions--emacs--file-extensions       
        sboo-exclusions--nix--file-extensions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile

  :init

  (progn
    )

  :config

  (setq
     projectile-globally-ignored-directories    (append sboo-exclusions-directories       projectile-globally-ignored-directories)
     projectile-globally-ignored-files          (append sboo-exclusions-file-names        projectile-globally-ignored-files)
     projectile-globally-ignored-file-suffixes  (append sboo-exclusions-file-extensions   projectile-globally-ignored-file-suffixes))

  :delight
  
  '(:eval (concat " " (projectile-project-name)))
  ;; ^
  ;; Remove the mode name for projectile-mode, but show the project name.

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; TODO
;; ;; ;; ./.dir-locals
;; ((nil .
;;   ((projectile-project-compilation-cmd . "make")
;;     (projectile-project-test-cmd . "make test")
;;     (projectile-project-run-cmd . "make run"))))
;; ;;
;; ;; NOTES: projectile dir-locals
;; ;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;; `projectile` exclusions
;; =======================
;;
;; [1] `projectile-globally-ignored-files`
;;
;;     A list of files globally ignored by projectile.
;;
;; [2] `projectile-globally-ignored-directories`
;;
;;     A list of directories globally ignored by projectile.
;;
;; [3] `projectile-globally-ignored-file-suffixes`
;;
;;     A list of file suffixes globally ignored by projectile.
;;
;; [4] `projectile-globally-ignored-modes`
;;
;;     A list of regular expressions for major modes ignored by projectile.
;;     If a buffer is using a given major mode, projectile will ignore it for functions working with buffers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-projectile)