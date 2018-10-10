;;; -*- lexical-binding: t -*-
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
 sboo-exclusions--emacs--file-names        '(".emacs.desktop" ".emacs.desktop.lock" "savehist.el" ".emacs-buffers" "places" "saved-places" "ido.last" "tramp" ".abbrev_defs" ".smex-items")
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
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; =======================
;; `projectile' Exclusions
;; =======================
;;
;; [1] `projectile-globally-ignored-files':
;;
;;     A list of files globally ignored by projectile.
;;
;; [2] `projectile-globally-ignored-directories':
;;
;;     A list of directories globally ignored by projectile.
;;
;; [3] `projectile-globally-ignored-file-suffixe':
;;
;;     A list of file suffixes globally ignored by projectile.
;;
;; [4] `projectile-globally-ignored-mode':
;;
;;     A list of regular expressions for major modes ignored by projectile.
;;     If a buffer is using a given major mode, projectile will ignore it for functions working with buffers.
;;

;; See:
;;
;; - 
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-projectile)