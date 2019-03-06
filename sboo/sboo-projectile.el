;;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 sboo-exclusions--global--directories      '(".sboo" "tmp" "ignore" ".git" ".dropbox-dist" ".nixnote")
 sboo-exclusions--global--file-names       '(".bash_history_eternal")
 sboo-exclusions--global--file-extensions  '("~" "#" "log"))

(setq
 sboo-exclusions--haskell--directories     '("dist" "dist-newstyle" "dist-dante" ".stack-work" ".cabal-sandbox")
 sboo-exclusions--haskell--file-names      '()
 sboo-exclusions--haskell--file-extensions '("o" "hi" "chi" "chs.h"))

(setq
 sboo-exclusions--emacs--directories       '("db" "auto-save-list" "backups" "elpa" "eshell" "smex-items" "cask" )
 sboo-exclusions--emacs--file-names        '(".emacs.desktop" ".emacs.desktop.lock" "bookmarks.el" "savehist.el" ".emacs-buffers" "places" "saved-places" "ido.last" "tramp" ".abbrev_defs" ".smex-items" ".yas-compiled-snippets.el")
 sboo-exclusions--emacs--file-extensions   '("elc" "window-layout"))

  ;;TODO `session.*` (prefix, not suffix)

(setq
 sboo-exclusions--nix--directories       '("result")
 sboo-exclusions--nix--file-names        '()
 sboo-exclusions--nix--file-extensions   '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-exclusions-directories

      (append
        sboo-exclusions--global--directories      
        sboo-exclusions--haskell--directories     
        sboo-exclusions--emacs--directories       
        sboo-exclusions--nix--directories)

      "Directories to exclude from `projectile' searches.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-exclusions-file-names

      (append
        sboo-exclusions--global--file-names      
        sboo-exclusions--haskell--file-names     
        sboo-exclusions--emacs--file-names       
        sboo-exclusions--nix--file-names)

      "File basenames to exclude from `projectile' searches.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sboo-exclusions-file-extensions

      (append
        sboo-exclusions--global--file-extensions      
        sboo-exclusions--haskell--file-extensions     
        sboo-exclusions--emacs--file-extensions       
        sboo-exclusions--nix--file-extensions)

      "File extensions to exclude from `projectile' searches.")

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