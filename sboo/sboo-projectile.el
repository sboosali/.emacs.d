;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for the `projectile' package.
;;
;; See:
;;
;; * `sboo-excluded-directories'.
;; * `sboo-excluded-file-names'
;; * `sboo-excluded-file-extensions'.
;;
;; 
;;

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(require 'cl)
(require 'pcase)

;;

;;----------------------------------------------;;
;; Utilities -----------------------------------;;
;;----------------------------------------------;;

(setq sboo-excluded--global--directories      '(".sboo" "tmp" "ignore" ".git" ".dropbox-dist" ".nixnote"))
(setq sboo-excluded--global--file-extensions  '("~" "#" "log"))
(setq sboo-excluded--global--file-names       '(".bash_history_eternal"))

;;----------------------------------------------;;

(setq sboo-excluded--haskell--directories     '("dist" "dist-newstyle" "dist-dante" ".stack-work" ".cabal-sandbox"))
(setq sboo-excluded--haskell--file-extensions '("o" "hi" "chi" "chs.h"))
(setq sboo-excluded--haskell--file-names      '())

;;----------------------------------------------;;

(setq sboo-excluded--emacs--directories       '("db" "auto-save-list" "backups" "elpa" "eshell" "smex-items" "cask" ))
(setq sboo-excluded--emacs--file-extensions   '("elc" "window-layout"))
(setq sboo-excluded--emacs--file-names        '(".emacs.desktop" ".emacs.desktop.lock" "bookmarks" ".emacs.desktop..el" "bookmarks.el" "savehist.el" ".emacs-buffers" "places" "saved-places" "ido.last" "tramp" ".abbrev_defs" ".smex-items" ".yas-compiled-snippets.el"))

  ;;TODO `session.*` (prefix, not suffix)

;;----------------------------------------------;;

(setq sboo-excluded--nix--directories       '("result"))
(setq sboo-excluded--nix--file-extensions   '())
(setq sboo-excluded--nix--file-names        '())

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defvar sboo-excluded-directories

      (append
        sboo-excluded--global--directories      
        sboo-excluded--haskell--directories     
        sboo-excluded--emacs--directories       
        sboo-excluded--nix--directories)

      "Directories to exclude from `projectile' searches.")

;;----------------------------------------------;;

(defvar sboo-excluded-file-names

      (append
        sboo-excluded--global--file-names      
        sboo-excluded--haskell--file-names     
        sboo-excluded--emacs--file-names       
        sboo-excluded--nix--file-names)

      "File basenames to exclude from `projectile' searches.")

;;----------------------------------------------;;

(defvar sboo-excluded-file-extensions

      (append
        sboo-excluded--global--file-extensions      
        sboo-excluded--haskell--file-extensions     
        sboo-excluded--emacs--file-extensions       
        sboo-excluded--nix--file-extensions)

      "File extensions to exclude from `projectile' searches.")

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

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

;;----------------------------------------------;;
(provide 'sboo-projectile)