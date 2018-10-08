;;; init.el --- SBoo's Emacs Configuration -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error t)

(when (boundp 'user-init-file) (find-file user-init-file))

;; ^ for Debugging

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((EmacsDirectory (or user-emacs-directory
			   "~/.emacs.d/"))
       (SbooDirectory  (file-name-as-directory (concat EmacsDirectory
						       "sboo/")))
       (SbooFile       (file-truename (concat SbooDirectory
					      "sboo-init.el"))))   ;;TODO EnvironmentVars

  (load SbooFile nil nil t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-ask-also-kill-buffers nil)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-args-cabal-repl (list "--ghc-option=-ferror-spans"))
 '(haskell-process-path-ghci "cabal")
 '(haskell-process-type (quote cabal-new-repl))
 '(package-selected-packages (quote (helm)))
 '(safe-local-variable-values
   (quote
    ((dante-project-root . "~/haskell/skeletor/")
     (dante-project-root . "~/haskell/haskell-project-skeleton/")
     (dante-target . "lib:haskell-project")
     (dante-project-root . "~/haskell/spiros/")
     (dante-target . "lib:spiros")
     (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
