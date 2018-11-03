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
    ((eval progn
           (dante-mode -1)
           (flycheck-mode -1))
     (dante-target . "lib:enumerate-function")
     (dante-project-root . "~/haskell/enumerate/")
     (dante-target . "lib:enumerate")
     (dante-project-root . "~/haskell/skeletor/")
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
