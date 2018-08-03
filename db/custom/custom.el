(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package)))
 '(safe-local-variable-values
   (quote
    ((dante-repl-command-line "nix-shell" "/home/sboo/haskell/cards/default.nix" "-A" "shells.ghc" "--run" "cabal new-repl cards-frontend")
     (dante-repl-command-line "nix-shell" "/home/sboo/haskell/magic-card-search/shell.nix" "--run" "cabal repl magic-card-search")))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
