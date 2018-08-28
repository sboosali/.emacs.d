;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun haskell-doc-current-info ()
  (progn))
 ;; ^ HACK fixes this pseudo-error:
 ;; 
 ;;     eldoc error: (void-function haskell-doc-current-info) [2 times]
 ;; 

(defun haskell-mode-after-save-handler ()
  (progn))
 ;; ^ HACK fixes this pseudo-error:
 ;;
 ;;     Error running timer ‘real-auto-save-buffers’: (void-function haskell-mode-after-save-handler)
 ;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-mode
  :demand t

  :interpreter ("runhaskell" "runghc" "stack")
  :mode        ("\\.hs\\'" "\\.lhs\\'" "\\.hsig\\'" "\\.hsc\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-decl-scan
  :after    haskell-mode
  :commands (haskell-decl-scan)
  
  :hook     haskell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-interactive-mode
  :after haskell-mode
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-process
  :after haskell-mode

  :config
  (setq
   haskell-process-type 'cabal-new-repl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-cabal
  :after    haskell-mode
  :commands (haskell-cabal-mode)

  :mode        ("\\.cabal\\'" "\\.project\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-haskell)