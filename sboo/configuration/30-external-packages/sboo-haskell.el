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

(use-package haskell
  :demand t
  :commands    (haskell-mode)

  :hook        ((haskell-mode . interactive-haskell-mode))

  :interpreter (("runhaskell"  . haskell-mode)
                ("runghc"      . haskell-mode)
                ("stack"       . haskell-mode))
  :mode        (("\\.hs\\'"    . haskell-mode)
                ("\\.lhs\\'"   . haskell-mode)
                ("\\.hsig\\'"  . haskell-mode)
                ("\\.hsc\\'"   . haskell-mode))

  :config
  
  (progn
    (custom-set-variables

     '(haskell-process-type             (quote cabal-new-repl))
     '(haskell-process-path-ghci        "cabal")
     ;; '(haskell-process-type             (quote stack-ghci))
     ;; '(haskell-process-path-ghci        "stack")

     '(haskell-process-args-cabal-repl  (list "--ghc-option=-ferror-spans"))
     '(haskell-ask-also-kill-buffers    nil)
     '(haskell-interactive-popup-errors nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-decl-scan
  :after    haskell
  :commands (haskell-decl-scan-mode)
  
  :hook     ((haskell-mode . haskell-decl-scan-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-cabal
  :after    haskell-mode
  :commands (haskell-cabal-mode)

  :mode        (("\\.cabal\\'"   . haskell-cabal-mode)
                ("\\.project\\'" . haskell-cabal-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-haskell)