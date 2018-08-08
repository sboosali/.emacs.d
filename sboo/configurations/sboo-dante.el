;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun haskell-doc-current-info () (progn))
 ;; ^ HACK fixes this pseudo-error:
 ;; 
 ;;     eldoc error: (void-function haskell-doc-current-info) [2 times]
 ;; 

(defun haskell-mode-after-save-handler () (progn))
 ;; ^ HACK fixes this pseudo-error:
 ;;
 ;;     Error running timer ‘real-auto-save-buffers’: (void-function haskell-mode-after-save-handler)
 ;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dante
  :commands 'dante-mode
  :after    haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-dante)