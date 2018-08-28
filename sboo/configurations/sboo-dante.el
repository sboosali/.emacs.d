;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dante
  :demand t
  ;; :after    haskell-mode

  :commands dante-mode

  :hook ((haskell-mode-hook . dante-mode)
         (haskell-mode-hook . flycheck-mode)))

  ;; :init
  ;; (add-hook 'haskell-mode-hook #'dante-mode)
  ;; (add-hook 'haskell-mode-hook #'flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 
;;
;; customize these variables:
;;
;; - `dante-project-root', and/or
;; - `dante-repl-command-line'.
;;
;; at these scopes:
;;
;; - file-locally, or
;; - directory-locally.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-dante)