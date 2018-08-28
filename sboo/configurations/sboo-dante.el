;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dante

  :hook ((haskell-mode . flycheck-mode)
         (haskell-mode . dante-mode))

  :commands dante-mode)

  ;; ^
  ;;
  ;; i.e.:
  ;;
  ;;     :init
  ;;     (add-hook 'haskell-mode-hook #'dante-mode)
  ;;     (add-hook 'haskell-mode-hook #'flycheck-mode)
  ;;

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