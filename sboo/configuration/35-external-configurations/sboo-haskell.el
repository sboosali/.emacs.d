;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-haskell-mode)
;; ^
;; my baseline (and the standard) `haskell`-language configuration. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sboo-dante)
;; ^
;; my `dante` configuration.
;; `dante` is a lightweight, configurable Haskell IDE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'sboo-lsp)
;; ;; ^
;; ;; `haskell-ide-engine' uses `LSP'.
;; ;;
;; ;;

;; (use-package dante
;;   :commands dante-mode)
;; ;; ^
;; ;; load `dante.el', which registers `dante-target' (&al) as `safe-local-var'(s).
;; ;; autoload `dante-mode', so we can run 《 M-x dante-mode 》.
;; ;;
;; ;; but don't **configure** it, i.e. no `hook's;
;; ;; when this statement is uncommented, we're using `lsp-haskell'.
;; ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(require 'sboo-intero)
;; ^
;; my `intero` configuration.
;; `intero` is a `stack`-only Haskell IDE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(require 'sboo-ghcid)
;; ^
;; my custom `ghcid`-mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-haskell)