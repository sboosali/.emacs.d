;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require 'use-package)

(require 'sboo-helm)

(require 'helm-swoop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun symbol-at-point-or-helm-swoop-pattern ()
;;     "`thing-at-point' or `helm-swoop-pattern'."
;;         (let (($pre-input (thing-at-point 'symbol)))
;;           (if (eq (length $pre-input) 0)
;;               (or helm-swoop-pattern "")
;;             $pre-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable

(use-package helm-swoop

  :init
  (setq 

   helm-swoop-speed-or-color nil
   ;; ^
   ;; `nil`: you can slightly boost invoke-speed in exchange for text-color 

   ;; helm-swoop-pre-input-function #'symbol-at-point-or-helm-swoop-pattern
   ;; ;; ^ if there is no symbol at the cursor, use the last used words instead.
   ;; ;; `helm-swoop-pattern' holds the last used words.

   helm-swoop-use-fuzzy-match t)
   ;; ^
   ;; fuzzy matching means: TODO.

  :bind
  (("M-i" . helm-swoop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; e.g.
;;

;; See:
;;     - https://github.com/ShingoFukuyama/helm-swoop/blob/master/README.md
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-helm-swoop)