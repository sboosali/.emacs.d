;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require 'use-package)

(require 'sboo-helm)

(require 'helm-swoop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable

(use-package helm-swoop

  :init
  (setq 

   helm-swoop-speed-or-color nil
   ;; ^
   ;; `nil`: you can slightly boost invoke-speed in exchange for text-color 

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