;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (setq helm-mode-fuzzy-match                 t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-allow-mouse t)

  (setq helm-split-window-in-side-p           t) ; open helm buffer inside current window, not occupy whole other window
  (setq helm-move-to-line-cycle-in-source     t) ; move to end or beginning of source when reaching top or bottom of source
  (setq helm-scroll-amount                    8) ; scroll 8 lines other window using M-<next>/M-<prior>
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-echo-input-in-header-line        t)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (global-set-key   (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (require 'helm-config)
  (helm-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap list-buffers]             'helm-buffers-list)
(define-key global-map [remap find-file]                'helm-find-files)

(define-key global-map [remap occur]                    'helm-occur)
;(define-key global-map [remap dabbrev-expand]           'helm-dabbrev)

(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See:
;; - https://github.com/emacs-helm/helm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-init-helm)