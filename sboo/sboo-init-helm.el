;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (require 'package)

  (setq package-load-list '((helm-core t) (helm t) (async t) (popup t)))

  (package-initialize)

  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (file-truename (expand-file-name "./submodules/helm/emacs-helm.sh")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (require 'helm-config)
  (helm-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [remap find-file]                'helm-find-files)
(define-key global-map [remap occur]                    'helm-occur)
(define-key global-map [remap list-buffers]             'helm-buffers-list)
(define-key global-map [remap dabbrev-expand]           'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)

(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;