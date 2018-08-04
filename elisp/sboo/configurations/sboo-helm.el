;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require 'use-package)

(setq helm-allow-mouse t)
;; [1/3]
(require 'helm)

(require 'helm-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings

(setq helm-allow-mouse t)
;; ^ the mouse is gratuitously disabled by default.
;; this enables, for example, clicking on a helm candidate to activate it,
;; rather than navigating it with several arrow and/or character keypresses.
;; TODO doesn't work.
;; [2/3]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings

;; (global-set-key (kbd "M-x")
;;                 #'helm-M-x)

;; (global-set-key (kbd "C-x r b")
;;                 #'helm-filtered-bookmarks)

;; (global-set-key (kbd "C-x C-f")
;;                 #'helm-find-files)

;;; ^ Helm provides generic functions for completions to replace tab-completion in Emacs, with no loss of functionality.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable

;;TODO
(helm-mode 1)

(setq helm-allow-mouse t)
;; [3/3]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package helm
;;   :bind
;;   :map helm-mode-map
;;   ("C-x C-f" .  helm-find-files))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES

;; "Helm is a framework for incremental narrowing and selecting."

;; https://emacs-helm.github.io/helm/

;; https://github.com/emacs-helm/helm/wiki

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;NOTES

;; e.g.
;;
;; (use-package helm
;;   :defer t
;;   :bind (:map helm-map
;;               ("<tab>" . helm-execute-persistent-action)
;;               ("C-i"   . helm-execute-persistent-action)
;;               ("C-z"   . helm-select-action)
;;               ("A-v"   . helm-previous-page))
;;   :config
;;   (helm-autoresize-mode 1))
;;   ;; from jwiegley's configuration.

;; ;;; "Extended Config" from http://tuhdo.github.io/helm-intro.html

;; (require 'helm)
;; (require 'helm-config)

;; ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.


;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

;; (setq 
;;        helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;       helm-ff-file-name-history-use-recentf t
;;       helm-echo-input-in-header-line t)

;; (setq helm-autoresize-max-height 0)
;; (setq helm-autoresize-min-height 20)
;; (helm-autoresize-mode 1)

;; (helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;