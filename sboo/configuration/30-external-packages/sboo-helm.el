;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-init-helm! ()

  "Initialize `helm' variables.

  Fast & Safe (only `setq's a few `defvar's).
  "
  (interactive)

  (progn

     (setq
      helm-mode-fuzzy-match                 t
      helm-completion-in-region-fuzzy-match t)

     ;; ^ fuzzy-matching.

     ;; helm-boring-buffer-regexp-list '()
     ;;
     ;; ;; ^
     ;; ;; by default, `helm-boring-buffer-regexp-list' is:
     ;; ;;     ("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf")
     ;; ;;

     (setq helm-allow-mouse t)

     ;; ^ the mouse is gratuitously disabled by default.
     ;; this enables, for example, clicking on a helm candidate to activate it,
     ;; rather than navigating it with several arrow and/or character keypresses.
     ;;

     ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sboo-config-helm! ()

  "Require `helm' and configure `helm-mode'.

  Idempotent (invoking it twice is the same as calling once).

  Safe (doesn't throw an error when the package can't be `load'ed).
  "
  (interactive)

  (when (require 'helm nil t)
  
      ;;TODO (and (require 'helm-config nil t) (require 'helm nil t))

    (setq helm-mode-fuzzy-match                 t)
    (setq helm-completion-in-region-fuzzy-match t)
    ;; ^ fuzzy-matching.

    (setq helm-allow-mouse t)
    ;; ^ the mouse is gratuitously disabled by default.
    ;; this enables, for example, clicking on a helm candidate to activate it,
    ;; rather than navigating it with several arrow and/or character keypresses.
    ;;

   ;; helm-boring-buffer-regexp-list '()
   ;; ;; ^
   ;; ;; by default, `helm-boring-buffer-regexp-list' is:
   ;; ;;     ("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf")
   ;; ;;

      ;;;(add-hook 'MODE-mode-hook '-mode)
  
      ;;;(defalias 'ALIAS '-mode)

  (helm-mode 1)))

;; ^ 
;; 
;; 
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings

;; (global-set-key (kbd "M-x")
;;                 #'helm-M-x)

;; (global-set-key (kbd "C-x r b")
;;                 #'helm-filtered-bookmarks)

;; (global-set-key (kbd "C-x C-f")
;;                 #'helm-find-files)

;;; ^ Helm provides generic functions for completions to replace tab-completion in Emacs, with no loss of functionality.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package helm
;;   :init
;;   (progn
;;     (setq helm-mode-fuzzy-match                 t)
;;     (setq helm-completion-in-region-fuzzy-match t)
;;     ;; ^ fuzzy-matching.
;;    ;; helm-boring-buffer-regexp-list '()
;;    ;; ;; ^
;;    ;; ;; by default, `helm-boring-buffer-regexp-list' is:
;;    ;; ;;     ("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf")
;;    ;; ;;
;;     (setq helm-allow-mouse t)
;;     ;; ^ the mouse is gratuitously disabled by default.
;;     ;; this enables, for example, clicking on a helm candidate to activate it,
;;     ;; rather than navigating it with several arrow and/or character keypresses.
;;     ;;
;;     nil)
;;   :bind (:map helm-command-map
;;               ("<f9>" . helm-quit-and-helm-mini))
;; ;   ("C-x C-f" .  helm-find-files)
;;   :config
;;   (helm-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "Helm is a framework for incremental narrowing and selecting."

;; https://emacs-helm.github.io/helm/

;; https://github.com/emacs-helm/helm/wiki

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sboo-helm)