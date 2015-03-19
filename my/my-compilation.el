(provide 'my-compilation)

(setq compilation-auto-jump-to-first-error t)
(setq next-error-highlight t)
(setq next-error-follow-minor-mode t)
(setq compile-command "cd ~/voice/commands-core/; make")
(setq compilation-read-command nil)

;;; compilation-minor-mode
(add-hook 'compilation-mode-hook (lambda () (progn
 ;(define-key compilation-mode-map (kbd "<mouse-1>") 'compile-goto-error)
 ;(define-key compilation-mode-map (kbd "RET") 'comint-send-input)
)))

