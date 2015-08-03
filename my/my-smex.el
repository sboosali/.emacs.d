(provide 'my-smex)


; "Smex is a M-x enhancement for Emacs. Built on top of IDO, it provides a convenient interface to your recently and most frequently used commands."

;; (global-set-key [(meta x)] (lambda ()
;;  (interactive)
;;  (or (boundp 'smex-cache)
;;      (smex-initialize))
;;  (global-set-key [(meta x)] 'smex)
;;  (smex)))

;; (global-set-key [(shift meta x)] (lambda ()
;;  (interactive)
;;  (or (boundp 'smex-cache)
;;     (smex-initialize))
;;  (global-set-key [(shift meta x)] 'smex-major-mode-commands)
;;  (smex-major-mode-commands)))
