(provide 'my-desktop)

(desktop-save-mode 1)
(setq desktop-auto-save-timeout 5) ;; in seconds 

;; To suppress this warning:
;;    warning: desktop file appears to be in use by PID 200.
;;    Using it may cause conflicts. Use it anyway? (y or n)
;; Find and delete this file: .emacs.desktop.lock, C-h v desktop-base-lock-name RET.

