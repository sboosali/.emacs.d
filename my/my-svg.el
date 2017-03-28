(provide 'my-svg)


; http://shallowsky.com/blog/linux/editors/graphics-in-emacs.html

(defun my-iimage ()
 (turn-on-iimage-mode)
 (iimage-mode-buffer t)
 ; (run-with-idle-timer 5 t 'refresh-iimages) ; stupid message overrides hover info
)

(defun refresh-iimages ()
  "Only way I've found to refresh iimages (without also recentering)"
  (interactive)
  (clear-image-cache nil)
  (iimage-mode nil)
  (iimage-mode t)
  (message "Refreshed images")
 )

