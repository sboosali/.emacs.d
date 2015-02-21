(provide 'my-tabbar)
(require 'tabbar)


(tabbar-mode t)

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
  (list
   (cond
    ((let ((n (length (buffer-name))))
      (string-equal ".hs" (substring (buffer-name) (- n 3) n)))
     "Haskell"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ;; ((string-equal "*" (substring (buffer-name) 0 1))
    ;;  "Emacs"
    ;;  )
    (t
     "User"
     )
    )))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

(global-set-key [M-s-left] 'tabbar-backward)
(global-set-key [M-s-right] 'tabbar-forward)

