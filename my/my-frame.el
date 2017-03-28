(provide 'my-frame)


(add-hook 'after-make-frame-functions 'my-frame-initialization)

;; doesnt have any effect on second frame
(defun my-frame-initialization (frame)
  ;;;  look and feel
(set-background-color "gray")
;;   (set-face-background 'default "black")
;;   (set-face-background 'region "black")
;;   (set-cursor-color "red")
)


; emacs frames are called windows in other applications

;; ; 'window-system is 'ns on OS X for me
;; (when window-system
;;  (set-frame-size (selected-frame) 122 35)
;;  ;; (set-frame-size (selected-frame) 127 34)
;;  ;; (set-frame-size (selected-frame) 150 60)
;; )

