(provide 'my-frame)


; emacs frames are called windows in other applications

; 'window-system is 'ns on OS X for me
(when window-system
 (set-frame-size (selected-frame) 122 35)
 ;; (set-frame-size (selected-frame) 127 34)
 ;; (set-frame-size (selected-frame) 150 60)
)

