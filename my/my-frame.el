(provide 'my-frame)

; 'window-system is 'ns on OS X for me
(when window-system
 (set-frame-size (selected-frame) 122 35)
)

