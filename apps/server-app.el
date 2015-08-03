(provide 'server-app)

(defun server-app ()
 (server-start)
; undo with:
;  (server-force-delete)
)

(when-app "\\Server.app" 'server-app)

