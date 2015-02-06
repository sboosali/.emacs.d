(provide 'server-app)

(defun server-app ()
 (server-start)
; undo with:
;  (server-force-delete)
)

(if (string-match "Emacs\\.app" (getenv "EMACSPATH"))
    (server-app))
