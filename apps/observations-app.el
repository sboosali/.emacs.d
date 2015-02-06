(provide 'observations-app)

(defun obs-app ()
;  (find-file "~/Dropbox/.obs")
  (find-file "~/.emacs.d/init.el")
  (find-file "~/config/.profile")
  (find-file "~/Haskell")

  (shell)
  (split-window-vertically)
  (find-file "~/things")
  (end-of-buffer)

  ;(edit-server-start)
)

(if (string-match "Obs\\.app" (getenv "EMACSPATH"))
    (obs-app))

