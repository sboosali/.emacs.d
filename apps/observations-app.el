(provide 'observations-app)
(require 'edit-server)
(require 'my-edit-server)


(defun obs-app ()
;  (find-file "~/Dropbox/.obs")
  (find-file "~/config/.profile")
  (find-file "~/Haskell")
  (find-file "~/.emacs.d/init.el")

  (edit-server-start)

  (require 'my-erc)

  (shell)
  (insert "find apps")
  (comint-send-input)
  (insert "find my")
  (comint-send-input)

  (split-window-vertically)
  (find-file "~/things")
  (end-of-buffer)

)

(if (string-match "Obs\\.app" (getenv "EMACSPATH"))
    (obs-app))

