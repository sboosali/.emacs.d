(provide 'work-app)

(defun work-app ()
  (find-file "~/voice/commands-core/commands-core.cabal")
  (end-of-buffer)
  (split-window-vertically)

  (find-file "~/voice/commands-core/notes")
  (end-of-buffer)

  (shell)
  (insert "find sources")
  (comint-send-input)

  (find-file "~/voice/commands-core/sources/Commands/Etc.hs")

  (sleep-for 5)
  (other-window 1)
  (switch-to-buffer "*shell*")

  ;(compilation-minor-mode)
  ;(other-window 1)

  ;(push #'elscreen-store kill-emacs-hook)
  ;(elscreen-restore)
)

(if (string-match "Work\\.app" (getenv "EMACSPATH"))
    (work-app))

