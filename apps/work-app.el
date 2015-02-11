(provide 'work-app)

(require 'my-speedbar)

(defun work-app ()
  (set-frame-position (selected-frame) 70 0)

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

  (insert "cd sources")
  (comint-send-input)
  (speedbar)
  (set-frame-position (selected-frame) 0 0)
  (speedbar-expand-most)
  (insert "cd ..")
  (comint-send-input)

  (other-frame 1)

  ;(compilation-minor-mode)
  ;(other-window 1)

  ;(push #'elscreen-store kill-emacs-hook)
  ;(elscreen-restore)
)

(if (string-match "Work\\.app" (getenv "EMACSPATH"))
    (work-app))

