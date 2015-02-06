(provide 'terminal-app)

(defun terminal-app ()
  (split-window-horizontally)
  (multi-term)
  (other-window 1)
  (multi-term)
  (other-window 1)
)

(if (string-match "Terminal\\.app" (getenv "EMACSPATH"))
    (terminal-app))

