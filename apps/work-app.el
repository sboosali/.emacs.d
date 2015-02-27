(provide 'work-app)
(require 'my-speedbar)
(require 'my-compilation)


(defun work-app ()
  (set-frame-position (selected-frame) 70 0)

  (find-file "~/voice/commands-core/commands-core.cabal")
  (end-of-buffer)
  (split-window-vertically)

  (shell)
  (insert "make")
  (comint-send-input)
  (compilation-shell-minor-mode)

  (find-file "~/voice/commands-core/sources/*/*.hs" t)
  (find-file "~/voice/commands-core/sources/*/*/*.hs" t)
  (find-file "~/voice/commands-core/sources/*/*/*/*.hs" t)

  (find-file "~/voice/commands-core/notes")
  (end-of-buffer)

  (switch-to-buffer "Etc.hs")
  (key (kbd "M-u") 'compile)

  ;(compilation-minor-mode)
  ;(other-window 1)

  ;(push #'elscreen-store kill-emacs-hook)
  ;(elscreen-restore)
)

(if (string-match "Work\\.app" (getenv "EMACSPATH"))
    (work-app))

