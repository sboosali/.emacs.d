(provide 'work-app)

(require 'my-speedbar)
(require 'my-compilation)


(defun work-app ()
  (set-frame-size (selected-frame) 126 35)
  (set-frame-position (selected-frame) 70 0)
  ; does order matter with dynamic scope? if its rebound before files
  ; are opened?
  (setq tabbar-buffer-groups 'work/tabbar-buffer-groups)                               

  (find-file "~/voice/commands-core/commands-core.cabal")
  (end-of-buffer)
  (split-window-vertically)

  (shell)
  (insert "make run")
  (compilation-shell-minor-mode)
  (comint-send-input)
  (end-of-buffer)
  (comint-previous-input 1)

  (find-file "~/voice/commands-core/sources/*/*.hs" t)
  (find-file "~/voice/commands-core/sources/*/*/*.hs" t)
  (find-file "~/voice/commands-core/sources/*/*/*/*.hs" t)

  (find-file "~/voice/commands-core/notes")
  (end-of-buffer)

  (switch-to-buffer "Command.hs")
  (key (kbd "M-u") 'compile)

  ;(compilation-minor-mode)
  ;(other-window 1)

  ;(push #'elscreen-store kill-emacs-hook)
  ;(elscreen-restore)
)

(if (string-match "Work\\.app" (getenv "EMACSPATH"))
    (work-app))

(defun work/tabbar-buffer-groups ()
  "overrides tabbar-buffer-groups defined in my-tabbar.
 puts \".note\" files into User not Notes"
  (list
   (cond
    ((my/ends-with (buffer-name) ".hs")
     "Haskell")
    ((eq major-mode 'dired-mode)
     "Dired")
    (t
     "User")
    )))
