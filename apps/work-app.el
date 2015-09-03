(provide 'work-app)

(defun work-imports ()
 (require 'my-speedbar)
 (require 'my-compilation)
 (require 'my-haskell)
)

(defun work-app ()
  (work-imports)                         ; must call this first
  (when-host "odysseus" 'work-at-home)
  (when-host "c02m71hdfd58" 'work-at-work)
  )

(defun work-at-home ()
  (set-frame-size (selected-frame) 128 35)
  (set-frame-position (selected-frame) 10 0)
  ; does order matter with dynamic scope? if its rebound before files
  ; are opened?
  (setq tabbar-buffer-groups 'haskell/tabbar-buffer-groups)                               

  (find-file "~/voice/commands-core/commands-core.cabal")
  (end-of-buffer)
  (split-window-vertically)

  (shell)
  (compilation-shell-minor-mode)
  (insert "find .. -maxdepth 1")  (comint-send-input)
  (insert "make run")  (comint-send-input)
  (end-of-buffer)
  (comint-previous-input 1)

  (find-file "~/voice/commands-core/notes")
  (end-of-buffer)

  (find-file "~/voice/commands-core/sources/*/*.hs" t)
  (find-file "~/voice/commands-core/sources/*/*/*.hs" t)
  (find-file "~/voice/commands-core/sources/*/*/*/*.hs" t)

  (find-file "~/voice/commands-core/tests/*.hs" t)
  (find-file "~/voice/commands-core/executables/*.hs" t)

  (find-file "~/voice/commands-core/sources/Commands/Plugins/Example.hs" t)
  
  (key (kbd "M-u") 'compile)

  ;(compilation-minor-mode)
  ;(other-window 1)

  ;(push #'elscreen-store kill-emacs-hook)
  ;(elscreen-restore)

;  (tabbar-mode) ; tab on each window (not one per frame)
)

(defun work-at-work ()
)

(when-app "Work\\.app" 'work-app)
