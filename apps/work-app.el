(provide 'work-app)

(require 'my-speedbar)
(require 'my-compilation)
(require 'my-haskell)


(defun work-app ()
  (when-host "odysseus" 'work-at-home)
  (when-host "c02m71hdfd58" 'work-at-work)
  )

(defun work-file (filename)
  (concat work-directory "/" filename))

(defun work-setup ()
  (set-frame-size (selected-frame) 128 35)
  (set-frame-position (selected-frame) 10 0)
  ; does order matter with dynamic scope? if its rebound before files
  ; are opened?
  (setq tabbar-buffer-groups 'haskell/tabbar-buffer-groups)                               

  (find-file (work-file "commands-core.cabal"))
  (end-of-buffer)
  (split-window-vertically)

  (shell)
  (compilation-shell-minor-mode)
  (insert "find .. -maxdepth 1")  (comint-send-input)
  (insert "make run")  (comint-send-input)
  (end-of-buffer)
  (comint-previous-input 1)

  (find-file (work-file "notes"))
  (end-of-buffer)

  (find-file (work-file "sources/*/*.hs") t)
  (find-file (work-file "sources/*/*/*.hs") t)
  (find-file (work-file "sources/*/*/*/*.hs") t)

  (find-file (work-file "tests/*.hs") t)
  (find-file (work-file "executables/*.hs") t)

  (find-file (work-file "sources/Commands/Plugins/Example.hs") t)
  
  (key (kbd "M-u") 'compile)

  ;(compilation-minor-mode)
  ;(other-window 1)

  ;(push #'elscreen-store kill-emacs-hook)
  ;(elscreen-restore)

;  (tabbar-mode) ; tab on each window (not one per frame)
)

(defun work-at-home ()
  (defvar work-directory "~/voice/commands-core")
  (work-setup))

(defun work-at-work ()
  (defvar work-directory "~/commands-core")
  (work-setup))

(when-app "Work\\.app" 'work-app)
