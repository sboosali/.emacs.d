(provide 'commands-app)
(require 'my-compilation)
(require 'my-haskell)

(require 's)

(defun commands-app ()
  ;; (when-host "odysseus" 'commands-at-home)
  (when-host "c02m71hdfd58" 'commands-at-work)
  )

(defvar project-directory "~/commands-spiros")
(defun project-file (filename)
   (concat project "/" filename))

(defun commands-at-work ()
  (setq tabbar-buffer-groups 'haskell/tabbar-buffer-groups)                               
  (key (kbd "M-u") 'compile)

  (find-file (project-file "notes"))
  (end-of-buffer)

  (find-file (project-file "commands-spiros.cabal"))
  (end-of-buffer)
  (split-window-vertically)

  (shell)
  (compilation-shell-minor-mode)
  (insert "make")  (comint-send-input)
  (insert "find .. -maxdepth 1")  (comint-send-input)
  (end-of-buffer)

  (shell "*commands-server*")
  (compilation-shell-minor-mode)
  (insert "cabal run")  (comint-send-input)

  )

(when-app "Commands\\.app" 'commands-app)
