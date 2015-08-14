(provide 'commands-app)
(require 'my-compilation)
(require 's)

(defun commands-app ()
  ;; (when-host "odysseus" 'commands-at-home)
  (when-host "c02m71hdfd58" 'commands-at-work)
  )

(defvar project-directory "~/commands-spiros")
(defvar project-file (file)
   (concat project "/" file))

(defun commands-at-work ()
  (setq tabbar-buffer-groups 'commands/tabbar-buffer-groups)                               
  (key (kbd "M-u") 'compile)

  (find-file (project-file "notes"))
  (end-of-buffer)

  (find-file (project-file "commands-core.cabal"))
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

(defun commands/tabbar-buffer-groups ()
  "overrides tabbar-buffer-groups defined in my-tabbar.
 puts \".note\" files into User not Notes"
  (list
   (cond
    ((s-matches? "\.hs" (buffer-name))  ;;(my/ends-with (buffer-name) ".hs") ;; e.g. "Etc.hs<Commands>"
     "Haskell")
    ((eq major-mode 'dired-mode)
     "Dired")
    (t
     "User")
    )))

(when-app "Commands\\.app" 'commands-app)
